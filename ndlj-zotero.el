;;; ndlj-zotero.el --- Zotero integration for ndlj  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2026 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/ndlj.el
;; Version: 0.1.1
;; Package-Requires: ((emacs "30.1") (compat "31.0.0.2") (ndlj "0.1.1") (zotero "0.1.0"))
;; Keywords: convenience, hypermedia
;;
;;; License:
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This module provides a Zotero integration for `ndlj'.
;;
;; References:
;;
;;   - Zotero API schema: https://api.zotero.org/schema
;;
;;; Code:

(require 'compat)

(require 'cl-lib)
(require 'map)
(require 'seq)

(require 'zotero)

(defconst ndlj-zotero-roles
  '(("book" . (("著" . "author")
               ("編" . "editor")
               ("訳" . "translator")
               ("監修" . "contributor")
               ("漫画" . "author")
               ("写真" . "author")
               ("シリーズ編" . "seriesEditor")))))

(defun ndlj-zotero-article--create (item)
  "Create an article from ITEM."
  (let ((creator-indices (ndlj-zotero--creator-indices (map-elt item "著者標目"))))
    (append
     '( :itemType "magazineArticle" )
     (ndlj-zotero--json-title (map-elt item "タイトル")
                              (map-elt item "シリーズタイトル"))
     (ndlj-zotero--json-creators (map-elt item "著者・編者")
                                 (map-elt item "シリーズ著者・編者")
                                 creator-indices)
     `( :publicationTitle ,(map-elt item "タイトル（掲載誌）") )
     (ndlj-zotero--json-publisher (map-elt item "出版事項（掲載誌）"))
     (ndlj-zotero--json-date (map-elt item "掲載年月日（W3CDTF）"))
     `( :volume ,(map-elt item "掲載巻")
        :issue ,(map-elt item "掲載号")
        :pages ,(map-elt item "掲載ページ") )
     (ndlj-zotero--json-language (map-elt item "本文の言語コード"))
     `( :libraryCatalog "NDL Search"
        :callNumber ,(map-elt item "請求記号") )
     (ndlj-zotero--json-extra
      `(("NDLBibID" . ,(map-elt (map-elt item "書誌ID（NDLBibID）") "NDLBibID")))))))

(defun ndlj-zotero-item-magazine-article (rec)
  "Transform a record dom REC into a Zotero magazineArticle item."
  (append
   '( :itemType "magazineArticle"
      :title nil
      :creators nil
      :publicatonTitle nil
      :publisher nil
      :date nil
      :volume nil
      :issue nil
      :pages nil
      :language nil
      :libraryCatalog nil
      :callNumber nil
      :tags nil
      :extra nil )))

(defun ndlj-zotero-item-book (rec)
  "Transform a record dom REC into a Zotero book item."
  (let ((item-type "book"))
    (append
     `( :itemType ,item-type
        :title ,(map-elt rec 'title)
        :shortTitle ,(map-elt rec 'short-title)
        :volume ,(map-elt rec 'volume)
        :creators
        ,(cl-map 'vector
                 (lambda (it)
                   (append
                    `( :creatorType
                       ,(let ((role (map-elt it 'role)))
                          (if-let* ((ct (map-elt (map-elt ndlj-zotero-roles item-type) role)))
                              ct
                            (ndlj-message "Unknown role: '%s'" role)
                            role)) )
                    (when-let* ((fullname (map-elt it 'fullname)))
                      `( :name ,fullname ))
                    (when-let* ((surname (map-elt it 'surname)))
                      `( :lastName ,surname ))
                    (when-let* ((given-name (map-elt it 'given-name)))
                      `( :firstName ,given-name ))))
                 (map-elt rec 'creators))
        :series ,(map-elt rec 'series)
        :seriesNumber ,(map-elt rec 'series-number)
        :edition ,(map-elt rec 'edition)
        :publisher ,(map-elt rec 'publisher)
        :place ,(map-elt rec 'place)
        :date ,(map-elt rec 'date)
        :numPages ,(map-elt rec 'num-pages)
        :isbn ,(map-elt rec 'isbn)
        :language ,(let ((lang (map-elt rec 'language)))
                     (pcase lang
                       ("jpn" "ja")
                       ("eng" "en")
                       (_ lang)))
        :libraryCatalog "NDL Search"
        :callNumber ,(map-elt rec 'call-number)
        :extra ,(string-join
                 (seq-keep (lambda (it)
                             (when-let* ((k (car it)) (v (cdr it)))
                               (format "%s: %s" k v)))
                           (map-elt rec 'extra))
                 "\n")
        :tags ,(cl-map 'vector
                       (lambda (tag)
                         `( :tag ,tag ))
                       (flatten-list (seq-uniq (map-elt rec 'tags))))))))

;;; Interactive Commands

;;;###autoload
(defun ndlj-zotero-create-item ()
  "Create a Zotero item obtained from QUERY."
  (interactive)
  (when-let* ((item (call-interactively #'ndlj-search-any))
              (material-type (map-elt item 'material-type)))
    (let ((json (funcall (pcase material-type
                           ("記事" #'ndlj-zotero-item-magazine-article)
                           ("図書" #'ndlj-zotero-item-book)
                           (_ #'ndlj-zotero-item-book))
                         item)))
      (when ndlj-debug
        (pp json))
      (zotero-create-item json))))

(provide 'ndlj-zotero)
;;; ndlj-zotero.el ends here
