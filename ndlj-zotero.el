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

(require 'map)
(require 'seq)

(require 'ndlj)
(require 'zotero)

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

(defun ndlj-zotero-book--create (item)
  "Create a book from ITEM."
  (let ((creator-indices (ndlj-zotero--creator-indices (map-elt item "著者標目"))))
    (append
     `( :itemType "book" )
     (ndlj-zotero--json-title (map-elt item "タイトル"))
     (when-let* ((volume (map-elt item "巻次・部編番号")))
       `( :volume ,volume ))
     (ndlj-zotero--json-creators (map-elt item "著者・編者")
                                 (map-elt item "シリーズ著者・編者")
                                 creator-indices)
     (when-let* ((s (map-elt item "シリーズタイトル")))
       (let ((parts (when (string-match "[ \t]*[;][ \t]*" s)
                      (cons (substring s 0 (match-beginning 0))
                            (substring s (match-end 0))))))
         `( :series ,(or (car parts) s)
            :seriesNumber ,(cdr parts) )))
     `( :edition ,(map-elt item "版") )
     (ndlj-zotero--json-publisher (map-elt item "出版事項"))
     (ndlj-zotero--json-date (map-elt item "出版年月日等"))
     `( :numPages ,(let ((it (map-elt item "数量")))
                     (if (string= (map-elt it "単位") "p")
                         (map-elt it "数量")))
        :isbn ,(map-elt item "ISBN") )
     (ndlj-zotero--json-language (map-elt item "本文の言語コード"))
     `( :libraryCatalog "NDL Search"
        :callNumber ,(map-elt item "請求記号") )
     (ndlj-zotero--json-tags (append
                              (when-let* ((s (map-elt item "NDC9版")))
                                (list s))
                              (when-let* ((s (map-elt item "NDC10版")))
                                (list s)))
                             (map-elt item "件名標目"))
     (ndlj-zotero--json-extra
      `(("NDLBibID" . ,(map-elt (map-elt item "書誌ID（NDLBibID）") "NDLBibID")))))))

(defun ndlj-zotero--creator-indices (item)
  "Turn creator indices (著者標目) ITEM into an alist.
The alist maps full names to '(cons surname given-name)'. The alist is
typically used to infer surname and given name from a full name."
  (mapcar (lambda (it)
            (let ((surname (map-elt it "氏"))
                  (given-name (map-elt it "名")))
              `(,(concat surname given-name) . (,surname . ,given-name))))
          item))

(defun ndlj-zotero--json-tags (ndc-categories topic-term-indices)
  "Render NDC-CATEGORIES and TOPIC-TERM-INDICES as :tags."
  (list :tags
        (vconcat
         (mapcar
          (lambda (tag) `( :tag ,tag ))
          (seq-uniq
           (mapcar
            (lambda (tag)
              (ndlj-zotero--normalize-string tag))
            (flatten-list
             (list
              (mapcar
               (lambda (s)
                 (when-let*
                     ((_ (string-match "\\`.*: *\\(?1:.+\\)\\'" s))
                      (terms (string-split (match-string 1 s)
                                           "\\([．]\\|--\\)" t "\\s-+")))
                   terms))
               ndc-categories)
              (mapcar
               (lambda (it)
                 (cond
                  ((map-elt it "氏名")
                   (map-elt it "氏名"))
                  ((and (map-elt it "氏") (map-elt it "名"))
                   (concat (map-elt it "氏") " " (map-elt it "名")))
                  (t
                   (string-split (map-elt it "件名") "--" 'omit-empty "\\s-+"))))
               topic-term-indices)))))))))

(defun ndlj-zotero--json-publisher (publisher-items)
  "Render PUBLISHER-ITEMS as :publisher and :place."
  (let ((it (seq-find (lambda (it)
                        (let ((etc (map-elt it "その他")))
                          (or (null etc) (string= etc "出版"))))
                      publisher-items)))
    `( :publisher ,(map-elt it "出版社")
       :place ,(map-elt it "所在地") )))

(defun ndlj-zotero--normalize-string (str)
  "Normalize string STR."
  (pcase-dolist (`(,from . ,to) '(("　" . " ")
                                  ("!" . "！")
                                  ("?" . "？")
                                  ("(" . "（")
                                  (")" . "）")
                                  ("[" . "［")
                                  ("]" . "］")))
    (setq str (string-replace from-to from-to str)))
  str)

(defun ndlj-zotero--json-title (title &optional series-title)
  "Render TITLE and SERIES-TITLE as :title and :shortTitle."
  (let* ((title (ndlj-zotero--normalize-string title))
         (parts (string-split title "[:：]+" 'omit-empty "\\s-+"))
         (short-title (car parts))
         (title (string-join parts " ")))
    (append
     `( :title
        ,(concat title
                 (if-let* ((s (and series-title
                                   (ndlj-zotero--normalize-string series-title))))
                     (concat " （" s "）")
                   "")))
     (when (< (length short-title) (length title))
       `( :shortTitle ,short-title )))))

(defun ndlj-zotero--json-creators (creators series-creators &optional creator-indices)
  "Render CREATORS and SERIES-CREATORS as :creators.
When given, CREATOR-INDICES holds creator index (著者標目) entries."
  (list :creators
        (vconcat
         (mapcar
          (lambda (creator)
            (append
             (list :creatorType
                   (pcase (map-elt creator "区分")
                     ("著" "author")
                     ("漫画" "author")
                     ("編" (if (map-elt creator "シリーズ")
                               "seriesEditor"
                             "editor"))
                     ("訳" "translator")
                     ("監修" "contributor")
                     (_
                      (ndlj-message "Unknown role for '%s': '%s'"
                                    (or (map-elt creator "氏名")
                                        (concat (map-elt creator "氏")
                                                (map-elt creator "名")))
                                    (map-elt creator "区分"))
                      "contributor")))
             (if-let* ((fullname (map-elt creator "氏名")))
                 (if-let* ((index (map-elt creator-indices fullname))
                           (surname (car index))
                           (given-name (cdr index)))
                     `( :lastName ,surname :firstName ,given-name )
                   `( :name ,fullname ))
               `( :lastName ,(map-elt creator "氏")
                  :firstName ,(map-elt creator "名") ))))
          (append creators
                  (mapcar (lambda (it)
                            (cons '("シリーズ" .  t) it))
                          series-creators))))))

(defun ndlj-zotero--json-date (date)
  "Render DATE as :date.
If DATE is a list of form '(year month day)', it will be rendered as
'YYYY-MM-DD'."
  (list :date
        (if (listp date)
            (cond
             ((nth 2 date)
              (format "%04d-%02d-%02d" (nth 0 date) (nth 1 date) (nth 2 date)))
             ((nth 1 date)
              (format "%04d-%02d" (nth 0 date) (nth 1 date)))
             (t
              (format "%04d" (nth 0 date))))
          date)))

(defun ndlj-zotero--json-language (lang)
  "Render LANG as :language."
  `( :language ,(pcase lang
                  ("jpn" "ja")
                  ("eng" "en")
                  (_ lang))))

(defun ndlj-zotero--json-extra (alis)
  "Render key value pairs in ALIS as :extra."
  `( :extra ,(mapconcat (lambda (it)
                          (format "%s: %s" (car it) (cdr it)))
                        alis
                        "\n")))

;;; Interactive Commands

;;;###autoload
(defun ndlj-zotero-create-item ()
  "Create a Zotero item obtained from QUERY."
  (interactive)
  (when-let* ((item (call-interactively #'ndlj-search-any)))
    (let ((json (pcase (map-elt item "資料種別")
                  ("記事" (ndlj-zotero-article--create item))
                  ("図書" (ndlj-zotero-book--create item))
                  (_ (ndlj-zotero-book--create item)))))
      (when ndlj-debug
        (pp json))
      (zotero-create-item json))))

(provide 'ndlj-zotero)
;;; ndlj-zotero.el ends here
