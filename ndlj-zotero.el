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

(require 'ndlj)
(require 'zotero)

(defconst ndlj-zotero-roles
  '(("book" . (("著" . "author")
               ("編" . "editor")
               ("訳" . "translator")
               ("監修" . "contributor")
               ("漫画" . "author")
               ("写真" . "author")
               ("シリーズ編" . "seriesEditor")))))

(defun ndlj-zotero-date-render (date)
  "Render DATE for JSON."
  (if (stringp date)
      date
    (format-time-string
     (cond ((and (decoded-time-year date)
                 (decoded-time-month date)
                 (decoded-time-day date))
            "%Y-%m-%d")
           ((and (decoded-time-year date)
                 (decoded-time-month date))
            "%Y-%m")
           ((decoded-time-year date)
            "%Y")
           (t "%Y-%m-%d"))
     (encode-time (decoded-time-set-defaults date)))))

(defun ndlj-zotero-language-render (lang)
  (pcase lang
    ("jpn" "ja")
    ("eng" "en")
    (_ lang)))

(defun ndlj-zotero-keep-non-nil (plis)
  (map-into (map-filter (lambda (_ v) v) plis) 'plist))

(defun ndlj-zotero-creator-render (item-type rec)
  (ndlj-zotero-keep-non-nil
   `( :creatorType
      ,(if-let* ((role (map-elt rec 'role)))
           (if-let* ((role (if-let* ((_ (map-elt rec 'series)))
                               (concat "シリーズ" role)
                             role))
                     (role (map-elt (map-elt ndlj-zotero-roles item-type) role)))
               role
             (ndlj-message "Unknown role: %s" role)
             role)
         (ndlj-message "Missing role for: %s" rec)
         "author")
      :name ,(map-elt rec 'fullname)
      :lastName ,(map-elt rec 'surname)
      :firstName ,(map-elt rec 'given-name) )))

(defun ndlj-zotero-extra-render (alis)
  (string-join (map-apply (lambda (k v) (format "%s: %s" k v))
                          (seq-filter #'cdr alis))
               "\n"))

(defun ndlj-zotero-item-magazine-article (rec)
  "Transform a record dom REC into a Zotero magazineArticle item."
  (let ((item-type "magazineArticle"))
    (ndlj-zotero-keep-non-nil
     `( :itemType ,item-type
        :title ,(map-elt rec 'title)
        :shortTitle ,(map-elt rec 'short-title)
        :creators ,(cl-map 'vector
                           (apply-partially #'ndlj-zotero-creator-render item-type)
                           (map-elt rec 'creators))
        :publicationTitle ,(map-elt rec 'publication-title)
        :publisher ,(map-elt rec 'publisher)
        :place ,(map-elt rec 'place)
        :date ,(ndlj-zotero-date-render (map-elt rec 'date))
        :volume ,(map-elt rec 'volume)
        :issue ,(map-elt rec 'issue)
        :pages ,(map-elt rec 'pages)
        :language ,(ndlj-zotero-language-render (map-elt rec 'language))
        :libraryCatalog "NDL Search"
        :callNumber ,(map-elt rec 'call-number)
        :extra ,(ndlj-zotero-extra-render (map-elt rec 'extra))
        :tags nil))))

(defun ndlj-zotero-item-book (rec)
  "Transform a record dom REC into a Zotero book item."
  (let ((item-type "book"))
    (ndlj-zotero-keep-non-nil
     `( :itemType ,item-type
        :title ,(map-elt rec 'title)
        :shortTitle ,(map-elt rec 'short-title)
        :volume ,(map-elt rec 'volume)
        ;; :creators ,(cl-map 'vector #'ndlj-zotero-creator-render (map-elt rec 'creators))
        :creators ,(cl-map 'vector
                           (apply-partially #'ndlj-zotero-creator-render item-type)
                           (map-elt rec 'creators))
        :series ,(map-elt rec 'series)
        :seriesNumber ,(map-elt rec 'series-number)
        :edition ,(map-elt rec 'edition)
        :publisher ,(map-elt rec 'publisher)
        :place ,(map-elt rec 'place)
        :date ,(ndlj-zotero-date-render (map-elt rec 'date))
        :numPages ,(map-elt rec 'num-pages)
        :isbn ,(map-elt rec 'isbn)
        :language ,(ndlj-zotero-language-render (map-elt rec 'language))
        :libraryCatalog "NDL Search"
        :callNumber ,(map-elt rec 'call-number)
        :extra ,(ndlj-zotero-extra-render (map-elt rec 'extra))
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
    (when ndlj-debug
      (pp item))
    (let* ((fun (cond
                 ((member material-type '("記事" "記事・論文"))
                  #'ndlj-zotero-item-magazine-article)
                 ((member material-type '("図書"))
                  #'ndlj-zotero-item-book)
                 (t #'identity)))
           (json (funcall fun item)))
      (when ndlj-debug
        (pp json))
      (let* ((resp (zotero-create-item json))
             (payload (zotero-response-data resp)))
        (cond ((map-nested-elt payload '(:success :0))
               (ndlj-message "Created Zotero item: '%s'"
                             (map-nested-elt payload '(:success :0))))
              ((map-nested-elt payload '(:failed :0))
               (ndlj-message "Creating Zotero item failed: %s"
                             (map-nested-elt payload '(:failed :0))))
              (t
               (ndlj-message "Error: %s" payload)))))))

(provide 'ndlj-zotero)
;;; ndlj-zotero.el ends here
