;;; ndlj.el --- National Diet Library, Japan  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2026 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/ndlj.el
;; Version: 0.1.1
;; Keywords: convenience
;; Package-Requires: ((emacs "30.1") (compat "31.0.0.2") (s "1.13.1"))
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
;; A search utility for the National Diet Library, Japan (国立国会図書館).
;;
;; References:
;;
;;   - API specifications: https://ndlsearch.ndl.go.jp/help/api/specifications
;;
;;; Code:

(require 'compat)

;; (require 'cl-lib)
;; (require 'dom)
(require 'map)
;; (require 'seq)
;; (require 'url-expand)
(require 'url-parse)
;; (require 'url-util)
;; (require 'xml)

;; (require 's)

(eval-when-compile
  (require 'consult nil t))
(declare-function consult--read "consult")
(declare-function consult--lookup-cons "consult")

;;; Customization Group & Options

(defgroup ndlj nil
  "Customization group for `ndlj'."
  :group 'convenience
  :prefix "ndlj-")

(defcustom ndlj-debug t
  "Debug switch."
  :type 'boolean
  :group 'ndlj)

(defcustom ndlj-sleep '(0.10 0.15)
  "Sleep in seconds between HTTP calls."
  :type '(choice
          (integer :tag "Sleep in seconds")
          (cons (integer :tag "Sleep in seconds")
                (integer :tag "Jitter in seconds")))
  :group 'ndlj)

(defcustom ndlj-max-items 400
  "Maximum items to get from the NDL search API."
  :type '(integer :tag "Max item count")
  :group 'ndlj)

(defcustom ndlj-item-types-abbrev
  '(("紙" . "")
    ("記録メディア" . "")
    ("デジタル" . "󰓷")
    ("マイクロ" . "󰍉"))
  "Item types and their abbreviations."
  :type '(repeat (cons (string :tag "資料形態")
                       (string :tag "省略形")))
  :group 'ndlj)

(defcustom ndlj-item-material-types-abbrev
  '(("図書" . "図")
    ("電子書籍・電子雑誌" . "電")
    ("録音資料" . "録")
    ("雑誌" . "雑")
    ("雑誌タイトル" . "雑タ")
    ("記事" . "記")
    ("児童書" . "児")
    ("映像資料" . "映")
    ("博士論文" . "博"))
  "Item material types and their abbreviations."
  :type '(repeat (cons (string :tag "種類種別")
                       (string :tag "省略形")))
  :group 'ndlj)

(defcustom ndlj-dpid
  '("iss-ndl-opac" "zassaku")
  "Data providers for queries."
  :type '(repeat (string :tag "Data provider"))
  :group 'ndlj)

;;; Constants

(defconst ndlj-data-providers
  '("zassaku" "iss-ndl-opac")
  "All available data providers.")

(defconst ndlj--regexp-roles
  (regexp-opt '("著" "編" "訳" "監修" "漫画")))

(defun ndlj--create-completion (item-alist)
  "Create a completion string, along with prefix and suffix, from ITEM-ALIST."
  (cl-letf
      (((symbol-function 's-align)
        (lambda (text column &optional side)
          "Align TEXT to the COLUMN by SIDE."
          (apply #'concat
                 (let ((col (pcase side
                              ('right (- column (string-width text)))
                              (_ column))))
                   (list (propertize " " 'display `(space :align-to ,col))
                         text))))))
    (let-alist item-alist
      (let* ((width (frame-width)) ; used to be `window-body-width' of `minibuffer-window'
             (prefix
              (concat (s-align (or .categories "") 9 'right)))
             (completion
              (concat (s-align (or .title "") 11)
                      (s-align (or .author "") (round (* width 0.45)))
                      (s-align (or .publisher .book)
                               (round (* width 0.65)))
                      (s-align (or .publish-date
                                   (concat .book-publish-date
                                           ": "
                                           .page))
                               (round (* width 0.80)))))
             (suffix
              (concat (s-align (or .material-types "") width 'right))))
        (list completion prefix suffix)))))

(defun ndlj--completing-read (search-result-items)
  "Completing read SEARCH-RESULT-ITEMS."
  (when-let*
      ((candidates
        (mapcar
         (lambda (it)
           (pcase-let ((`(,cmpl ,pre ,suf) (ndlj--create-completion it)))
             (cons (propertize cmpl
                               'item-data it
                               'completion-prefix pre
                               'completion-suffix suf)
                   it)))
         search-result-items))
       (completion-extra-properties
        '(:affixation-function
          (lambda (completions)
            (mapcar
             (lambda (cmpl)
               (list cmpl
                     (get-text-property 0 'completion-prefix cmpl)
                     (get-text-property 0 'completion-suffix cmpl)))
             completions))))
       (chosen
        (cond
         ((featurep 'consult)
          (ndlj--completing-read-consult candidates))
         (t
          (map-elt (completing-read "Filter: " candidates) candidates)))))
    (get-text-property 0 'item-data chosen)))

(defun ndlj--completing-read-consult (candidates)
  "Completing read from CANDIDATES using `consult'."
  (car
   (let ((preview-buf (get-buffer-create " *ndlj-preview*"))
         (preview-win nil))
     (consult--read
      candidates
      :prompt "Filter (consult): "
      :lookup #'consult--lookup-cons
      :inherit-input-method t
      :preview-key 'any
      :state
      (lambda (action candidate)
        (pcase action
          ('preview
           (when candidate
             (let ((content ""))
               (when-let* ((item-data (cdr candidate)))
                 (let-alist item-data
                   (setq content (concat (or .highlight "")
                                         (or .children "")))))

               (with-current-buffer preview-buf
                 (let ((inhibit-read-only t))
                   (erase-buffer)
                   (when content
                     (insert content))
                   (special-mode)))

               (if (and (window-live-p preview-win)
                        (eq (window-buffer preview-win) preview-buf))
                   (when (string-empty-p content)
                     (delete-window preview-win)
                     (setq preview-win nil))
                 (unless (string-empty-p content)
                   (setq preview-win
                         (display-buffer preview-buf
                                         '((display-buffer-below-selected
                                            display-buffer-at-bottom)
                                           (window-height . 0.2)))))))))))))))

;;; Interactive Commands

(defun ndlj-query--command (&rest args)
  "Query interface for interactive command.
ARGS are passed to a search query function."
  (let ((dpid (if current-prefix-arg
                  (completing-read-multiple
                   "Choose data provider: " ndlj-data-providers
                   nil t (string-join ndlj-dpid ","))
                ndlj-dpid)))
    (require 'ndlj-openurl)
    (when-let*
        ((search-result-item
          (ndlj--completing-read
           (apply #'ndlj-openurl-search-query `(:dpid ,dpid ,@args))))
         (entity-path (map-elt search-result-item 'url))
         (url (url-recreate-url
               (url-parse-make-urlobj "https" nil nil "ndlsearch.ndl.go.jp" nil
                                      entity-path nil nil t))))
      (ndlj-openurl-bib-item-get url))))

;;;###autoload
(defun ndlj-search-any (query)
  "Perform 'any' search for QUERY.
Invoke this command with a prefix argument to switch data providers."
  (interactive "sNDL search (any): ")
  (ndlj-query--command :any query))

;;;###autoload
(defun ndlj-search-creator (query)
  "Perform 'creator' search for QUERY.
Invoke this command with a prefix argument to switch data providers."
  (interactive "sNDL search (creator): ")
  (ndlj-query--command :creator query))

;;;###autoload
(defun ndlj-search-title (query)
  "Perform 'title' search for QUERY.
Invoke this command with a prefix argument to switch data providers."
  (interactive "sNDL search (title): ")
  (ndlj-query--command :title query))

;;;###autoload
(defun ndlj-search-any-visit (query)
  "Visit the item web page from QUERY."
  (interactive "sNDL search (any): ")
  (when-let* ((url (map-elt (ndlj-query--command :any query) "ndl:url")))
    (browse-url url)))

(provide 'ndlj)
;;; ndlj.el ends here
