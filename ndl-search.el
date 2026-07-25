;;; ndl-search.el --- ndl-search  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2026 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/ndl-search.el
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
(require 's)

(require 'cl-lib)
(require 'dom)
(require 'map)
(require 'url-expand)
(require 'url-parse)
(require 'url-util)
(require 'xml)

(defgroup ndl-search nil
  "Customization group for `ndl-search'."
  :group 'convenience
  :prefix "ndl-search-")

(defcustom ndl-search-debug t
  "Debug switch."
  :type 'boolean
  :group 'ndl-search)

(defcustom ndl-search-sleep '(0.10 0.15)
  "Sleep in seconds between HTTP calls."
  :type '(choice
          (integer :tag "Sleep in seconds")
          (cons (integer :tag "Sleep in seconds")
                (integer :tag "Jitter in seconds")))
  :group 'ndl-search)

(defcustom ndl-search-max-items 400
  "Maximum items to get from the NDL search API."
  :type '(integer :tag "Max item count")
  :group 'ndl-search)

(defcustom ndl-search-item-types-abbrev
  '(("紙" . "")
    ("記録メディア" . "")
    ("デジタル" . "󰓷")
    ("マイクロ" . "󰍉"))
  "Item types and their abbreviations."
  :type '(repeat (cons (string :tag "資料形態")
                       (string :tag "省略形")))
  :group 'ndl-search)

(defcustom ndl-search-item-material-types-abbrev
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
  :group 'ndl-search)

(defcustom ndl-search-dpid
  '("iss-ndl-opac" "zassaku")
  "Data providers for queries."
  :type '(repeat (string :tag "Data provider"))
  :group 'ndl-search)

(defconst ndl-search-data-providers
  '("zassaku" "iss-ndl-opac")
  "All available data providers.")

;; Bibilography Item

(defconst ndl-search--field-processors
  '(("出版事項" . ndl-search--process-publisher)
    ("出版事項（掲載誌）" . ndl-search--process-publisher)
    ("出版年月日等" . ndl-search--process-publication-date)
    ("出版年（W3CDTF）" . ndl-search--process-publication-year)
    ("数量" . ndl-search--process-quantity)
    ("著者・編者" . ndl-search--process-creators)
    ("シリーズ著者・編者" . ndl-search--process-creators)
    ("著者標目" . ndl-search--process-creator-indices)
    ("件名標目" . ndl-search--process-topic-term-indices)
    ("書誌ID（NDLBibID）" . ndl-search--process-ndl-bib-id)))

(defconst ndl-search--regexp-roles
  (regexp-opt '("著" "編" "訳" "監修" "漫画")))

(defun ndl-search-bib-item-get (url)
  "Get bib item as an alist from URL.
The bib item URL should have a path '/books/<id>'."
  (let ((url-automatic-caching t)
        bib-item)
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (search-forward "\n\n" nil t)
      (when-let* ((dom (libxml-parse-html-region (point) (point-max))))
        (mapcar
         (lambda (node)
           (when-let*
               ((field (dom-inner-text (dom-by-tag node 'dt)))
                (value (if-let* ((proc (map-elt ndl-search--field-processors field)))
                           (funcall proc (car (dom-by-tag node 'dd)))
                         (dom-inner-text (dom-by-tag node 'dd)))))
             (push (cons field value) bib-item)))
         (dom-by-class
          (car (dom-by-class dom "pages-books-section-bib-list"))
          "pages-books-ndls-section-bib-list-item"))))
    (if bib-item
        (progn
          (push (cons "ndl:url" url) bib-item)
          (pp bib-item))
      (when ndl-search-debug
        (message "No bib item extracted from %s" url)))
    bib-item))

(defun ndl-search--process-creators (node)
  "Process NODE ('dd') as author/editor/contributer info alist."
  (let ((pattern
         (concat "\\`"
                 "\\(?1:.*?\\)"
                 (format "\\( +\\[?\\(?3:%s\\)\\]?\\)?" ndl-search--regexp-roles)
                 "\\'")))
    (apply
     #'append
     (mapcar
      (lambda (span)
        (let ((s (dom-inner-text span)))
          (if (string-match pattern s)
              (let ((role (match-string 3 s))
                    (names (string-split (match-string 1 s) ", ")))
                (mapcar
                 (lambda (name)
                   (append
                    (when role (list (cons "区分" role)))
                    (if-let* ((_ (string-match "\\(?1:[^ ]+\\)\\s-+\\(?2:[^ ]+\\)"
                                               name))
                              (surname (match-string 1 name))
                              (given-name (match-string 2 name)))
                        (list (cons "氏" surname)
                              (cons "名" given-name))
                      (when name (list (cons "氏名" name))))))
                 names))
            (ndl-search--message "Unparsable (creators): '%s'" s)
            (list (list (cons "氏名" s))))))
      (dom-by-tag node 'span)))))

(defmacro ndl-search--when-text-match (pattern &rest body)
  "Evaluate BODY when PATTERN matches the current text node."
  (declare (indent 1))
  `(when-let* ((s (nth node-index nodes))
               (_ (and (stringp s) (string-match ,pattern s))))
     (setq node-index (1+ node-index)
           result (apply #'append (list result (progn ,@body))))))

(defmacro ndl-search--when-inner-text-match (tag pattern &rest body)
  "Evaluate BODY when PATTERN matches the inner text of current TAG."
  (declare (indent 2))
  `(when-let* ((n (nth node-index nodes))
               (_ (and (consp n) (eq (car n) ,tag)))
               (s (dom-inner-text n))
               (_ (string-match ,pattern s)))
     (setq node-index (1+ node-index)
           result (apply #'append (list result (progn ,@body))))))

(defun ndl-search--process-index (span)
  "Process indexed item under the SPAN node."
  (let* ((re-epoch "B\\. ?C\\.?\\|A\\. ?D\\.?")
         (re-year "[0-9]\\{1,4\\}")
         (re-person-name "\\([^ ,]+\\)\\(, +\\([^ ,]+?\\)\\)")
         (re-person-yomi "\\(\\(\\cK\\|[0-9a-zA-Z ]\\)+\\)\\(, +\\(\\(\\cK+\\|[0-9a-zA-Z ]\\)+\\)\\)")
         (re-entity-id "[0-9]+")
         (re-yob-yod (concat ", +"
                             "\\(" re-year "\\)" "\\(" re-epoch "\\)?" "-"
                             "\\(" re-year "\\|.+?\\)?" "\\(" re-epoch "\\)?"))
         (nodes (seq-keep (lambda (it)
                            (when (or (stringp it)
                                      (and (consp it) (eq (car it) 'a)))
                              it))
                          (dom-children span))))
    (or
     ;; Person
     (let ((result '(nil)) (node-index 0))
       (and
        (or (ndl-search--when-text-match "\\`\\(?2:[^ ：:]+?\\) *[：:] *\\'"
              (let ((role (match-string 2 s)))
                (append (when role (list (cons "区分" role))))))
            t)
        (or (ndl-search--when-inner-text-match
                'a (concat "\\`\\(?3:" re-person-name "?\\)"
                           "\\(?8:" re-yob-yod "\\)\\'")
              ;; Rely on the presence of year-of-birth/death.
              (let ((surname (match-string 4 s))
                    (given-name (match-string 6 s))
                    (yob (match-string 9 s))
                    (yob-epoch (match-string 10 s))
                    (yod (match-string 11 s))
                    (yod-epoch (match-string 12 s)))
                (append (if (and surname given-name)
                            `(("氏" . ,surname) ("名" . ,given-name))
                          `(("氏名" . ,surname)))
                        (when yob
                          `(("生年" . ,(if yob-epoch (concat yob " " yob-epoch) yob))))
                        (when yod
                          `(("没年" . ,(if yod-epoch (concat yod " " yod-epoch) yod)))))))
            (ndl-search--when-inner-text-match
                'a (concat "\\`\\(?3:" re-person-name "\\)\\'")
              ;; Rely on the comma for surname/given name.
              (let ((surname (match-string 4 s))
                    (given-name (match-string 6 s)))
                `(("氏" . ,surname) ("名" . ,given-name)))))
        (or (or (ndl-search--when-text-match
                    (concat "\\` *\\(?1:" re-person-yomi "?\\)"
                            "\\(?:" re-yob-yod "\\)\\'")
                  (let ((surname-yomi (match-string 2 s))
                        (given-name-yomi (match-string 5 s)))
                    (append (if (and surname-yomi given-name-yomi)
                                `(("氏／ヨミ" . ,surname-yomi)
                                  ("名／ヨミ" . ,given-name-yomi))
                              `(("氏名／ヨミ" . ,surname-yomi))))))
                (ndl-search--when-text-match
                    (concat "\\` *\\(?1:" re-person-yomi "\\)\\'")
                  (let ((surname-yomi (match-string 2 s))
                        (given-name-yomi (match-string 5 s)))
                    `(("氏／ヨミ" . ,surname-yomi)
                      ("名／ヨミ" . ,given-name-yomi)))))
            t)                ; this node may not exist if yomikata is missing
        (ndl-search--when-text-match " ( ")
        (ndl-search--when-inner-text-match
            'a (concat "\\(?1:" re-entity-id "\\)")
          `(("ID" . ,(match-string 1 s))))
        (ndl-search--when-text-match " )")
        (cdr result)))
     ;; Topic Term
     (let ((re-topic-name "[^ ]+")
           (re-topic-yomi "\\(\\cK\\|[0-9a-zA-Z ]\\)+")
           (result '(nil)) (node-index 0))
       (and
        (ndl-search--when-inner-text-match
            'a (concat "\\(?1:" re-topic-name "\\)")
          (let ((topic-name (match-string 1 s)))
            (append `(("件名" . ,topic-name)))))
        (or (ndl-search--when-text-match
                (concat "\\` *\\(?1:" re-topic-yomi "\\)")
              (let ((topic-yomi (match-string 1 s)))
                (append `(("件名／ヨミ" . ,topic-yomi)))))
            (cdr result))
        (ndl-search--when-text-match " ( ")
        (ndl-search--when-inner-text-match
            'a (concat "\\(?1:" re-entity-id "\\)")
          `(("ID" . ,(match-string 1 s))))
        (ndl-search--when-text-match " )")
        (cdr result)))
     ;; Topic Term without ID
     (let ((re-topic-name ".+")
           (result '(nil)) (node-index 0))
       (and
        (ndl-search--when-inner-text-match
            'a (concat "\\(?1:" re-topic-name "\\)")
          (let ((topic-name (match-string 1 s)))
            (append `(("件名" . ,topic-name)))))
        (cdr result))))))

(defun ndl-search--process-creator-indices (node)
  "Process NODE ('dd') as creator indices alist."
  (mapcar (lambda (span)
            (ndl-search--process-index span))
          (dom-by-tag node 'span)))

(defun ndl-search--process-topic-term-indices (node)
  "Process NODE ('dd') as topic term indices alist."
  (mapcar (lambda (span)
            (ndl-search--process-index span))
          (dom-by-tag node 'span)))

(defun ndl-search--process-publication-date (node)
  "Process NODE ('dd') as publication date."
  (let ((pattern "\\([0-9]+\\)\\(\\.\\([0-9]+\\)\\)?\\(\\.\\([0-9]+\\)\\)?")
        (s (dom-inner-text (dom-by-tag node 'span))))
    (if (string-match pattern s)
        (let ((year (match-string 1 s))
              (month (match-string 3 s))
              (day (match-string 5 s)))
          (list (when year (string-to-number year))
                (when month (string-to-number month))
                (when day (string-to-number day))))
      s)))

(defun ndl-search--process-publication-year (node)
  "Process NODE ('dd') as publication year."
  (let ((pattern "\\([0-9]+\\)")
        (s (dom-inner-text (dom-by-tag node 'span))))
    (if (string-match pattern s)
        (let ((year (match-string 1 s)))
          (string-to-number year))
      s)))

(defun ndl-search--process-publisher (node)
  "Process NODE ('dd') as publisher info alist."
  (let ((pattern "^\\(\\([^ :]+\\) *: *\\)?\\([^ (]+\\)\\( *(\\([^)]+\\))\\)?"))
    (mapcar
     (lambda (n)
       (let ((s (dom-inner-text n)))
         (when (string-match pattern s)
           (let ((place (match-string 2 s))
                 (publisher (match-string 3 s))
                 (role (match-string 5 s)))
             (append (when place (list (cons "所在地" place)))
                     (when publisher (list (cons "出版社" publisher)))
                     (when role (list (cons "その他" role))))))))
     (dom-by-tag node 'span))))

(defun ndl-search--process-quantity (node)
  "Process NODE ('dd') as quantity."
  (let ((pattern "^\\([0-9]+\\) *\\(.+\\)?$")
        (s (dom-inner-text (dom-by-tag node 'span))))
    (if (string-match pattern s)
        (let ((quantity (match-string 1 s))
              (unit (match-string 2 s)))
          (list (cons "数量" (string-to-number quantity))
                (cons "単位" unit)))
      s)))

(defun ndl-search--process-ndl-bib-id (node)
  "Process NODE ('dd') as quantity."
  (delq nil
        (list (when-let* ((span (dom-by-tag node 'span)))
                (cons "NDLBibID" (dom-inner-text span)))
              (when-let* ((a (dom-by-tag node 'a)))
                (cons "URL" (dom-inner-text a))))))

;; Search Query

(defun ndl-search--extract-search-items (url)
  "Extract items from search query result at URL."
  (message "Querying URL: %s" url)
  (cl-letf* (((symbol-function 'cname)
              (lambda (s)
                (concat "\\(?:^\\|[[:space:]]+\\)"
                        (regexp-quote s)
                        "\\(?:[[:space:]]+\\|$\\)")))
             ((symbol-function 'by-class)
              (lambda (dom class)
                (dom-by-class dom (cname class))))
             ((symbol-function 'inner-text)
              (lambda (node)
                (when node
                  (dom-inner-text node)))))
    (let* ((url-request-method "GET")
           (url-request-data nil)
           (url-request-extra-headers nil)
           (url-automatic-caching t)
           ;; TODO: Use `url-retrieve' for asynchronous callbacks:
           (response-buffer (url-retrieve-synchronously url)))
      (unless response-buffer
        (error "Response not received from %s" url))
      (with-current-buffer response-buffer
        (set-buffer-multibyte t)
        (decode-coding-region (point-min) (point-max) 'utf-8)
        (goto-char (point-min))
        (search-forward "\n\n" nil t)
        (let ((dom (libxml-parse-html-region (point) (point-max))))
          (cons
           (dom-attr (car (dom-by-id dom (cname "layouts-global-skip-link")))
                     'href)
           (mapcar
            (lambda (node)
              (let ((item-types
                     (mapconcat
                      (lambda (span)
                        (let ((s (dom-inner-text span)))
                          (map-elt ndl-search-item-types-abbrev s s)))
                      (dom-by-class node "search-result-item-type-tag")
                      " "))
                    (item-material-types
                     (mapconcat
                      (lambda (span)
                        (let ((s (dom-inner-text span)))
                          (map-elt ndl-search-item-material-types-abbrev s s)))
                      (dom-by-class node "search-result-item-material-type-tag")
                      "/"))
                    (meta (car (by-class node "search-result-item-meta"))))
                (list
                 (cons 'title
                       (inner-text (car (by-class node "search-result-item-heading"))))
                 (cons 'categories (concat item-types))
                 (cons 'material-types item-material-types)
                 (cons 'url
                       (dom-attr
                        (car (dom-by-tag
                              (car (by-class node "base-heading"))
                              'a))
                        'href))
                 (cons 'author
                       (inner-text (car (by-class meta "author"))))
                 (cons 'publisher
                       (inner-text (car (by-class meta "publisher"))))
                 (cons 'publish-date
                       (inner-text (car (by-class meta "publish-date"))))
                 (cons 'book
                       (inner-text (car (by-class meta "book"))))
                 (cons 'book-publish-date
                       (inner-text (car (by-class meta "book-publish-date"))))
                 (cons 'page
                       (inner-text (car (by-class meta "page"))))
                 (cons 'highlight
                       (mapconcat (lambda (ul)
                                    (inner-text ul))
                                  (by-class node "search-result-item-highlight")
                                  "\n"))
                 ;; TODO(2026-07-19): These nodes are filled dynamically
                 ;; via JS and not available; consider a headless
                 ;; browser like puppeteer or playwright to support this:
                 ;; (cons 'children
                 ;;       (inner-text
                 ;;        (car (by-class meta "search-result-item-children"))))
                 )))
            (by-class (car (by-class dom "search-result-body"))
                      "search-result-item"))))))))

(cl-defun ndl-search-query (&key any title creator dpid)
  "Make an OpenURL search query.
ANY maps to the query parameter 'any'.
TITLE maps to the query parameter 'btitle'.
CREATOR maps to the query parameter 'au'.
DPID maps to the query parameter 'ndl_dpid'."
  (let* ((query-params
          (append
           ;; NOTE(2026-07-21): The API documentation is wrong in that
           ;; ndl_dpid can be repeated; it interprets space-delimited
           ;; values, like other multi-word fields.
           (when dpid
             (list (cons 'ndl_dpid
                         (list (or (and (listp dpid) (string-join dpid " "))
                                   dpid)))))
           (when any (list (cons 'any (list any))))
           (when title (list (cons 'btitle (list title))))
           (when creator (list (cons 'au (list creator))))))
         (url (ndl-search-build-url
               "https://ndlsearch.ndl.go.jp/api/openurl" nil query-params))
         all-items)
    (while
        (pcase-let* ((`(,resolved-url . ,items) (ndl-search--extract-search-items url))
                     (parts (split-string
                             (url-filename
                              (url-generic-parse-url resolved-url))
                             "\\?"))
                     (url-path (nth 0 parts))
                     (raw-query (nth 1 parts))
                     (query-params (when raw-query
                                     (url-parse-query-string raw-query))))
          (message "Extracted %d item(s) from '%s'" (length items) resolved-url)
          (redisplay)
          (when items
            (setq all-items (append all-items items))
            (unless (<= ndl-search-max-items (length all-items))
              ;; Paginate
              (let* ((from (string-to-number
                            (car (map-elt query-params "from" (list "0")))))
                     (size (string-to-number
                            (car (map-elt query-params "size" (list "20"))))))
                (setf (map-elt query-params "from")
                      (list (number-to-string (+ from size))))
                (setf (map-elt query-params "size")
                      (list "100")))

              (setq url (url-recreate-url
                         (url-parse-make-urlobj
                          "https" nil nil "ndlsearch.ndl.go.jp" nil
                          (concat url-path
                                  "?" (url-build-query-string query-params))
                          nil nil t)))

              ;; Be nice to the API server.
              (apply #'ndl-search--sleep
                     (if (listp ndl-search-sleep) ndl-search-sleep (list ndl-search-sleep)))
              t))))
    (ndl-search--completing-read (take ndl-search-max-items all-items))))

(defun ndl-search--create-completion (item-alist)
  "Create completion string (also prefix and suffix) from ITEM-ALIST."
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

(defun ndl-search--completing-read (search-result-items)
  "Completing read SEARCH-RESULT-ITEMS."
  (when-let*
      ((candidates
        (mapcar (lambda (it)
                  (pcase-let ((`(,completion ,prefix ,suffix)
                               (ndl-search--create-completion it)))
                    (cons (propertize completion
                                      'item-data it
                                      'completion-prefix prefix
                                      'completion-suffix suffix)
                          it)))
                search-result-items))
       (completion-extra-properties
        '(:affixation-function
          (lambda (completions)
            (mapcar
             (lambda (completion)
               (list completion
                     (get-text-property 0 'completion-prefix completion)
                     (get-text-property 0 'completion-suffix completion)))
             completions))))
       (chosen
        (cond
         ((featurep 'consult)
          (ndl-search--completing-read-consult candidates))
         (t
          (map-elt (completing-read "Filter: " candidates) candidates)))))
    (get-text-property 0 'item-data chosen)))

(defun ndl-search--completing-read-consult (candidates)
  "Completing read from CANDIDATES using `consult'."
  (let* ((preview-buf (get-buffer-create " *ndl-search-preview*"))
         (preview-win nil))
    (car
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

;; Interactive Commands

(defun ndl-search-query--command (&rest _args)
  "Query interface for interactive command."
  (let ((dpid (if current-prefix-arg
                  (completing-read-multiple
                   "Choose data provider: " ndl-search-data-providers
                   nil t (string-join ndl-search-dpid ","))
                ndl-search-dpid)))
    (when-let*
        ((search-result-item (apply #'ndl-search-query `(:dpid ,dpid ,@_args)))
         (entity-path (map-elt search-result-item 'url))
         (url (url-recreate-url
               (url-parse-make-urlobj "https" nil nil "ndlsearch.ndl.go.jp" nil
                                      entity-path nil nil t))))
      (ndl-search-bib-item-get url))))

;;;###autoload
(defun ndl-search-any (query)
  "Perform 'any' search for QUERY.
Invoke this command with a prefix argument to switch data providers."
  (interactive "sNDL search (any): ")
  (ndl-search-query--command :any query))

;;;###autoload
(defun ndl-search-creator (query)
  "Perform 'creator' search for QUERY.
Invoke this command with a prefix argument to switch data providers."
  (interactive "sNDL search (creator): ")
  (ndl-search-query--command :creator query))

;;;###autoload
(defun ndl-search-title (query)
  "Perform 'title' search for QUERY.
Invoke this command with a prefix argument to switch data providers."
  (interactive "sNDL search (title): ")
  (ndl-search-query--command :title query))

;;;###autoload
(defun ndl-search-visit (query)
  "Visit the item web page from QUERY."
  (interactive "sNDL search (any): ")
  (when-let* ((url (map-elt (ndl-search-query--command :any query) "ndl:url")))
    (browse-url url)))

;; Utilities

(defun ndl-search-build-url (base-url path-segments query-alist)
  "Construct a safe, fully-encoded URL.
BASE-URL is the endpoint root (e.g., 'https://example.com/api/').
PATH-SEGMENTS is a list of unencoded string directories or endpoints.
QUERY-ALIST is an association list of keys and values for parameters."
  (let* ((clean-path (mapconcat #'url-hexify-string path-segments "/"))
         (full-url (url-expand-file-name clean-path base-url)))
    (if query-alist
        (concat full-url "?" (url-build-query-string query-alist))
      full-url)))

(defun ndl-search--sleep (seconds &optional jitter)
  "Sleep for SECONDS plus a JITTER.
When given, JITTER in seconds will be used to generate a random number
betwee 0 and JITTER."
  (sleep-for (+ seconds (if jitter
                            (/ (random (round (* jitter 1000.0))) 1000.0)
                          0))))

(defun ndl-search--message (s &rest _rest)
  "Display `ndl-search' message S."
  (apply #'message `(,(concat "[ndl-search] " s) ,@_rest)))

(defun ndl-search--warn (s &rest _rest)
  "Display `ndl-search' warning S."
  (apply #'warn `(,(concat "[ndl-search] " s) ,@_rest)))

(provide 'ndl-search)
;;; ndl-search.el ends here
