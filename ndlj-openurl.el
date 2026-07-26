;;; ndlj-openurl.el --- OpenURL API  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2026 Taro Sato
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
;; This module provides an OpenURL API integration for `ndlj'.
;;
;;; Code:

(require 'compat)

(require 'cl-lib)
(require 'dom)
(require 'map)
(require 'seq)
(require 'url)
;; (require 'url-expand)
(require 'url-parse)
(require 'url-util)
;; (require 'xml)

;; (require 's)
(require 'ndlj-util)

(defcustom ndlj-openurl-max-items 400
  "Maximum items to get from the OpenURL API."
  :type '(integer :tag "Max item count")
  :group 'ndlj)

(defcustom ndlj-openurl-sleep '(0.10 0.15)
  "Sleep in seconds between HTTP calls."
  :type '(choice
          (integer :tag "Sleep in seconds")
          (cons (integer :tag "Sleep in seconds")
                (integer :tag "Jitter in seconds")))
  :group 'ndlj)

(defconst ndlj-openurl--field-processors
  '(("出版事項" . ndlj-openurl--process-publisher)
    ("出版事項（掲載誌）" . ndlj-openurl--process-publisher)
    ("出版年月日等" . ndlj-openurl--process-publication-date)
    ("出版年（W3CDTF）" . ndlj-openurl--process-publication-year)
    ("数量" . ndlj-openurl--process-quantity)
    ("著者・編者" . ndlj-openurl--process-creators)
    ("シリーズ著者・編者" . ndlj-openurl--process-creators)
    ("著者標目" . ndlj-openurl--process-creator-indices)
    ("件名標目" . ndlj-openurl--process-topic-term-indices)
    ("書誌ID（NDLBibID）" . ndlj-openurl--process-ndl-bib-id)))

;;; Bibilography Item

(defun ndlj-openurl-bib-item-get (url)
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
                (value (if-let* ((proc (map-elt ndlj-openurl--field-processors field)))
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
      (when ndlj-debug
        (message "No bib item extracted from %s" url)))
    bib-item))

(defun ndlj-openurl--process-creators (node)
  "Process NODE ('dd') as author/editor/contributer info alist."
  (let ((pattern
         (concat "\\`"
                 "\\(?1:.*?\\)"
                 (format "\\( +\\[?\\(?3:%s\\)\\]?\\)?" ndlj--regexp-roles)
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
                    (when role `(("区分" . ,role)))
                    (if-let* ((_ (string-match "\\(?1:[^ ]+\\)\\s-+\\(?2:[^ ]+\\)"
                                               name))
                              (surname (match-string 1 name))
                              (given-name (match-string 2 name)))
                        `(("氏" . ,surname)
                          ("名" . ,given-name))
                      (when name `(("氏名" . ,name))))))
                 names))
            (ndlj-message "Unparsable (creators): '%s'" s)
            (list `(("氏名" . ,s))))))
      (dom-by-tag node 'span)))))

(defmacro ndlj-openurl--when-text-match (pattern &rest body)
  "Evaluate BODY when PATTERN matches the current text node."
  (declare (indent 1))
  `(when-let* ((s (nth node-index nodes))
               (_ (and (stringp s) (string-match ,pattern s))))
     (setq node-index (1+ node-index)
           result (apply #'append (list result (progn ,@body))))))

(defmacro ndlj-openurl--when-inner-text-match (tag pattern &rest body)
  "Evaluate BODY when PATTERN matches the inner text of current TAG."
  (declare (indent 2))
  `(when-let* ((n (nth node-index nodes))
               (_ (and (consp n) (eq (car n) ,tag)))
               (s (dom-inner-text n))
               (_ (string-match ,pattern s)))
     (setq node-index (1+ node-index)
           result (apply #'append (list result (progn ,@body))))))

(defun ndlj-openurl--process-index (span)
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
        (or (ndlj-openurl--when-text-match "\\`\\(?2:[^ ：:]+?\\) *[：:] *\\'"
                                           (let ((role (match-string 2 s)))
                                             (append (when role `(("区分" . ,role))))))
            t)
        (or (ndlj-openurl--when-inner-text-match
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
            (ndlj-openurl--when-inner-text-match
             'a (concat "\\`\\(?3:" re-person-name "\\)\\'")
             ;; Rely on the comma for surname/given name.
             (let ((surname (match-string 4 s))
                   (given-name (match-string 6 s)))
               `(("氏" . ,surname) ("名" . ,given-name)))))
        (or (or (ndlj-openurl--when-text-match
                 (concat "\\` *\\(?1:" re-person-yomi "?\\)"
                         "\\(?:" re-yob-yod "\\)\\'")
                 (let ((surname-yomi (match-string 2 s))
                       (given-name-yomi (match-string 5 s)))
                   (append (if (and surname-yomi given-name-yomi)
                               `(("氏／ヨミ" . ,surname-yomi)
                                 ("名／ヨミ" . ,given-name-yomi))
                             `(("氏名／ヨミ" . ,surname-yomi))))))
                (ndlj-openurl--when-text-match
                 (concat "\\` *\\(?1:" re-person-yomi "\\)\\'")
                 (let ((surname-yomi (match-string 2 s))
                       (given-name-yomi (match-string 5 s)))
                   `(("氏／ヨミ" . ,surname-yomi)
                     ("名／ヨミ" . ,given-name-yomi)))))
            t)                ; this node may not exist if yomikata is missing
        (ndlj-openurl--when-text-match " ( ")
        (ndlj-openurl--when-inner-text-match
         'a (concat "\\(?1:" re-entity-id "\\)")
         `(("ID" . ,(match-string 1 s))))
        (ndlj-openurl--when-text-match " )")
        (cdr result)))
     ;; Topic Term
     (let ((re-topic-name "[^ ]+")
           (re-topic-yomi "\\(\\cK\\|[0-9a-zA-Z ]\\)+")
           (result '(nil)) (node-index 0))
       (and
        (ndlj-openurl--when-inner-text-match
         'a (concat "\\(?1:" re-topic-name "\\)")
         (let ((topic-name (match-string 1 s)))
           (append `(("件名" . ,topic-name)))))
        (or (ndlj-openurl--when-text-match
             (concat "\\` *\\(?1:" re-topic-yomi "\\)")
             (let ((topic-yomi (match-string 1 s)))
               (append `(("件名／ヨミ" . ,topic-yomi)))))
            (cdr result))
        (ndlj-openurl--when-text-match " ( ")
        (ndlj-openurl--when-inner-text-match
         'a (concat "\\(?1:" re-entity-id "\\)")
         `(("ID" . ,(match-string 1 s))))
        (ndlj-openurl--when-text-match " )")
        (cdr result)))
     ;; Topic Term without ID
     (let ((re-topic-name ".+")
           (result '(nil)) (node-index 0))
       (and
        (ndlj-openurl--when-inner-text-match
         'a (concat "\\(?1:" re-topic-name "\\)")
         (let ((topic-name (match-string 1 s)))
           (append `(("件名" . ,topic-name)))))
        (cdr result))))))

(defun ndlj-openurl--process-creator-indices (node)
  "Process NODE ('dd') as creator indices alist."
  (mapcar (lambda (span)
            (ndlj-openurl--process-index span))
          (dom-by-tag node 'span)))

(defun ndlj-openurl--process-topic-term-indices (node)
  "Process NODE ('dd') as topic term indices alist."
  (mapcar (lambda (span)
            (ndlj-openurl--process-index span))
          (dom-by-tag node 'span)))

(defun ndlj-openurl--process-publication-date (node)
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

(defun ndlj-openurl--process-publication-year (node)
  "Process NODE ('dd') as publication year."
  (let ((pattern "\\([0-9]+\\)")
        (s (dom-inner-text (dom-by-tag node 'span))))
    (if (string-match pattern s)
        (let ((year (match-string 1 s)))
          (string-to-number year))
      s)))

(defun ndlj-openurl--process-publisher (node)
  "Process NODE ('dd') as publisher info alist."
  (let ((pattern "^\\(\\([^ :]+\\) *: *\\)?\\([^ (]+\\)\\( *(\\([^)]+\\))\\)?"))
    (mapcar
     (lambda (n)
       (let ((s (dom-inner-text n)))
         (when (string-match pattern s)
           (let ((place (match-string 2 s))
                 (publisher (match-string 3 s))
                 (role (match-string 5 s)))
             (append (when place `(("所在地" . ,place)))
                     (when publisher `(("出版社" . ,publisher)))
                     (when role `(("その他" . ,role))))))))
     (dom-by-tag node 'span))))

(defun ndlj-openurl--process-quantity (node)
  "Process NODE ('dd') as quantity."
  (let ((pattern "^\\([0-9]+\\) *\\(.+\\)?$")
        (s (dom-inner-text (dom-by-tag node 'span))))
    (if (string-match pattern s)
        (let ((quantity (match-string 1 s))
              (unit (match-string 2 s)))
          `(("数量" . ,(string-to-number quantity))
            ("単位" . ,unit)))
      s)))

(defun ndlj-openurl--process-ndl-bib-id (node)
  "Process NODE ('dd') as quantity."
  (append (when-let* ((span (dom-by-tag node 'span)))
            `(("NDLBibID" . ,(dom-inner-text span))))
          (when-let* ((a (dom-by-tag node 'a)))
            `(("URL" . ,(dom-inner-text a))))))

;;; Search Query

(defun ndlj-openurl--extract-search-items (url)
  "Extract items from search query result at URL."
  (ndlj-message "Querying URL: %s" url)
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
                          (map-elt ndlj-item-types-abbrev s s)))
                      (dom-by-class node "search-result-item-type-tag")
                      " "))
                    (item-material-types
                     (mapconcat
                      (lambda (span)
                        (let ((s (dom-inner-text span)))
                          (map-elt ndlj-item-material-types-abbrev s s)))
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

(cl-defun ndlj-openurl-search-query (&key any title creator dpid)
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
         (url (ndlj-build-url
               "https://ndlsearch.ndl.go.jp/api/openurl" nil query-params))
         all-items)
    (while
        (pcase-let* ((`(,resolved-url . ,items) (ndlj-openurl--extract-search-items url))
                     (parts (split-string
                             (url-filename
                              (url-generic-parse-url resolved-url))
                             "\\?"))
                     (url-path (nth 0 parts))
                     (raw-query (nth 1 parts))
                     (query-params (when raw-query
                                     (url-parse-query-string raw-query))))
          (ndlj-message "Extracted %d item(s) from '%s'" (length items) resolved-url)
          (redisplay)
          (when items
            (setq all-items (append all-items items))
            (unless (<= ndlj-openurl-max-items (length all-items))
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
              (apply #'ndlj-sleep
                     (if (listp ndlj-openurl-sleep) ndlj-openurl-sleep (list ndlj-openurl-sleep)))
              t))))
    (take ndlj-openurl-max-items all-items)))

(provide 'ndlj-openurl)
;;; ndlj-openurl.el ends here
