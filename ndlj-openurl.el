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
(require 'url-parse)

(require 'ndlj-api)
(require 'ndlj-util)

(defcustom ndlj-openurl-max-items 400
  "Maximum items to get from the OpenURL API."
  :type '(integer :tag "Max item count")
  :group 'ndlj)

(defcustom ndlj-openurl-sleep '(0.025 0.25)
  "Sleep in seconds between HTTP calls."
  :type '(choice
          (integer :tag "Sleep in seconds")
          (cons (integer :tag "Sleep in seconds")
                (integer :tag "Jitter in seconds")))
  :group 'ndlj)

(defconst ndlj-openurl--field-extractors
  '(("出版事項" . ndlj-openurl--extract-publisher)
    ("出版事項（掲載誌）" . ndlj-openurl--extract-publisher)
    ("出版年月日等" . ndlj-openurl--extract-publication-date)
    ("出版年（W3CDTF）" . ndlj-openurl--extract-publication-year)
    ("数量" . ndlj-openurl--extract-quantity)
    ("著者・編者" . ndlj-openurl--extract-creators)
    ("シリーズ著者・編者" . ndlj-openurl--extract-creators)
    ("著者標目" . ndlj-openurl--extract-creator-indices)
    ("件名標目" . ndlj-openurl--extract-topic-term-indices)
    ("書誌ID（NDLBibID）" . ndlj-openurl--extract-ndl-bib-id)
    ("NDC8版" . ndlj-openurl--extract-ndc)
    ("NDC9版" . ndlj-openurl--extract-ndc)
    ("NDC10版" . ndlj-openurl--extract-ndc)))

(defconst ndlj-openurl-hostname "ndlsearch.ndl.go.jp")
(defconst ndlj-openurl-api-path "/api/openurl")

;;; Bibilography Item

(defun ndlj-openurl--extract-creators (node)
  "Process NODE ('dd') as author/editor/contributer info alist."
  (apply #'append
         (mapcar (lambda (span)
                   (ndlj-api-parse-creator (dom-inner-text span)))
                 (dom-by-tag node 'span))))

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

(defun ndlj-openurl--extract-index (span)
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
                (append (when role `((role . ,role))))))
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
                            `((surname . ,surname) (given-name . ,given-name))
                          `((fullname . ,surname)))
                        (when yob
                          `((yob . ,(if yob-epoch (concat yob " " yob-epoch) yob))))
                        (when yod
                          `((yod . ,(if yod-epoch (concat yod " " yod-epoch) yod)))))))
            (ndlj-openurl--when-inner-text-match
                'a (concat "\\`\\(?3:" re-person-name "\\)\\'")
              ;; Rely on the comma for surname/given name.
              (let ((surname (match-string 4 s))
                    (given-name (match-string 6 s)))
                `((surname . ,surname) (given-name . ,given-name)))))
        (or (or (ndlj-openurl--when-text-match
                    (concat "\\` *\\(?1:" re-person-yomi "?\\)"
                            "\\(?:" re-yob-yod "\\)\\'")
                  (let ((surname-yomi (match-string 2 s))
                        (given-name-yomi (match-string 5 s)))
                    (append (if (and surname-yomi given-name-yomi)
                                `((surname-yomi . ,surname-yomi)
                                  (given-name-yomi . ,given-name-yomi))
                              `((fullname-yomi . ,surname-yomi))))))
                (ndlj-openurl--when-text-match
                    (concat "\\` *\\(?1:" re-person-yomi "\\)\\'")
                  (let ((surname-yomi (match-string 2 s))
                        (given-name-yomi (match-string 5 s)))
                    `((surname-yomi . ,surname-yomi)
                      (given-name-yomi . ,given-name-yomi)))))
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

(defun ndlj-openurl--extract-creator-indices (node)
  "Process NODE ('dd') as creator indices alist."
  (mapcar (lambda (span)
            (ndlj-openurl--extract-index span))
          (dom-by-tag node 'span)))

(defun ndlj-openurl--extract-topic-term-indices (node)
  "Process NODE ('dd') as topic term indices alist."
  (mapcar (lambda (span)
            (ndlj-openurl--extract-index span))
          (dom-by-tag node 'span)))

(defun ndlj-openurl--extract-publication-date (node)
  "Process NODE ('dd') as publication date."
  (ndlj-api-date-from-str (dom-inner-text (dom-by-tag node 'span))))

(defun ndlj-openurl--extract-publication-year (node)
  "Process NODE ('dd') as publication year."
  (ndlj-api-date-from-str (dom-inner-text (dom-by-tag node 'span))))

(defun ndlj-openurl--extract-publisher (node)
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

(defun ndlj-openurl--extract-quantity (node)
  "Process NODE ('dd') as quantity."
  (let ((pattern "^\\([0-9]+\\) *\\(.+\\)?$")
        (s (dom-inner-text (dom-by-tag node 'span))))
    (if (string-match pattern s)
        (let ((quantity (match-string 1 s))
              (unit (match-string 2 s)))
          `(("数量" . ,(string-to-number quantity))
            ("単位" . ,unit)))
      s)))

(defun ndlj-openurl--extract-ndl-bib-id (node)
  "Process NODE ('dd') as quantity."
  (append (when-let* ((span (dom-by-tag node 'span)))
            `(("NDLBibID" . ,(dom-inner-text span))))
          (when-let* ((a (dom-by-tag node 'a)))
            `(("URL" . ,(dom-inner-text a))))))

(defun ndlj-openurl--extract-ndc (node)
  "Process NODE ('dd') as an NDC info."
  (when-let* ((span (dom-by-tag node 'span))
              (str (dom-inner-text span)))
    (if (string-match "\\`\\([0-9/.]+\\)\\( *: *\\(.+\\)\\)?\\'" str)
        (let ((code (match-string 1 str))
              (text (match-string 3 str)))
          (progn
            (concat code
                    (if text
                        (let* ((text (replace-regexp-in-string "[．] *" ". " text))
                               (text (replace-regexp-in-string " *-- *" " -- " text)))
                          (concat " " text))
                      ""))
            ))
      (when ndlj-debug (ndlj-message "Unparsable NDC: '%s'" str))
      str)))

(defun ndlj-openurl-extract-fields (dom)
  (mapcar
   (lambda (node)
     (when-let*
         ((field (dom-inner-text (dom-by-tag node 'dt)))
          (value (if-let* ((exf (map-elt ndlj-openurl--field-extractors field)))
                     (funcall exf (car (dom-by-tag node 'dd)))
                   (dom-inner-text (dom-by-tag node 'dd)))))
       `(,field . ,value)))
   (dom-by-class (dom-by-class dom "pages-books-section-bib-list")
                 "pages-books-ndls-section-bib-list-item")))

(defun ndlj-openurl-book-extra (rec)
  "Get extra from REC for the book item."
  (append
   (when-let* ((ndl-bib-id (map-elt (map-elt rec "書誌ID（NDLBibID）") "NDLBibID")))
     `(("NDLBibID" . ,ndl-bib-id)))
   (when-let* ((ndc8 (map-elt rec "NDC8版")))
     `(("NDC8" . ,ndc8)))
   (when-let* ((ndc9 (map-elt rec "NDC9版")))
     `(("NDC9" . ,ndc9)))
   (when-let* ((ndc10 (map-elt rec "NDC10版")))
     `(("NDC10" . ,ndc10)))))

(defun ndlj-openurl-book-tags (rec)
  "Get tags from REC for the book item."
  (flatten-list
   (seq-uniq
    (append
     (mapcar
      (lambda (str)
        (cdr (string-split str "\\([.]? \\|--\\)" t "\\s-+")))
      (append (when (map-elt rec "NDC9版") `(,(map-elt rec "NDC9版")))
              (when (map-elt rec "NDC10版") `(,(map-elt rec "NDC10版")))))
     (mapcar
      (lambda (it)
        (cond ((map-elt it "氏名")
               (map-elt it "氏名"))
              ((and (map-elt it "氏") (map-elt it "名"))
               (concat (map-elt it "氏") " " (map-elt it "名")))
              (t
               (string-split (map-elt it "件名") " *-- *" 'omit-empty "\\s-+"))))
      (map-elt rec "件名標目"))))))

;;;###autoload
(defun ndlj-openurl-bib-item-get (search-result-item)
  "Get an item as alist from SEARCH-RESULT-ITEM."
  (let* ((item-url (map-elt search-result-item 'item-url))
         (rec (with-ndlj-url-retrieve-html item-url
                (ndlj-openurl-extract-fields dom))))
    (append
     `((ndl:url . ,item-url)
       (material-type . ,(map-elt rec "資料種別")))
     (let-alist (ndlj-api-book-titles (map-elt rec "タイトル")
                                      (map-elt rec "シリーズタイトル"))
       `((title . ,.title)
         (short-title . ,.short-title)))
     (when-let* ((volume (map-elt rec "巻次・部編番号")))
       `((volume . ,volume)))
     `((creators . ,(ndlj-api-book-creators
                     :creators (map-elt rec "著者・編者")
                     :series-creators (mapcar
                                       (lambda (it) (cons '(series .  t) it))
                                       (map-elt rec "シリーズ著者・編者"))
                     :creator-entities (map-elt rec "著者標目"))))
     (let-alist (ndlj-api-book-series (map-elt rec "シリーズタイトル"))
       (append (when .series `((series . ,.series)))
               (when .series-number `((series-number . ,.series-number)))))
     `((edition . ,(map-elt rec "版")))
     (let* ((item (seq-find (lambda (it)
                              (let ((etc (map-elt it "その他")))
                                (or (null etc) (string= etc "出版"))))
                            (map-elt rec "出版事項")))
            (publisher (map-elt item "出版社"))
            (place (map-elt item "所在地")))
       (append (when publisher `((publisher . ,publisher)))
               (when place `((place . ,place)))))
     `((date . ,(map-elt rec "出版年月日等"))
       (num-pages . ,(let ((it (map-elt rec "数量")))
                       (if (string= (map-elt it "単位") "p")
                           (map-elt it "数量"))))
       (isbn . ,(map-elt rec "ISBN"))
       (language . ,(map-elt rec "本文の言語コード"))
       (call-number . ,(map-elt rec "請求記号"))
       (extra . ,(ndlj-openurl-book-extra rec))
       (tags . ,(ndlj-openurl-book-tags rec))))))

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
    (let ((url-request-method "GET")
          (url-request-data nil)
          (url-request-extra-headers nil))
      (with-ndlj-url-retrieve-html url
        (let ((resolved-url (url-recreate-url url-http-target-url))
              (hostname (url-host url-http-target-url)))
          (cons
           resolved-url
           (mapcar
            (lambda (node)
              (let* ((item-types
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
                     (meta (car (by-class node "search-result-item-meta")))

                     (base-heading-a (dom-by-tag (by-class node "base-heading") 'a))
                     (item-url-path (dom-attr base-heading-a 'href))
                     (repo-item-id (dom-attr base-heading-a 'id)))
                `((material-types . ,item-material-types)
                  (item-url . ,(ndlj-url-unparse :netloc hostname :path item-url-path))
                  (repo-item-id . ,repo-item-id)
                  (title . ,(inner-text (car (by-class node "search-result-item-heading"))))
                  (categories . ,(concat item-types))
                  (author . ,(inner-text (car (by-class meta "author"))))
                  (publisher . ,(inner-text (car (by-class meta "publisher"))))
                  (publish-date . ,(inner-text (car (by-class meta "publish-date"))))
                  (book . ,(inner-text (car (by-class meta "book"))))
                  (book-publish-date . ,(inner-text (car (by-class meta "book-publish-date"))))
                  (page . ,(inner-text (car (by-class meta "page"))))
                  (highlight . ,(mapconcat (lambda (ul)
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

;;;###autoload
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
             `((ndl_dpid . (,(or (and (listp dpid) (string-join dpid " "))
                                 dpid)))))
           (when any `((any . (,any))))
           (when title `((btitle . (,title))))
           (when creator `((au . (,creator))))))
         (url (ndlj-url-unparse :netloc ndlj-openurl-hostname
                                :path ndlj-openurl-api-path
                                :params query-params))
         all-items)
    (while
        (pcase-let*
            ((`(,resolved-url . ,items) (ndlj-openurl--extract-search-items url))
             (url-st (ndlj-url-parse resolved-url))
             (query-params (ndlj-url-params url-st)))
          (ndlj-message "Found %d item(s) at '%s'" (length items) resolved-url)
          (when items
            (setq all-items (append all-items items))
            (unless (<= ndlj-openurl-max-items (length all-items))
              ;; Need another query to paginate
              (let ((from (or (when-let* ((from (map-elt query-params "from")))
                                (string-to-number (car from)))
                              0))
                    (size (or (when-let* ((size (map-elt query-params "size")))
                                (string-to-number (car size)))
                              (length items))))
                (setf (map-elt query-params "from") `(,(number-to-string (+ from size)))
                      (map-elt query-params "size") '("100")))
              (setf (ndlj-url-params url-st) query-params)
              (setq url (ndlj-url-unparse :url url-st))

              ;; Be nice to the API server.
              (apply #'ndlj-sleep (if (listp ndlj-openurl-sleep) ndlj-openurl-sleep `(,ndlj-openurl-sleep)))
              t))))
    (take ndlj-openurl-max-items all-items)))

(provide 'ndlj-openurl)
;;; ndlj-openurl.el ends here
