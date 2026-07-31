;;; ndlj-oaipmh.el --- ndlj-oaipmh  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2026 Taro Sato
;;
;;; License:
;;
;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; The OAI-PMH API integration.
;;
;;; Code:

(require 'compat)

(require 'xml)

(require 'ndlj-api)
(require 'ndlj-util)

(defun ndlj-oaipmh-url-get-record (repo-item-id)
  "Generate the entity URL for REPO-ITEM-ID.
REPO-ITEM-ID is of form `R<number>-I<number>'."
  (let ((identifier (format "oai:ndlsearch.ndl.go.jp:%s" repo-item-id)))
    (ndlj-url-unparse :netloc "ndlsearch.ndl.go.jp"
                      :path "/api/oaipmh"
                      :params `((verb . ("GetRecord"))
                                (metadataPrefix . ("dcndl_v3"))
                                (identifier . (,identifier))))))

(defun ndlj-oaipmh-dom-by-tag-attr (dom tag attribute value)
  "Get DOM nodes by TAG matching ATTRIBUTE VALUE."
  (seq-keep (lambda (node)
              (when-let* ((s (dom-attr node attribute)))
                (when (string-match value s)
                  node)))
            (dom-by-tag dom tag)))

(defun ndlj-oaipmh-book-creators (dom)
  (let ((creators
         (apply #'append
                (seq-keep (lambda (it)
                            (ndlj-api-parse-creator (dom-inner-text it)))
                          (dom-by-tag dom 'dc:creator))))
        (creator-entities
         (mapcar (lambda (it)
                   (cons (concat (map-elt it 'surname) (map-elt it 'given-name))
                         it))
                 (seq-keep (lambda (it)
                             (ndlj-api-parse-entity-person-name
                              (dom-inner-text (dom-by-tag it 'foaf:name))))
                           (dom-by-tag dom 'dcterms:creator)))))
    (mapcar (lambda (it)
              (append
               `((role . ,(map-elt it 'role)))
               (if-let* ((fullname (map-elt it 'fullname))
                         (ce (map-elt creator-entities fullname)))
                   `((surname . ,(map-elt ce 'surname))
                     (given-name . ,(map-elt ce 'given-name)) )
                 `((fullname . ,(map-elt it 'fullname)) ))))
            creators)))

(defun ndlj-oaipmh-book-extra (dom)
  (append
   (when-let* ((resource "\\`http://id.ndl.go.jp/class/ndc10/\\(.*\\)")
               (n (ndlj-oaipmh-dom-by-tag-attr dom 'dcterms:subject 'rdf:resource resource))
               (s (dom-attr n 'rdf:resource))
               (_ (string-match resource s)))
     `(("NDLC10" . ,(match-string 1 s))))
   (when-let* ((resource "http://ndl.go.jp/dcndl/terms/NDLBibID")
               (n (ndlj-oaipmh-dom-by-tag-attr dom 'dcterms:identifier 'rdf:datatype resource)))
     `(("NDLBibID" . ,(dom-inner-text n))))))

;;;###autoload
(defun ndlj-oaipmh-bib-item-get (search-result-item)
  "Get an item as alist from SEARCH-RESULT-ITEM."
  (let* ((item-url (map-elt search-result-item 'item-url))
         (repo-item-id (map-elt search-result-item 'repo-item-id))
         (repo-item-url (ndlj-oaipmh-url-get-record repo-item-id))
         (results
          (ndlj-url-retrieve-gather
           `((,repo-item-url . ndlj-url-retrieve-as-xml)
             (,item-url . (lambda (start end)
                            (let ((dom (ndlj-url-retrieve-as-html start end)))
                              (require 'ndlj-openurl nil t)
                              (let ((rec (ndlj-openurl-extract-fields dom)))
                                `( :extra ,(ndlj-openurl-book-extra rec)
                                   :tags ,(ndlj-openurl-book-tags rec) ))))))))
         (rec (dom-by-tag (plist-get (nth 0 results) :value) 'record))
         (extra (plist-get (plist-get (nth 1 results) :value) :extra))
         (tags (plist-get (plist-get (nth 1 results) :value) :tags)))
    (append
     `((ndl:url . ,item-url)
       (material-type . ,(dom-attr (dom-by-tag rec 'dcndl:materialType) 'rdfs:label)))
     (let-alist (ndlj-api-book-titles
                 (dom-inner-text (dom-by-tag (dom-by-tag rec 'dc:title) 'rdf:value)))
       `((title . ,.title)
         (short-title . ,.short-title)))
     `((volume . ,(dom-inner-text (dom-by-tag (dom-by-tag rec 'dcndl:volume) 'rdf:value))))
     `((creators . ,(ndlj-oaipmh-book-creators rec)))
     (let-alist (ndlj-api-book-series
                 (dom-inner-text (dom-by-tag (dom-by-tag rec 'dcndl:seriesTitle) 'rdf:value)))
       (append (when .series `((series . ,.series)))
               (when .series-number `((series-number . ,.series-number)))))
     `((edition . ,(dom-inner-text (dom-by-tag rec 'dcndl:edition))))
     (let* ((node (dom-by-tag rec 'dcterms:publisher))
            (publisher (dom-inner-text (dom-by-tag node 'foaf:name)))
            (place (dom-inner-text (dom-by-tag node 'dcndl:location))))
       (append (when publisher `((publisher . ,publisher)))
               (when place `((place . ,place)))))
     `((date . ,(ndlj-api-date-from-str
                 (dom-inner-text (dom-by-tag rec 'dcterms:date)))))
     (when-let* ((s (dom-inner-text (dom-by-tag rec 'dcterms:extent)))
                 (_ (string-match "\\([0-9]+\\)\\s-?p" s))
                 (num-pages (match-string 1 s)))
       `((num-pages . ,num-pages)))
     `((isbn . ,(dom-inner-text
                 (ndlj-oaipmh-dom-by-tag-attr
                  rec 'dcterms:identifier 'rdf:datatype
                  "\\`http://ndl.go.jp/dcndl/terms/ISBN\\'")))
       (language . ,(dom-inner-text (dom-by-tag rec 'dcterms:language)))
       (call-number . ,(dom-inner-text (dom-by-tag rec 'dcndl:callNumber)))
       (extra . ,(or extra (ndlj-oaipmh-book-extra rec)))
       (tags . ,tags)))))

(provide 'ndlj-oaipmh)
;;; ndlj-oaipmh.el ends here
