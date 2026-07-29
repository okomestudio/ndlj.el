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

(defun ndlj-oaipmh-book-title (dom)
  (dom-inner-text (dom-by-tag (dom-by-tag dom 'dc:title) 'rdf:value)))

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

(defun ndlj-oaipmh-book-series (dom)
  (let* ((n (dom-by-tag (dom-by-tag dom 'dcndl:seriesTitle) 'rdf:value))
         (s (dom-inner-text n))
         (parts (when (string-match "[ \t]*[;][ \t]*" s)
                  (cons (substring s 0 (match-beginning 0))
                        (substring s (match-end 0))))))
    (if parts
        `((series . ,(car parts))
          (series-number . ,(cdr parts)) )
      `((series . ,s)))))

(defun ndlj-oaipmh-book-edition (dom)
  (dom-inner-text (dom-by-tag dom 'dcndl:edition)))

(defun ndlj-oaipmh-book-publisher (dom)
  (let* ((n (dom-by-tag dom 'dcterms:publisher))
         (publisher (dom-inner-text (dom-by-tag n 'foaf:name)))
         (place (dom-inner-text (dom-by-tag n 'dcndl:location))))
    (append (when publisher
              `((publisher . ,publisher)))
            (when place
              `((place . ,place))))))

(defun ndlj-oaipmh-book-date (dom)
  (dom-inner-text (dom-by-tag dom 'dcterms:date)))

(defun ndlj-oaipmh-book-pages (dom)
  (when-let* ((s (dom-inner-text (dom-by-tag dom 'dcterms:extent)))
              (_ (string-match "\\([0-9]+\\)\\s-?p" s)))
    (match-string 1 s)))

(defun ndlj-oaipmh-book-isbn (dom)
  (dom-inner-text
   (ndlj-oaipmh-dom-by-tag-attr dom 'dcterms:identifier 'rdf:datatype
                                "\\`http://ndl.go.jp/dcndl/terms/ISBN\\'")))

(defun ndlj-oaipmh-book-language (dom)
  (dom-inner-text (dom-by-tag dom 'dcterms:language)))

(defun ndlj-oaipmh-book-call-number (dom)
  (dom-inner-text (dom-by-tag dom 'dcndl:callNumber)))

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

(defun ndlj-oaipmh-bib-item-get (search-result-item)
  "Get an item as alist from SEARCH-RESULT-ITEM."
  (let* ((repo-item-id (map-elt search-result-item 'repo-item-id))
         (item-url (ndlj-oaipmh-url-get-record repo-item-id))
         (rec (with-ndlj-url-retrieve-xml item-url
                (when ndlj-debug (dom-pp dom))
                (dom-by-tag dom 'record))))
    (append
     `((ndl:url . ,item-url)
       (material-type . ,(dom-attr (dom-by-tag rec 'dcndl:materialType) 'rdfs:label))
       (title . ,(ndlj-oaipmh-book-title rec))
       (creators . ,(ndlj-oaipmh-book-creators rec))
       ,(ndlj-oaipmh-book-series rec)
       (edition . ,(ndlj-oaipmh-book-edition rec))
       ,(ndlj-oaipmh-book-publisher rec)
       (date . ,(ndlj-oaipmh-book-date rec))
       (num-pages . ,(ndlj-oaipmh-book-pages rec))
       (isbn . ,(ndlj-oaipmh-book-isbn rec))
       (language . ,(ndlj-oaipmh-book-language rec))
       (call-number . ,(ndlj-oaipmh-book-call-number rec))
       (extra . ,(ndlj-oaipmh-book-extra rec))))))

(provide 'ndlj-oaipmh)
;;; ndlj-oaipmh.el ends here
