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

(require 'seq)
(require 'xml)

(require 'ndlj-api)
(require 'ndlj-util)
(require 'ndlj-openurl)

(defun ndlj-oaipmh-url-get-record (repo-item-id)
  "Generate the entity URL for REPO-ITEM-ID.
REPO-ITEM-ID is of form `R<number>-I<number>'."
  (let ((identifier (format "oai:ndlsearch.ndl.go.jp:%s" repo-item-id)))
    (ndlj-url-unparse :netloc "ndlsearch.ndl.go.jp"
                      :path "/api/oaipmh"
                      :params `((verb . ("GetRecord"))
                                (metadataPrefix . ("dcndl_v3"))
                                (identifier . (,identifier))))))

(defun ndlj-oaipmh-ndl-repo-id (dom)
  "Extract a repository ID (レポジトリ番号) from DOM."
  (ndlj-dom-by-path dom 'dcndl:bibRecordCategory))

(defun ndlj-oaipmh-ndl-bib-id (dom)
  "Extract an NDLBibID from DOM."
  (ndlj-dom-by-tag-by-attr dom 'dcterms:identifier 'rdf:datatype
                           "http://ndl.go.jp/dcndl/terms/NDLBibID"))

(defun ndlj-oaipmh-ndlsh (dom)
  "Extract NDL subject headings (件名標目) from DOM."
  (let ((pattern "\\`http://id.ndl.go.jp/auth/ndlsh/\\(?1:.*\\)\\'"))
    (mapcar
     (lambda (node)
       (ndlj-alist-keep-non-nil
        `((id . ,(when-let* ((val (dom-attr node 'rdf:about))
                             (_ (string-match pattern val)))
                   (match-string 1 val)))
          (subject . ,(dom-inner-text (dom-by-tag node 'rdf:value))))))
     (ndlj-dom-by-path dom '(dcterms:subject rdf:Description)
                       :fun nil :reducer nil))))

(defun ndlj-oaipmh-ndlc (dom)
  "Extract the NDLC from DOM."
  (when-let*
      ((resource "\\`http://id.ndl.go.jp/class/ndlc/\\(.*\\)\\'")
       (s (ndlj-dom-by-tag-by-attr dom 'dcterms:subject 'rdf:resource resource
                                   :fun (lambda (n)
                                          (dom-attr n 'rdf:resource))))
       (_ (string-match resource s)))
    (match-string 1 s)))

(defun ndlj-oaipmh-ndc (dom version)
  "Extract the VERSION of NDC from DOM."
  (when-let*
      ((resource (format "\\`http://id.ndl.go.jp/class/ndc%d/\\(.*\\)" version))
       (s (ndlj-dom-by-tag-by-attr dom 'dcterms:subject 'rdf:resource resource
                                   :fun (lambda (n)
                                          (dom-attr n 'rdf:resource))))
       (_ (string-match resource s)))
    (match-string 1 s)))

(defun ndlj-oaipmh-magazine-article-item-get (search-result-item)
  (let* ((item-url (map-elt search-result-item 'item-url))
         (repo-item-url (ndlj-oaipmh-url-get-record
                         (map-elt search-result-item 'repo-item-id)))
         (results (ndlj-url-retrieve-gather
                   `((,repo-item-url . ndlj-url-retrieve-as-xml))))
         (rec (dom-by-tag (plist-get (nth 0 results) :value) 'record)))
    (ndlj-alist-keep-non-nil
     (append
      `((ndl:item-url . ,item-url)
        (material-type . ,(ndlj-dom-by-path-attr rec 'dcndl:materialType 'rdfs:label)))
      (let-alist (ndlj-api-book-titles
                  (ndlj-dom-by-path rec '(dc:title rdf:value))
                  (ndlj-dom-by-path rec '(dcndl:seriesTitle rdf:value)))
        `((title . ,.title)
          (short-title . ,.short-title)))
      `((creators . ,(ndlj-api-creators
                      :creators (ndlj-dom-by-path rec 'dc:creator :reducer nil)
                      :entities (ndlj-dom-by-path rec '(dcterms:creator foaf:name) :reducer nil))))
      (let-alist (ndlj-api-publisher-parse
                  (ndlj-dom-by-path rec '(dcterms:publisher foaf:name)))
        `((publisher . ,.publisher)
          (place . ,.place)))
      `((date . ,(ndlj-api-date-from-str (ndlj-dom-by-path rec 'dcterms:issued)))
        (publication-title . ,(ndlj-dom-by-path rec '(dcndl:publicationName rdf:value)))
        (volume . ,(ndlj-dom-by-path rec '(dcndl:publicationName dcndl:publicationVolume)))
        (issue . ,(ndlj-dom-by-path rec '(dcndl:publicationName dcndl:number)))
        (pages . ,(ndlj-dom-by-path rec '(dcndl:publicationName dcndl:pageRange)))
        (language . ,(ndlj-dom-by-path rec 'dcterms:language))
        (call-number . ,(ndlj-dom-by-path rec 'dcndl:callNumber))
        (extra . (("NDLBibID" . ,(ndlj-oaipmh-ndl-bib-id rec))
                  ("NDLRepoID" . ,(ndlj-oaipmh-ndl-repo-id rec)))))
      ))))

(defun ndlj-oaipmh-book-parts (dom)
  "Extract book parts information from DOM."
  (mapcar (lambda (node)
            `((title . ,(ndlj-dom-by-path node 'dcterms:title))
              (creators . ,(ndlj-api-creators
                            :creators (ndlj-dom-by-path node 'dc:creator
                                                        :reducer nil)))))
          (ndlj-dom-by-path dom 'dcndl:partInformation :fun nil :reducer nil)))

(defun ndlj-oaipmh-book-item-get (search-result-item)
  (let* ((item-url (map-elt search-result-item 'item-url))
         (repo-item-url (ndlj-oaipmh-url-get-record
                         (map-elt search-result-item 'repo-item-id)))
         (results (ndlj-url-retrieve-gather
                   `((,repo-item-url . ndlj-url-retrieve-as-xml)
                     (,item-url . ndlj-openurl-book-extract-plus))))
         (rec (dom-by-tag (plist-get (nth 0 results) :value) 'record))
         (openurl (plist-get (nth 1 results) :value)))
    (ndlj-alist-keep-non-nil
     (append
      `((ndl:item-url . ,item-url)
        (material-type . ,(ndlj-dom-by-path-attr rec 'dcndl:materialType 'rdfs:label)))
      (let-alist (ndlj-api-book-titles (ndlj-dom-by-path rec '(dc:title rdf:value)))
        `((title . ,.title)
          (short-title . ,.short-title)))
      `((volume . ,(ndlj-dom-by-path rec '(dcndl:volume rdf:value)))
        (creators
         . ,(ndlj-api-creators
             :creators
             (seq-keep (lambda (it)
                         ;; Only keep direct children of BibResource;
                         ;; otherwise, creators may be fetched from part
                         ;; information.
                         (when (eq (car (dom-parent rec it)) 'dcndl:BibResource)
                           (dom-inner-text it)))
                       (dom-by-tag rec 'dc:creator))
             :series-creators (ndlj-dom-by-path rec 'dcndl:seriesCreator :reducer nil)
             :entities (ndlj-dom-by-path rec '(dcterms:creator foaf:name) :reducer nil))))
      (let-alist (ndlj-api-book-series
                  (ndlj-dom-by-path rec '(dcndl:seriesTitle rdf:value)))
        (append (when .series `((series . ,.series)))
                (when .series-number `((series-number . ,.series-number)))))
      `((edition . ,(ndlj-dom-by-path rec 'dcndl:edition))
        (publisher . ,(ndlj-dom-by-path rec '(dcterms:publisher foaf:name)))
        (place . ,(ndlj-dom-by-path rec '(dcterms:publisher dcndl:location)))
        (date . ,(ndlj-api-date-from-str (ndlj-dom-by-path rec 'dcterms:date))))
      (when-let* ((s (ndlj-dom-by-path rec 'dcterms:extent))
                  (_ (string-match "\\([0-9]+\\)\\s-?p" s))
                  (num-pages (match-string 1 s)))
        `((num-pages . ,num-pages)))
      `((isbn . ,(ndlj-dom-by-tag-by-attr rec 'dcterms:identifier 'rdf:datatype
                                          "\\`http://ndl.go.jp/dcndl/terms/ISBN\\'"))
        (language . ,(ndlj-dom-by-path rec 'dcterms:language))
        (call-number . ,(ndlj-dom-by-path rec 'dcndl:callNumber))
        (parts . ,(ndlj-oaipmh-book-parts rec))
        (ndlsh . ,(ndlj-oaipmh-ndlsh rec))
        (ndlc . ,(ndlj-oaipmh-ndlc rec))
        (ndc8 . ,(or (map-elt openurl 'ndc8) (ndlj-oaipmh-ndc rec 8)))
        (ndc9 . ,(or (map-elt openurl 'ndc9) (ndlj-oaipmh-ndc rec 9)))
        (ndc10 . ,(or (map-elt openurl 'ndc10) (ndlj-oaipmh-ndc rec 10)))
        (ndl-bib-id . ,(ndlj-oaipmh-ndl-bib-id rec))
        (ndl-repo-id . ,(ndlj-oaipmh-ndl-repo-id rec))
        (note-general . ,(map-elt openurl 'note-general))
        (index . ,(map-elt openurl 'index))
        (summary . ,(map-elt openurl 'summary)))))))

;;;###autoload
(defun ndlj-oaipmh-bib-item-get (search-result-item)
  "Get an item as alist from SEARCH-RESULT-ITEM."
  (let ((material-types (map-elt search-result-item 'material-types)))
    (cond
     ((seq-intersection '("記事") material-types)
      (ndlj-oaipmh-magazine-article-item-get search-result-item))
     ((seq-intersection '("図書") material-types)
      (ndlj-oaipmh-book-item-get search-result-item))
     (t (ndlj-message "Unknwon material types: '%s'" material-types)))))

(provide 'ndlj-oaipmh)
;;; ndlj-oaipmh.el ends here
