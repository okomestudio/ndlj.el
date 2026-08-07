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

(defun ndlj-oaipmh-url-get-record (repo-item-id)
  "Generate the entity URL for REPO-ITEM-ID.
REPO-ITEM-ID is of form `R<number>-I<number>'."
  (let ((identifier (format "oai:ndlsearch.ndl.go.jp:%s" repo-item-id)))
    (ndlj-url-unparse :netloc "ndlsearch.ndl.go.jp"
                      :path "/api/oaipmh"
                      :params `((verb . ("GetRecord"))
                                (metadataPrefix . ("dcndl_v3"))
                                (identifier . (,identifier))))))

(defun ndlj-oaipmh-magazine-article-item-get (search-result-item)
  (let* ((item-url (map-elt search-result-item 'item-url))
         (repo-item-url (ndlj-oaipmh-url-get-record
                         (map-elt search-result-item 'repo-item-id)))
         (results (ndlj-url-retrieve-gather
                   `((,repo-item-url . ndlj-url-retrieve-as-xml))))
         (rec (dom-by-tag (plist-get (nth 0 results) :value) 'record)))
    (seq-filter
     #'cdr
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
        (extra
         . (("NDLBibID" . ,(ndlj-dom-by-tag-by-attr rec 'dcterms:identifier 'rdf:datatype
                                                    "http://ndl.go.jp/dcndl/terms/NDLBibID"))
            ("NDLRepoID" . ,(ndlj-dom-by-path rec 'dcndl:bibRecordCategory)))))
      ))))

(defun ndlj-oaipmh-book-extra (dom)
  (append
   (when-let* ((resource "\\`http://id.ndl.go.jp/class/ndc10/\\(.*\\)")
               (n (ndlj-dom-by-tag-by-attr dom 'dcterms:subject 'rdf:resource resource))
               (s (dom-attr n 'rdf:resource))
               (_ (string-match resource s)))
     `(("NDLC10" . ,(match-string 1 s))))
   (when-let* ((resource "http://ndl.go.jp/dcndl/terms/NDLBibID")
               (n (ndlj-dom-by-tag-by-attr dom 'dcterms:identifier 'rdf:datatype resource)))
     `(("NDLBibID" . ,(dom-inner-text n))))))

(defun ndlj-oaipmh-book-item-get (search-result-item)
  (let* ((item-url (map-elt search-result-item 'item-url))
         (repo-item-url (ndlj-oaipmh-url-get-record
                         (map-elt search-result-item 'repo-item-id)))
         (results (ndlj-url-retrieve-gather
                   `((,repo-item-url . ndlj-url-retrieve-as-xml)
                     (,item-url
                      . (lambda (start end)
                          (require 'ndlj-openurl nil t)
                          (let* ((dom (ndlj-url-retrieve-as-html start end))
                                 (rec (ndlj-openurl-extract-fields dom)))
                            `( :extra ,(ndlj-openurl-book-extra rec)
                               :tags ,(ndlj-openurl-book-tags rec) )))))))
         (rec (dom-by-tag (plist-get (nth 0 results) :value) 'record))
         (extra (plist-get (plist-get (nth 1 results) :value) :extra))
         (tags (plist-get (plist-get (nth 1 results) :value) :tags)))
    (seq-filter
     #'cdr
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
        (extra . ,(or extra (ndlj-oaipmh-book-extra rec)))
        (tags
         . ,(seq-uniq
             (append tags
                     (mapcan #'ndlj-api-tags-from-topic
                             (ndlj-dom-by-path rec '(dcterms:subject rdf:value) :reducer nil))))))
      ))))

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
