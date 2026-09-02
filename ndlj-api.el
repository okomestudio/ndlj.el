;;; ndlj-api.el --- ndlj-api  -*- lexical-binding: t -*-
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
;; Provides the library of features common to all the NDL APIs.
;;
;;; Code:

(require 'compat)

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'time-date)

(require 'ndlj-util)

(defconst ndlj-api-regexp-roles
  (regexp-opt '("著" "編" "訳" "述" "編著" "監修" "共著" "漫画" "原作" "作画" "写真" "訳注")))

(defconst ndlj-api--regexp-year
  "\\([0-9]\\{1,4\\}\\)\\(B\\. ?C\\.?\\|A\\. ?D\\.?\\)?")

(defun ndlj-api-date-from-str (str)
  "Parse STR to get a decoded-time object."
  (let ((str (ndlj-str-norm str)))
    (if (string-match "\\([0-9]+\\)\\([-.]\\([0-9]+\\)\\)?\\([-.]\\([0-9]+\\)\\)?"
                      str)
        (make-decoded-time :year (when-let* ((year (match-string 1 str)))
                                   (string-to-number year))
                           :month (when-let* ((month (match-string 3 str)))
                                    (string-to-number month))
                           :day (when-let* ((day (match-string 5 str)))
                                  (string-to-number day)))
      (ndlj-message "Unparsable date: '%s'" str)
      str)))

(defun ndlj-api-publisher-parse (str)
  "Parse STR to get publisher, place, and role."
  (let ((re "\\`\\(\\(?2:[^ :]+\\) *: *\\)?\\(?3:[^ (]+\\)\\( *(\\(?5:[^)]+\\)) *\\)?\\'"))
    (when (string-match re str)
      `((place . ,(match-string 2 str))
        (publisher . ,(match-string 3 str))
        (role . ,(match-string 5 str))))))

(defun ndlj-api-parse-creator (str)
  "Parse STR and return a list of creator name/role alist for ITEM-TYPE."
  (if-let* ((pattern (format "\\`\\(?1:.*?\\)\\( +\\[?\\(?3:%s\\)\\]?\\)?\\'"
                             ndlj-api-regexp-roles))
            (_ (string-match pattern str))
            (role (match-string 3 str))
            (names (string-split (match-string 1 str) ", ")))
      (mapcar (lambda (name)
                (append
                 (when role `((role . ,role)))
                 (if-let* ((_ (string-match "\\(?1:[^ ]+\\)\\s-+\\(?2:[^ ]+\\)"
                                            name))
                           (surname (match-string 1 name))
                           (given-name (match-string 2 name)))
                     `((surname . ,surname)
                       (given-name . ,given-name))
                   (when name `((fullname . ,name))))))
              names)
    (ndlj-message "Unparsable (creators): '%s'" str)
    (list `((fullname . ,str)))))

(defun ndlj-api-parse-entity-person-name (str)
  "Parse STR and return a person entity alist."
  (if-let* ((re-name "\\([^ ,]+\\)\\(, +\\([^ ,]+?\\)\\)?")
            (re-yobd (concat ndlj-api--regexp-year
                             "-\\(" ndlj-api--regexp-year "\\)?"))
            (pattern (concat "\\`\\(?1:" re-name "\\)\\(, \\(?6:" re-yobd "\\)\\)?\\'"))
            (_ (string-match pattern str)))
      (let ((surname (match-string 2 str))
            (given-name (match-string 4 str))
            (yob (match-string 7 str))
            (yob-epoch (match-string 8 str))
            (yod (match-string 10 str))
            (yod-epoch (match-string 11 str)))
        (append
         (if (and surname given-name)
             `((surname . ,surname) (given-name . ,given-name))
           `((fullname . ,surname)))
         `((yob . ,(if yob-epoch (concat yob " " yob-epoch) yob)))
         (when yod
           `((yod . ,(if yod-epoch (concat yod " " yod-epoch) yod))))))))

(defun ndlj-api-parse-entity-person-name-yomi (str)
  "Parse STR and return a person entity yomi alist."
  (if-let* ((re-yomi "\\(\\(\\cK\\|[0-9a-zA-Z ]\\)+\\)\\(, +\\(\\(\\cK+\\|[0-9a-zA-Z ]\\)+\\)\\)?")
            (re-yobd (concat ndlj-api--regexp-year
                             "-\\(" ndlj-api--regexp-year "\\)?"))
            (pattern (concat "\\`\\(?1:" re-name "\\), \\(?5:" re-yobd "\\)"))
            (_ (string-match pattern str)))
      (let ((surname (match-string 2 str))
            (given-name (match-string 4 str))
            (yob (match-string 6 str))
            (yob-epoch (match-string 7 str))
            (yod (match-string 9 str))
            (yod-epoch (match-string 10 str)))
        (append
         (if (and surname given-name)
             `((surname-yomi . ,surname) (given-name-yomi . ,given-name))
           `((fullname-yomi . ,surname)))
         `((yob . ,(if yob-epoch (concat yob " " yob-epoch) yob)))
         (when yod
           `((yod . ,(if yod-epoch (concat yod " " yod-epoch) yod))))))))

(defun ndlj-api-tags-from-topic (str)
  "Parse tags from topic STR."
  (ndlj-debug-message "api-tags-from-topic: '%s'" str)
  (string-split str "\\( *-- *\\| *(\\|) *\\)" 'omit-empty "\\s-+"))

(defun ndlj-api-book-titles (title &optional series-title)
  "Parse TITLE and optionally SERIES-TITLE to get title and short title."
  (let* ((title (replace-regexp-in-string " *： *" "：" (ndlj-str-norm title)))
         (short-title (or (and (string-match "\\([：]\\| +\\)" title)
                               (substring title 0 (match-beginning 1)))
                          title))
         (series-title (when series-title (ndlj-str-norm series-title))))
    (when series-title
      (setq title (concat title "（" series-title "）")))
    (ndlj-alist-keep-non-nil
     `((title . ,title)
       (short-title . ,(when (< (length short-title) (length title))
                         short-title))))))

(cl-defun ndlj-api-creators (&key creators series-creators entities)
  "Parse CREATORS and SERIES-CREATORS using ENTITIES."
  (let ((creators (mapcan #'ndlj-api-parse-creator creators))
        (series-creators (mapcan (lambda (it)
                                   (mapcar (lambda (em)
                                             (cons '(series . t) em))
                                           (ndlj-api-parse-creator it)))
                                 series-creators))
        (entities (mapcar (lambda (it)
                            (cons (concat (map-elt it 'surname)
                                          (map-elt it 'given-name))
                                  it))
                          (mapcar #'ndlj-api-parse-entity-person-name entities))))
    (mapcar (lambda (it)
              (append
               (when-let* ((v (map-elt it 'role))) `((role . ,v)))
               (when-let* ((v (map-elt it 'series))) `((series . ,v)))
               (if-let* ((fullname (map-elt it 'fullname)))
                   (let* ((em (or (map-elt entities fullname) it)))
                     (if-let* ((surname (map-elt em 'surname))
                               (given-name (map-elt em 'given-name)))
                         `((surname . ,surname)
                           (given-name . ,given-name))
                       `((fullname . ,fullname))))
                 `((surname . ,(map-elt it 'surname))
                   (given-name . ,(map-elt it 'given-name))))))
            (append creators series-creators))))

(defun ndlj-api-book-series (series-title)
  "Parse SERIES-TITLE to get series and series number."
  (when-let* ((str series-title))
    (if-let* ((parts (when (string-match "[ \t]*[;][ \t]*" str)
                       (cons (substring str 0 (match-beginning 0))
                             (substring str (match-end 0))))))
        `((series . ,(car parts))
          (series-number . ,(cdr parts)) )
      `((series . ,str)))))

(defun ndlj-api-url-parse (url)
  (when-let* ((urlobj (ndlj-url-parse url))
              (path (ndlj-url-path urlobj))
              (_ (string-match "\\(?1:R[0-9]+\\)-I\\(?2:[0-9]+\\)" path)))
    `((repo-id . ,(match-string 1 path))
      (bib-id . ,(match-string 2 path)))))

(provide 'ndlj-api)
;;; ndlj-api.el ends here
