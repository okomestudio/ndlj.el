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

(require 'ndlj-util)

(defconst ndlj-api-regexp-roles
  (regexp-opt '("著" "編" "訳" "監修" "漫画" "写真")))

(defconst ndlj-api--regexp-year
  "\\([0-9]\\{1,4\\}\\)\\(B\\. ?C\\.?\\|A\\. ?D\\.?\\)?")

(defun ndlj-api-zotero-item-entry (value field &optional processors)
  "Render VALUE for FIELD as a kay-value pair for plist.
Applies a FIELD function from PROCESSORS if exists."
  (when value
    `(,field ,(if-let* ((processor (map-elt processors field)))
                  (funcall processor value)
                value))))

(defun ndlj-api-parse-creator (str)
  "Parse STR and return a list of creator name/role alist for ITEM-TYPE."
  (if-let* ((pattern (format "\\`\\(?1:.*?\\)\\( +\\[?\\(?3:%s\\)\\]?\\)?\\'"
                             ndlj-api-regexp-roles))
            (_ (string-match pattern str))
            (names (string-split (match-string 1 str) ", "))
            (role (match-string 3 str)))
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

(provide 'ndlj-api)
;;; ndlj-api.el ends here
