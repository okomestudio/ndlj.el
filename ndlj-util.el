;;; ndlj-util.el --- ndlj-util  -*- lexical-binding: t -*-
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
;; Provides utility functions for `ndlj'.
;;
;;; Code:

(require 'compat)

(require 'cl-lib)
(require 'url)
(require 'url-expand)
(require 'url-util)
(require 'xml)

;;; HTTP

(defun ndlj-url-retrieve-as-html (start end)
  (libxml-parse-html-region start end))

(defun ndlj-url-retrieve-as-xml (start end)
  (xml-parse-region start end nil nil nil))

(defun ndlj-url-retrieve-gather (urls &optional timeout)
  "Fetch URLS concurrently and block until all complete.
Returns a list of response bodies matching the order of URLS.
Optionally accepts a TIMEOUT in seconds."
  (let* ((count (length urls))
         (results (make-vector count nil))
         (remaining count)
         (start-time (float-time)))
    (cl-loop
     for url-item in urls
     for index from 0
     do (let ((idx index)
              (url (or (and (consp url-item) (car url-item)) url-item))
              (retriever (and (consp url-item) (cdr url-item))))
          (url-retrieve
           url
           (lambda (status)
             (set-buffer-multibyte t)
             (decode-coding-region (point-min) (point-max) 'utf-8)
             (goto-char (point-min))
             (let ((body
                    (if (re-search-forward "\r?\n\r?\n" nil t)
                        (if retriever
                            (funcall retriever (point) (point-max))
                          (buffer-substring-no-properties (point) (point-max)))
                      (buffer-string))))
               (kill-buffer)
               (aset results idx `(:status ,status :value ,body))
               (cl-decf remaining))))))

    (while (and (> remaining 0)
                (or (null timeout)
                    (< (- (float-time) start-time) timeout)))
      (accept-process-output nil 0.05))

    (if (> remaining 0)
        (error "Timed out waiting for %d request(s) to finish" remaining)
      (append results nil))))

(defmacro with-ndlj-url-retrieve (url &rest body)
  "Retrieve URL into a buffer and run BODY in it."
  (declare (indent 1))
  `(let ((url-automatic-caching t)
         ;; TODO: Use `url-retrieve' for asynchronous callbacks:
         (buf (url-retrieve-synchronously ,url)))
     (unless buf
       (error "Response not received from %s" ,url))
     (when buf
       (prog1
           (with-current-buffer buf
             (set-buffer-multibyte t)
             (decode-coding-region (point-min) (point-max) 'utf-8)
             (progn ,@body))
         (kill-buffer buf)
         (when ndlj-debug
           (message "Received response from %s" ,url))))))

(defmacro with-ndlj-url-retrieve-html (url &rest body)
  "Retrieve an HTML content from URL into a buffer and run BODY in it.
Within BODY, the variable `dom' is available for processing."
  (declare (indent 1))
  `(with-ndlj-url-retrieve ,url
     (goto-char (point-min))
     (re-search-forward "\r?\n\r?\n" nil t)
     (let ((dom (libxml-parse-html-region (point) (point-max))))
       (when ndlj-debug
         (pp dom))
       ,@body)))

(defmacro with-ndlj-url-retrieve-xml (url &rest body)
  "Retrieve an XML content from URL into a buffer and run BODY in it.
Within BODY, the variable `dom' is available for processing."
  (declare (indent 1))
  `(with-ndlj-url-retrieve ,url
     (goto-char (point-min))
     (re-search-forward "\r?\n\r?\n" nil t)
     (let ((dom (xml-parse-region (point) (point-max) nil nil nil)))
       (when ndlj-debug
         (pp dom))
       ,@body)))

(defun ndlj-sleep (seconds &optional jitter)
  "Sleep for SECONDS plus a JITTER.
When given, JITTER in seconds will be used to generate a random number
betwee 0 and JITTER."
  (sleep-for (+ seconds (if jitter
                            (/ (random (round (* jitter 1000.0))) 1000.0)
                          0))))

;;; URL

(cl-defstruct (ndlj-url (:include url))
  path params)

(cl-defun ndlj-url-parse (url)
  "Parse URL into a `ndlj-url' struct."
  (let ((url (url-generic-parse-url url)))
    (make-ndlj-url :type (url-type url)
                   :user (url-user url)
                   :password (url-password url)
                   :host (url-host url)
                   :portspec (url-portspec url)
                   :filename (url-filename url)
                   :target (url-target url)
                   :fullness (url-fullness url)
                   :path (car (url-path-and-query url))
                   :params (when (cdr (url-path-and-query url))
                             (url-parse-query-string
                              (cdr (url-path-and-query url)))))))

(cl-defun ndlj-url-unparse ( &key
                             url
                             scheme
                             netloc
                             path
                             params
                             query    ; unimplemented (see RFC 2396)
                             fragment
                             username
                             password
                             hostname
                             port )
  "Construct a safe, fully-encoded URL."
  (let ((path (or (and url (ndlj-url-path url))
                  (and path (mapconcat #'url-hexify-string
                                       (string-split path "/")
                                       "/"))))
        (params (or (and url (ndlj-url-params url))
                    params)))
    (when netloc
      (when (string-match
             "\\`\\(\\(?2:[^:]+\\)\\(\\:\\(?4:[^@]+\\)\\)@\\)?\\(?5:[^:]+\\)\\(\\:\\(?7:.+\\)\\)?\\'"
             netloc)
        (setq username (match-string 2 netloc)
              password (match-string 4 netloc)
              hostname (match-string 5 netloc)
              port (match-string 7 netloc))))
    (url-recreate-url
     (url-parse-make-urlobj
      (or (and url (url-type url)) scheme "https")
      (or (and url (url-user url)) username)
      (or (and url (url-password url)) password)
      (or (and url (url-host url)) hostname)
      (or (and url (url-portspec url)) port)
      (if params (concat path "?" (url-build-query-string params)) path)
      (or (and url (url-target url)) fragment)
      nil t))))

;;; String Operations

(defun ndlj-string-normalize-ja (str)
  "Normalize STR with standard Japanese characters (typically zenkaku)."
  (pcase-dolist (`(,from . ,to) '(("　" . " ")
                                  ("!" . "！")
                                  ("?" . "？")
                                  ("(" . "（")
                                  (")" . "）")
                                  ("\\[" . "［")
                                  ("\\]" . "］")
                                  (":" . "：")
                                  (" +" . " ")))
    (setq str (replace-regexp-in-string from to str)))
  str)

;;; System

(defun ndlj-message (s &rest args)
  "Display S via `message'.
ARGS are data used if S is format string."
  (apply #'message `(,(concat "[ndlj] " s) ,@args)))

(defun ndlj-warn (s &rest args)
  "Display S via `warning'.
ARGS are data used if S is format string."
  (apply #'warn `(,(concat "[ndlj] " s) ,@args)))

(provide 'ndlj-util)
;;; ndlj-util.el ends here
