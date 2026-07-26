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

(require 'url-expand)
(require 'url-util)

(defun ndlj-build-url (base-url path-segments query-alist)
  "Construct a safe, fully-encoded URL.
BASE-URL is the endpoint root (e.g., https://example.com/api/).
PATH-SEGMENTS is a list of unencoded string directories or endpoints.
QUERY-ALIST is an association list of keys and values for parameters."
  (let* ((clean-path (mapconcat #'url-hexify-string path-segments "/"))
         (full-url (url-expand-file-name clean-path base-url)))
    (if query-alist
        (concat full-url "?" (url-build-query-string query-alist))
      full-url)))

(defun ndlj-sleep (seconds &optional jitter)
  "Sleep for SECONDS plus a JITTER.
When given, JITTER in seconds will be used to generate a random number
betwee 0 and JITTER."
  (sleep-for (+ seconds (if jitter
                            (/ (random (round (* jitter 1000.0))) 1000.0)
                          0))))

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
