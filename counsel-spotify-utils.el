;;; counsel-spotify-utils.el --- Description -*- lexical-binding: t -*-
;;
;;
;; Author: Ryuei Sasaki <https://github.com/louixs>
;; Created: May 22, 2022
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(require 'counsel-spotify-backends)

(defun counsel-spotify-get-id-from-uri (uri)
  (car
   (cdr
    (cdr
     (split-string uri ":")))))

(defun counsel-spotify-get-id (spotify-object-string)
  (counsel-spotify-get-id-from-uri
   (uri (counsel-spotify-unwrap-spotify-object spotify-object-string))))

(provide 'counsel-spotify-utils)
;;; counsel-spotify-utils.el ends here
