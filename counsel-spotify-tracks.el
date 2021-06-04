;;; counsel-spotify-tracks.el --- Description -*- lexical-binding: t; -*-
;;
;;
;; Author: Ryuei Sasaki <https://github.com/louixs>
;; Created: April 15, 2021
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(require 'dash)
(require 'counsel-spotify-oauth)

(aio-defun counsel-spotify--get-current-track-id-p ()
  (let* ((url (concat counsel-spotify-spotify-api-url "/me/player"))
         (token (counsel-spotify-oauth-fetch-token))
         (response (aio-await (counsel-spotify-oauth2-url-retrieve-p token url)))
         (track (->> response (alist-get 'item)))
         (id (alist-get 'id track))
         (name (alist-get 'name track)))
    (message (concat "Current track: " name " id: " id))
    id))

(aio-defun counsel-spotify--save-current-track-from-id-p (id)
  (let* ((url (concat counsel-spotify-spotify-api-url
                      "/me/tracks"
                      "?ids="
                      id))
         (token (counsel-spotify-oauth-fetch-token))
         (url-request-extra-headers '(("Content-Type" . "application/json")
                                      ("Content-Length" . "0")))
         (result (aio-await (counsel-spotify-oauth2-url-retrieve-p token url "PUT" ""))))
    (message "Reuslt: %s" result)
    (message "%s added to the Liked Songs." id)))

(provide 'counsel-spotify-tracks)
;;; counsel-spotify-tracks.el ends here
