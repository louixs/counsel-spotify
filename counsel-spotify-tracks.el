;;; counsel-spotify-tracks.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Ryuei Sasaki
;;
;; Author: Ryuei Sasaki <https://github.com/george>
;; Maintainer: Ryuei Sasaki <ryuei.sasaki@gmail.com>
;; Created: April 15, 2021
;; Modified: April 15, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/george/counsel-spotify-tracks
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(require 'dash)
(require 'cousel-spotify-oauth)

(defun counsel-spotify--get-current-track-id ()
  (let* ((url (concat counsel-spotify-spotify-api-url "/me/player"))
         (token (counsel-spotify-oauth-fetch-token))
         (response (oauth2-query-results token url))
         (track (->> response (alist-get 'item)))
         (id (alist-get 'id track))
         (name (alist-get 'name track)))
    (message (concat "Current track: " name " id: " id))
    id))

(defun counsel-spotify--save-current-track-from-id (id)
  (let* ((url (concat counsel-spotify-spotify-api-url
                      "/me/tracks"
                      "?ids="
                      id))
         (token (counsel-spotify-oauth-fetch-token))
         (url-request-extra-headers '(("Content-Type" . "application/json")
                                      ("Content-Length" . "0"))))
    (message "Adding " id " to the Liked Songs. ")
    (oauth2-query-results token url "PUT" "")))

(provide 'counsel-spotify-tracks)
;;; counsel-spotify-tracks.el ends here
