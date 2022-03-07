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

(defun counsel-spotify-oauth2-auth-bearer ()
  `("Authorization" . ,(concat "Bearer " (oauth2-token-access-token counsel-spotify-spotify-api-auth-token))))

(aio-defun counsel-spotify--get-current-track-id-p ()
  (let* ((url (concat counsel-spotify-spotify-api-url "/me/player"))
         (response (aio-await (counsel-spotify-request-p url
                                                         :type "GET"
                                                         :headers `(("Content-Type" . "application/json")
                                                                    ,(counsel-spotify-oauth2-auth-bearer)))))
                                                                    
         (track (->> response (alist-get 'item)))
         (id (alist-get 'id track))
         (name (alist-get 'name track)))
    (message (concat "Current track: " name " id: " id))
    id))

(defun counsel-spotify--save-current-track-parser (msg)
  "For some reason spotify api returns an empty json even after a successful addition of the current track
   resulting in throwing an error. We don't want to throw an error to user in this case so wrapping it with condition-case to avoid confusion."
  (condition-case error
      (json-read)
    (error
     (message msg))))

(aio-defun counsel-spotify--save-current-track-from-id-p (id)
  (let* ((url (concat counsel-spotify-spotify-api-url
                      "/me/tracks"
                      "?ids="
                      id))
         (result (aio-await (counsel-spotify-request-p url
                                                       :type "PUT"
                                                       :parser (lambda () (counsel-spotify--save-current-track-parser (concat "Added " id "to the Liked Songs playlist.")))
                                                       :headers `(("Content-Type" . "application/json")
                                                                  ,(counsel-spotify-oauth2-auth-bearer))))))))

 

(provide 'counsel-spotify-tracks)
;;; counsel-spotify-tracks.el ends here
