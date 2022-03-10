;;; counsel-spotify-playback.el --- Description -*- lexical-binding: t; -*-
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
(require 'counsel-spotify-oauth)

(defun counsel-spotify-oauth2-auth-bearer ()
  `("Authorization" . ,(concat "Bearer " (oauth2-token-access-token counsel-spotify-spotify-api-auth-token))))

(aio-defun counsel-spotify--get-current-playback-id-p ()
  (let* ((url (concat counsel-spotify-spotify-api-url "/me/player/currently-playing?additional_types=track,episode"))
         (response (aio-await (counsel-spotify-request-p url
                                                         :type "GET"
                                                         :headers `(("Content-Type" . "application/json")
                                                                    ,(counsel-spotify-oauth2-auth-bearer)))))
         (item (alist-get 'item response))
         (id (alist-get 'id item))
         (type (alist-get 'type item))
         (name (alist-get 'name item)))
    `((name . ,name)
      (id . ,id)
      (type . ,type))))
    

(defun counsel-spotify--save-current-playback-parser (msg)
  "For some reason spotify api returns an empty json even after a successful addition of the current track
   resulting in throwing an error. We don't want to throw an error to user in this case so wrapping it with condition-case to avoid confusion."
  (condition-case error
      (json-read)
    (error
     (message msg))))

(aio-defun counsel-spotify--save-current-playback-from-id-p (data)
  (let* ((id (alist-get 'id data))
         (name (alist-get 'name data))
         (type (alist-get 'type data))
         (save-to (cond
                   ((string-equal type "track") "tracks")
                   ((string-equal type "episode") "episodes")))
         (url (concat counsel-spotify-spotify-api-url
                      "/me/"
                      save-to
                      "?ids="
                      id))
         (result (aio-await (counsel-spotify-request-p url
                                                       :type "PUT"
                                                       :parser (lambda () (counsel-spotify--save-current-playback-parser (concat "Added " "'" name "'" " to your library.")))
                                                       :headers `(("Content-Type" . "application/json")
                                                                  ,(counsel-spotify-oauth2-auth-bearer))))))))

(provide 'counsel-spotify-playback)
;;; counsel-spotify-playback.el ends here
