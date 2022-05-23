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
(require 'counsel-spotify-search)

(defun counsel-spotify--ask-user-episode-or-show (type)
  "Interactrively asks user to choose the type of item to save if because if it's a podcast episode, user can save the episode itself or the podcast show to their library.
   To call this programatically use call-interactively function."
  (interactive
   (list
    (completing-read "Would you like to save the podcast episode or the show?" '("episode" "show"))))
 type)

(aio-defun counsel-spotify--get-current-playback-data-p ()
  (let* ((url (concat counsel-spotify-spotify-api-url "/me/player/currently-playing?additional_types=track,episode"))
         (response (aio-await (counsel-spotify-request-p url
                                                         :type "GET"
                                                         :headers (aio-await (counsel-spotify-oauth-bearer-headers-p)))))
         (item (alist-get 'item response)))
    item))

(defun counsel-spotify--save-current-playback-request-parser (msg)
  "For some reason spotify api returns an empty json even after a successful addition of the current track
   resulting in throwing an error. We don't want to throw an error to user in this case so wrapping it with condition-case to avoid confusion."
  (condition-case error
      (json-read)
    (error
     (message msg))))

(defun counsel-spotify--parse-playback-item (item)
  (let* ((id (alist-get 'id item))
         (name (alist-get 'name item))
         (type (alist-get 'type item))
         (id (if (string-equal type "show") (alist-get 'id (alist-get 'show item)) id))
         (name (if (string-equal type "show") (alist-get 'name (alist-get 'show item)) name)))
    `((name . ,name)
      (id . ,id)
      (type . ,type))))


(aio-defun counsel-spotify--save-current-playback (item)
  (let* ((data (counsel-spotify--parse-playback-item item))
         (id (alist-get 'id data))
         ;; If it's a podcast episode, ask user to confirm the episode or the show that user wants to save
         (save-type (if (string-equal type "episode") (call-interactively #'counsel-spotify--ask-user-episode-or-show) type))
         (save-to (cond
                   ((string-equal save-type "track") "tracks")
                   ((string-equal save-type "episode") "episodes")
                   ((string-equal save-type "show") "shows")))
         (added-msg (concat "Added " "'" name "'" " (" type ")"  " to your library."))
         (url (concat counsel-spotify-spotify-api-url
                      "/me/"
                      save-to
                      "?ids="
                      id))
         (aio-await
          (counsel-spotify-request-p url
                                     :type "PUT"
                                     :parser (lambda () (counsel-spotify--save-current-playback-request-parser added-msg))
                                     :headers (aio-await (counsel-spotify-oauth-bearer-headers-p)))))))

(aio-defun counsel-spotify-get-current-playback-id ()
  (let* ((data (aio-await (counsel-spotify--get-current-playback-data-p)))
         (parsed (counsel-spotify--parse-playback-item data)))
    (alist-get 'id parsed)))
         

(aio-defun counsel-spotify-get-current-playback-state ()
  (aio-await (counsel-spotify-request-p (concat counsel-spotify-spotify-api-url "/me/player")
                                        :type "GET"
                                        :parser #'json-read
                                        :encoding 'binary
                                        :headers (aio-await (counsel-spotify-oauth-bearer-headers-p)))))

(aio-defun counsel-spotify-get-currently-playing ()
  (aio-await (counsel-spotify-request-p (concat counsel-spotify-spotify-api-url "/me/player/currently-playing?additional_types=episode")
                                        :type "GET"
                                        :parser #'json-read
                                        :encoding 'binary
                                        :headers (aio-await (counsel-spotify-oauth-bearer-headers-p)))))

;; show current playback
(aio-defun counsel-spotify-get-current-playback-info ()
  (let* ((currently-playing (aio-await (counsel-spotify-get-currently-playing))))
    (counsel-spotify-oauth2-parse-response currently-playing 'current-playback)))

    
   
    

(provide 'counsel-spotify-playback)
;;; counsel-spotify-playback.el ends here
