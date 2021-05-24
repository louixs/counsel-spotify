;;; counsel-spotify.el --- Control Spotify search and select music with Ivy -*- lexical-binding: t; -*-

;; Copyright (C)
;; Author: Lautaro García <https://github.com/Lautaro-Garcia>
;; URL: https://github.com/Lautaro-Garcia/counsel-spotify
;; Package: counsel-spotify
;; Package-Requires: ((emacs "25.1") (ivy "0.13.0"))
;; Version: 0.5.2

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Makes it easier to browse Spotify API from Emacs.
;;; Code:

(require 'ivy)
(require 'counsel-spotify-search)
(require 'counsel-spotify-backends)
(require 'counsel-spotify-notifications)
(require 'counsel-spotify-messages)
(require 'counsel-spotify-tracks)

(defgroup  counsel-spotify nil
  "Customs for `counsel-spotify'"
  :group 'applications)


;;;;;;;;;;;;;;;;;
;; Controllers ;;
;;;;;;;;;;;;;;;;;

;;;###autoload
(defun counsel-spotify-play ()
  "Start playing current track."
  (interactive)
  (counsel-spotify-tell-backend-to counsel-spotify-current-backend #'play))

;;;###autoload
(defun counsel-spotify-toggle-play-pause ()
  "Toggle play or pause of the current track."
  (interactive)
  (counsel-spotify-tell-backend-to counsel-spotify-current-backend #'playpause))


;;;###autoload
(defun counsel-spotify-previous ()
  "Start playing previous song."
  (interactive)
  (counsel-spotify-tell-backend-to counsel-spotify-current-backend #'previous))

;;;###autoload
(defun counsel-spotify-next ()
  "Start playing next song."
  (interactive)
  (counsel-spotify-tell-backend-to counsel-spotify-current-backend #'next))

;;;###autoload
(defun counsel-spotify-toggle-repeat ()
  "Toggle repeat"
  (interactive)
  (counsel-spotify-tell-backend-to-toggle counsel-spotify-current-backend #'toggle-repeat))

;;;###autoload
(defun counsel-spotify-toggle-shuffle ()
  "Toggle shuffle"
  (interactive)
  (counsel-spotify-tell-backend-to-toggle counsel-spotify-current-backend #'toggle-shuffle))

;;;;;;;;;;;;;;;;;;;
;; Ivy interface ;;
;;;;;;;;;;;;;;;;;;;

(defun counsel-spotify-update-ivy-candidates (list-of-counsel-spotify-objects)
  "Tell Ivy to update the minibuffer candidates with the LIST-OF-COUNSEL-SPOTIFY-OBJECTS."
  (ivy-update-candidates (mapcar #'counsel-spotify-format list-of-counsel-spotify-objects)))

(defmacro counsel-spotify-search-by (&rest search-args)
  "Create the function to search by SEARCH-KEYWORD and other SEARCH-ARGS."
  `(lambda (search-term)
     (counsel-spotify-search #'counsel-spotify-update-ivy-candidates search-term ,@search-args)
     0))

;; oauth2
(defmacro counsel-spotify-oauth2-search-by (&rest search-args)
  `(lambda (search-term)
     (counsel-spotify-oauth2-search #'counsel-spotify-update-ivy-candidates search-term ,@search-args)
     0))

(aio-defun counsel-spotify-oauth2-fetch-by-type (type)
  (mapcar #'counsel-spotify-format
          (aio-await (counsel-spotify-oauth2-search-p "" :type type))))

(defmacro counsel-spotify-oauth2-search-synchronously-by (&rest search-args)
  "Create the function to search by SEARCH-KEYWORD and other SEARCH-ARGS."
  `(lambda (search-term)
     (counsel-spotify-oauth2-search-synchronously #'counsel-spotify-update-ivy-candidates search-term ,@search-args)
     0))

(defmacro counsel-spotify-oauth2-query-response-synchronously-by (&rest search-args)
  "Create the function to search by SEARCH-KEYWORD and other SEARCH-ARGS."
  `(lambda (search-term)
     (counsel-spotify-oauth2-query-response-synchronously search-term ,@search-args)
     0))

;;;###autoload
(defun counsel-spotify-search-track ()
  "Bring Ivy frontend to choose and play a track."
  (interactive)
  (counsel-spotify-verify-credentials)
  (ivy-read "Search track: " (counsel-spotify-search-by :type '(track))
            :dynamic-collection t
            :action '(1
                      ("p" counsel-spotify-play-string "Play track")
                      ("a" (lambda (elem) (counsel-spotify-do-play counsel-spotify-current-backend (album (counsel-spotify-unwrap-spotify-object elem)))) "Play album")
                      ("A" (lambda (elem) (counsel-spotify-do-play counsel-spotify-current-backend (artist (counsel-spotify-unwrap-spotify-object elem)))) "Play artist"))))

;;;###autoload
(defun counsel-spotify-search-artist ()
  "Bring Ivy frontend to choose and play an artist."
  (interactive)
  (counsel-spotify-verify-credentials)
  (ivy-read "Search artist: " (counsel-spotify-search-by :type '(artist)) :dynamic-collection t :action #'counsel-spotify-play-string))

;;;###autoload
(defun counsel-spotify-search-playlist ()
  "Bring Ivy frontend to choose and play a playlist."
  (interactive)
  (counsel-spotify-verify-credentials)
  (ivy-read "Search playlist: " (counsel-spotify-search-by :type '(playlist)) :dynamic-collection t :action #'counsel-spotify-play-string))

;;;###autoload
(aio-defun counsel-spotify-search-user-playlist ()
  "Bring Ivy frontend to choose and play a playlist from your current user.
   dynamic-collection is not turned on as the spotify API returns a list of playlist.
   There is no search API that dynamically updates the result."
  (interactive)
  (counsel-spotify-verify-credentials)
  ;; TODO: If there are more than 20 playlists, you need to paginate
  ;; by suppliyng offset to get the remaining playlists
  ;; also because we get the list and this is not a search
  ;; we don't need to call the API everytime we enter the search-term
  (ivy-read "Search user playlist: "
            (aio-await (counsel-spotify-oauth2-fetch-by-type '(user-playlist)))
            :action #'counsel-spotify-play-string))


;;;###autoload
(defun counsel-spotify-new-releases ()
  "Show new releases"
  (interactive)
  (counsel-spotify-verify-credentials)
  (ivy-read "Search new releases: "
            (counsel-spotify-oauth2-fetch-by-type '(new-releases))
            :action #'counsel-spotify-play-string))

;;;###autoload
(defun counsel-spotify-top-artists ()
  "Show user's top artists"
  (interactive)
  (counsel-spotify-verify-credentials)
  (ivy-read "Top artists: "
            (counsel-spotify-oauth2-fetch-by-type '(top-artists))
            :action #'counsel-spotify-play-string))

;;;###autoload
(defun counsel-spotify-top-tracks ()
  "Show user's top tracks"
  (interactive)
  (counsel-spotify-verify-credentials)
  (ivy-read "Top tracks: "
            (counsel-spotify-oauth2-fetch-by-type '(top-tracks))
            :action #'counsel-spotify-play-string))

;;;###autoload
(defun counsel-spotify-show-current-track ()
  "Show information about currently playing track."
  (interactive)
  (counsel-spotify-verify-credentials)
  (counsel-spotify-oauth2-search (lambda (data) (message data)) "" :type '(current-playback)))

;;;###autoload
(defun counsel-spotify-search-album ()
  "Bring Ivy frontend to choose and play an album."
  (interactive)
  (counsel-spotify-verify-credentials)
  (ivy-read "Search album: " (counsel-spotify-search-by :type '(album)) :dynamic-collection t :action #'counsel-spotify-play-string))

;;;###autoload
(defun counsel-spotify-search-tracks-by-artist ()
  "Bring Ivy frontend to search for all tracks for a given artist."
  (interactive)
  (counsel-spotify-verify-credentials)
  (ivy-read "Search tracks by artist: " (counsel-spotify-search-by :filter 'artist :type '(track)) :dynamic-collection t :action #'counsel-spotify-play-string))

;;;###autoload
(defun counsel-spotify-search-tracks-by-album ()
  "Bring Ivy frontend to search for all track on a given album."
  (interactive)
  (counsel-spotify-verify-credentials)
  (ivy-read "Search tracks by album: " (counsel-spotify-search-by :filter 'album :type '(track)) :dynamic-collection t :action #'counsel-spotify-play-string))

;;;###autoload
(defun counsel-spotify-search-show ()
  "Bring Ivy frontend to choose and play a (podcast) show"
  (interactive)
  (counsel-spotify-verify-credentials)
  (ivy-read "Search show: " (counsel-spotify-oauth2-search-by :type '(show)) :dynamic-collection t :action #'counsel-spotify-play-string))

;;;###autoload
(defun counsel-spotify-search-episode ()
  "Bring Ivy frontend to choose and play an (podcast) episode"
  (interactive)
  (counsel-spotify-verify-credentials)
  (ivy-read "Search episode: " (counsel-spotify-oauth2-search-by :type '(episode)) :dynamic-collection t :action #'counsel-spotify-play-string))

;;;###autoload
(defun counsel-spotify-save-current-track ()
  "Using player API to retrieve the currently playing track's ID.
   By PUT:ting the retrieved ID to the tracks API, this saves the currently playing track to user's 'Your Music' library.
   Basically same as clicking on the heart/like symbol by the tracks. The saved/liked track are stored in the Liked Songs playlist on Spotify."
  (interactive)
  (counsel-spotify--save-current-track-from-id
   (counsel-spotify--get-current-track-id)))

(provide 'counsel-spotify)
;;; counsel-spotify.el ends here
