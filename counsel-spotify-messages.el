;;; counsel-spotify-messages.el --- Show Spotify objects in Ivy and the minibuffer

;; Copyright (C)

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
;; Functions to format different kind of Spotify objects (like tracks, albums, artists, etc.)
;;; Code:
(require 'counsel-spotify-search)

(defun counsel-spotify-format-duration (duration-in-ms)
  (let* ((seconds-of-song (/ duration-in-ms 1000.0))
         (second-left-in-song (% (round seconds-of-song) 60))
         (minutes-in-song (truncate (/ seconds-of-song 60))))
    (format "(%d:%0.2d)"
            minutes-in-song
            second-left-in-song)))

(cl-defgeneric counsel-spotify-format (element)
  "Format an ELEMENT to be shown in the minibuffer.")

(cl-defmethod counsel-spotify-format :around (element)
  "Get the actual Spotify object from the string property of ELEMENT in order to format it according to its type."
  (propertize (decode-coding-string (string-make-unibyte (cl-call-next-method)) 'utf-8)
              'spotify-object element))

(cl-defmethod counsel-spotify-format ((playable counsel-spotify-playable))
  "Format a PLAYABLE Spotify object."
  (name playable))

(cl-defmethod counsel-spotify-format ((track counsel-spotify-track))
  "Format a TRACK Spotify object."
  (let* ((duration (counsel-spotify-format-duration (duration-in-ms track))))
    (format "%s %s - %s [%s]"
            duration
            (name (artist track))
            (name track)
            (name (album track)))))

(cl-defmethod counsel-spotify-format ((album counsel-spotify-album))
  "Format an ALBUM Spotify object."
  (format "%s - %s" (artist-name album) (name album)))

(cl-defmethod counsel-spotify-format ((show counsel-spotify-show))
  "Format a SHOW Spotify object."
  (format "%s | %s" (name show) (publisher show)))

(cl-defmethod counsel-spotify-format ((episode counsel-spotify-episode))
  "Format an EPISODE Spotify object."
  (let* ((duration (counsel-spotify-format-duration (duration-in-ms  episode))))
    (format "%s %s | [%s]" duration (name episode) (description episode))))

(cl-defmethod counsel-spotify-format ((current-playback counsel-spotify-current-playback))
  "Format a PLAYBACK Spotify object."
  (let* ((remaining-time-in-ms (counsel-spotify-format-duration (remaining-time-in-ms current-playback))))
    (format "%s - %s (Artists) - %s (Album) | %s (Remaining)"
            (name current-playback)
            (artist-name current-playback)
            (album current-playback)
            remaining-time-in-ms)))

(cl-defmethod counsel-spotify-format ((current-playback counsel-spotify-current-playback-episode))
  "Format a EPISODE PLAYBACK Spotify object."
  (let* ((remaining-time-in-ms (counsel-spotify-format-duration (remaining-time-in-ms current-playback))))
    (format "%s - %s (Podcast) | %s (Remaining)"
            (name current-playback)
            (show-name current-playback)
            remaining-time-in-ms)))

(provide 'counsel-spotify-messages)
;;; counsel-spotify-messages.el ends here
