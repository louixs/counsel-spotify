;;; counsel-spotify-search.el --- Search things through the Spotify Search API -*- lexical-binding: t; -*-

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
;; In this file there are functions to search things using Spotify Search API
;; and decoders for all the different types of things that Spotify can respond back
;;; Code:

(require 'json)
(require 'url)
(require 'counsel-spotify-oauth)

(defcustom counsel-spotify-spotify-api-url "https://api.spotify.com/v1"
  "Variable to define spotify API url."
  :type 'string :group 'counsel-spotify)

(defcustom counsel-spotify-new-releases-country "GB"
  "Specify country for new releases in ISO 3166-1 alpha-2 country code."
  :type 'string :group 'counsel-spotify)

(defcustom counsel-spotify-client-id ""
  "Spotify application client ID."
  :type 'string :group 'counsel-spotify)

(defcustom counsel-spotify-client-secret ""
  "Spotify application client secret."
  :type 'string :group 'counsel-spotify)

(defun counsel-spotify-verify-credentials ()
  "Tell the user that the credentials are not set."
  (when (or (string= counsel-spotify-client-id "") (string= counsel-spotify-client-secret ""))
    (error "The variables counsel-spotify-client-id or counsel-spotify-client-secret are undefined and both are required to authenticate to the Spotify API.  See https://developer.spotify.com/my-applications")))

(defclass counsel-spotify-playable ()
  ((name :initarg :name :initform "" :reader name)
   (uri :initarg :uri :initform "" :reader uri)))

(defclass counsel-spotify-non-playable ()
  ((name :initarg :name :initform "" :reader name)))

(defclass counsel-spotify-album (counsel-spotify-playable)
  ((artist-name :initarg :artist-name :initform "" :reader artist-name)))

(defclass counsel-spotify-track (counsel-spotify-playable)
  ((artist :initarg :artist :initform "" :reader artist)
   (album :initarg :album :initform "" :reader album)
   (duration-in-ms :initarg :duration :initform 0 :reader duration-in-ms)))

(defclass counsel-spotify-show (counsel-spotify-playable)
  ((publisher :initarg :publisher :initform "" :reader publisher)))

(defclass counsel-spotify-episode (counsel-spotify-playable)
  ((description :initarg :description :initform "" :reader description)
   (duration-in-ms :initarg :duration :initform 0 :reader duration-in-ms)))

(defclass counsel-spotify-current-playback (counsel-spotify-non-playable)
  ((artist-name :initarg :artist-name :initform "" :reader artist-name)
   (album :initarg :album :initform "" :reader album)
   (duration-in-ms :initarg :duration-in-ms :initform 0 :reader duration-in-ms)
   (progress-in-ms :initarg :progress-in-ms :initform 0 :reader progress-in-ms)))
   

(defclass counsel-spotify-current-playback-episode (counsel-spotify-non-playable)
  ((show-name :initarg :show-name :initform "" :reader show-name)
   (episode-description :initarg :episode-description :initform "" :reader episode-description)
   (duration-in-ms :initarg :duration-in-ms :initform 0 :reader duration-in-ms)
   (progress-in-ms :initarg :progress-in-ms :initform 0 :reader progress-in-ms)))

(cl-defgeneric counsel-spotify-parse-spotify-object (a-spotify-object type)
  "Parse A-SPOTIFY-OBJECT knowing it has the type TYPE.")

(cl-defmethod counsel-spotify-parse-spotify-object (a-spotify-object _type)
  "Parse a generic SPOTIFY-OBJECT of type _TYPE."
  (let* ((name (alist-get 'name a-spotify-object))
         (uri (alist-get 'uri a-spotify-object)))
    (make-instance 'counsel-spotify-playable :name name :uri uri)))

(cl-defmethod counsel-spotify-parse-spotify-object (a-spotify-album-object (_type (eql albums)))
  "Parse A-SPOTIFY-ALBUM-OBJECT of _TYPE album."
  (let* ((name (alist-get 'name a-spotify-album-object))
         (artist-name (alist-get 'name (elt (alist-get 'artists a-spotify-album-object) 0)))
         (uri (alist-get 'uri a-spotify-album-object)))
    (make-instance 'counsel-spotify-album :name name :uri uri :artist-name artist-name)))

(cl-defmethod counsel-spotify-parse-spotify-object (a-spotify-show-object (_type (eql shows)))
  "Parse A-SPOTIFY-SHOW-OBJECT of _TYPE shows."
  (let* ((name (alist-get 'name a-spotify-show-object))
         (publisher (alist-get 'publisher a-spotify-show-object))
         (uri (alist-get 'uri a-spotify-show-object)))
    (make-instance 'counsel-spotify-show :name name :uri uri :publisher publisher)))

(cl-defmethod counsel-spotify-parse-spotify-object (a-spotify-episode-object (_type (eql episodes)))
  "Parse A-SPOTIFY-EPISODE-OBJECT of _TYPE episodes."
  (let* ((name (alist-get 'name a-spotify-episode-object))
         (description (alist-get 'description a-spotify-episode-object))
         (uri (alist-get 'uri a-spotify-episode-object))
         (duration-in-ms (alist-get 'duration_ms a-spotify-episode-object)))
    (make-instance 'counsel-spotify-episode
                   :name name
                   :uri uri
                   :description description
                   :duration duration-in-ms)))

(cl-defmethod counsel-spotify-parse-spotify-object (a-spotify-track-object (_type (eql tracks)))
  "Parse A-SPOTIFY-TRACK-OBJECT of _TYPE track."
  (let* ((name (alist-get 'name a-spotify-track-object))
         (uri (alist-get 'uri a-spotify-track-object))
         (duration-in-ms (alist-get 'duration_ms a-spotify-track-object))
         (main-artist (counsel-spotify-parse-spotify-object (elt (alist-get 'artists a-spotify-track-object) 0) 'artists))
         (album (counsel-spotify-parse-spotify-object (alist-get 'album a-spotify-track-object) 'albums)))
    (make-instance 'counsel-spotify-track
                   :name name
                   :uri uri
                   :artist main-artist
                   :duration duration-in-ms
                   :album album)))

(defun format-artists-name (artists)
  (->> artists
       (-map (lambda (artist) (->> artist (alist-get 'name))))
       (--reduce (concat acc ", " it))))

(defun get-artist-name (item)
  (->> item
    (alist-get 'artists)
    format-artists-name))

(defun get-track-name (item)
  (->> item
    (alist-get 'name)))

(defun get-album-name (item)
  (->> item
    (alist-get 'album)
    (alist-get 'name)))

;; for displaying information about current tunes that is not podcast/show episodes
(cl-defmethod counsel-spotify-parse-spotify-object (a-spotify-current-playback-object (_type (eql current-playback)))
  "Parse a A-SPOTIFY-CURRENT-PLAYBACK-OBJECT of type _TYPE current-playback"
  (let* ((item (alist-get 'item a-spotify-current-playback-object))
         (progress-in-ms (alist-get 'progress_ms a-spotify-current-playback-object))
         (duration-in-ms (alist-get 'duration_ms item))
         (playback-name (alist-get 'name item))
         (artist-name (get-artist-name item))
         (album-name (get-album-name item)))
    (make-instance 'counsel-spotify-current-playback
                   :name playback-name
                   :artist-name artist-name
                   :album album-name
                   :duration-in-ms duration-in-ms
                   :progress-in-ms progress-in-ms)))

(defun get-episode-name (item)
  (->> item
       (alist-get 'name)))

(defun get-episode-description (item)
  (->> item
       (alist-get 'description)))

(defun get-show-name (item)
  (->> item
       (alist-get 'show)
       (alist-get 'name)))

(cl-defmethod counsel-spotify-parse-spotify-object (a-spotify-current-playback-object (_type (eql current-playback-episode)))
  "Parse a A-SPOTIFY-CURRENT-PLAYBACK-OBJECT of type _TYPE current-playback-episode i.e. podcast episodes "
  (let* ((item (alist-get 'item a-spotify-current-playback-object))
         (progress-in-ms (alist-get 'progress_ms a-spotify-current-playback-object))
         (duration-in-ms (alist-get 'duration_ms item))
         (episode-name (get-episode-name item))
         (show-name (get-show-name item))
         (episode-description (get-episode-description item)))
    (make-instance 'counsel-spotify-current-playback-episode
                   :name episode-name
                   :show-name show-name
                   :episode-description episode-description
                   :duration-in-ms duration-in-ms
                   :progress-in-ms progress-in-ms)))

(defun counsel-spotify-parse-items (a-spotify-alist-response a-type)
  "Parse every item in A-SPOTIFY-ALIST-RESPONSE as being of the type A-TYPE."
  (let ((items (alist-get 'items (alist-get a-type a-spotify-alist-response))))
    (mapcar (lambda (item) (counsel-spotify-parse-spotify-object item a-type))
            items)))

(defun counsel-spotify-parse-response (a-spotify-alist-response)
  "Parse A-SPOTIFY-ALIST-RESPONSE iterating through every category."
  (cl-mapcan
   (lambda (category)
     (counsel-spotify-parse-items a-spotify-alist-response  (car category)))
   a-spotify-alist-response))

(defun counsel-spotify-oauth2-parse-items (a-spotify-alist-response a-type)
  (let ((items (alist-get 'items a-spotify-alist-response)))
    (mapcar (lambda (item)
              (counsel-spotify-parse-spotify-object item a-type))
            items)))

(defun counsel-spotify-oauth2-parse-shows (a-spotify-alist-response a-type)
  (let ((response (alist-get a-type a-spotify-alist-response)))
    (counsel-spotify-oauth2-parse-items response a-type)))

(defun counsel-spotify-oauth2-parse-episodes (a-spotify-alist-response a-type)
  (let ((response (alist-get a-type a-spotify-alist-response)))
    (counsel-spotify-oauth2-parse-items response a-type)))

(defun counsel-spotify-oauth2-parse-new-releases (response)
  (counsel-spotify-oauth2-parse-items (alist-get 'albums response) 'album))

(cl-defun counsel-spotify-oauth2-make-query (search-term &key type filter)
  "Make a Spotify query to search for TERM of type TYPE with a FILTER."
  (when (null type) (error "Must supply a type of object to search for"))
  (let* ((search-type (mapconcat #'symbol-name type ","))
         (url-safe-search-term (url-encode-url search-term)))
    (cond
     ((string-equal search-type "user-playlist") (concat counsel-spotify-spotify-api-url "/me/playlists?limit=50"))
     ((string-equal search-type "current-playback") (concat counsel-spotify-spotify-api-url "/me/player/currently-playing?additional_types=track,episode"))
     ((string-equal search-type "new-releases") (concat counsel-spotify-spotify-api-url (concat "/browse/new-releases/?country=" counsel-spotify-new-releases-country)))
     ((string-equal search-type "top-artists") (concat counsel-spotify-spotify-api-url "/me/top/artists"))
     ((string-equal search-type "top-tracks") (concat counsel-spotify-spotify-api-url "/me/top/tracks"))
     (t (format "%s/search?q=%s&type=%s"
                counsel-spotify-spotify-api-url
                (if filter (format "%s:%s" filter url-safe-search-term) url-safe-search-term)
                search-type)))))

(defun counsel-spotify-get-playback-type (response)
  (->> response
       (alist-get 'item)
       (alist-get 'type)))

(defun counsel-spotify-playback-type? (type response)
  (string= type (counsel-spotify-get-playback-type response)))

(defun playback-episode? (a-spotify-alist-response category)
  "Check if current playback is an episode"
  (and (eq category 'current-playback)
       (counsel-spotify-playback-type? "episode" a-spotify-alist-response)))

;; oauth2
(defun counsel-spotify-oauth2-parse-response (a-spotify-alist-response category)
  (cond
   ((eq category 'user-playlist) (counsel-spotify-oauth2-parse-items a-spotify-alist-response category))
   ;; data structure for playback is different if they are podcast episode
   ((playback-episode? a-spotify-alist-response category)
    (counsel-spotify-parse-spotify-object a-spotify-alist-response 'current-playback-episode))
   ((eq category 'current-playback) (counsel-spotify-parse-spotify-object a-spotify-alist-response 'current-playback))
   ((eq category 'new-releases) (counsel-spotify-oauth2-parse-new-releases a-spotify-alist-response))
   ((eq category 'top-artists) (counsel-spotify-oauth2-parse-items a-spotify-alist-response 'artists))
   ((eq category 'top-tracks) (counsel-spotify-oauth2-parse-items a-spotify-alist-response 'tracks))
   ((eq category 'show) (counsel-spotify-oauth2-parse-shows a-spotify-alist-response 'shows))
   ((eq category 'episode) (counsel-spotify-oauth2-parse-episodes a-spotify-alist-response 'episodes))
   (t (counsel-spotify-parse-response a-spotify-alist-response))))

(defun get-last-element (l)
  (car (car (last l))))

(cl-defun counsel-spotify-oauth2-search (a-callback &rest rest)
  (let* ((query-url (apply #'counsel-spotify-oauth2-make-query rest))
         (token (counsel-spotify-oauth-fetch-token))
         (category (get-last-element rest)))
    (counsel-spotify-oauth2-query-results
     token
     query-url
     (lambda (results)
       (let ((parsed (counsel-spotify-oauth2-parse-response results category)))
         (funcall a-callback parsed))))))

(defun counsel-spotify-oauth2-api-error (result)
  "If API returns error in its response, this function returns its error status.
   For example, error status 401 means Invalid Auth token. You need to refresh auth token to solve this."
  (when-let ((err (alist-get 'error result)))
    (let ((status (alist-get 'status err))
          (error-msg (alist-get 'message err)))
      status)))

(aio-defun counsel-spotify-oauth2-search-p (&rest rest)
  (let* ((query-url (apply #'counsel-spotify-oauth2-make-query rest))
         (category (get-last-element rest))
         (result (aio-await (counsel-spotify-request-p query-url
                                                       :type "GET"
                                                       :encoding 'binary ;; temporal workaround to make special chars display work
                                                       :headers (aio-await (counsel-spotify-oauth-bearer-headers-p))))))
    (counsel-spotify-oauth2-parse-response result category)))

(provide 'counsel-spotify-search)
;;; counsel-spotify-search.el ends here
