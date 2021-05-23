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

;; (defcustom counsel-spotify-spotify-api-authentication-url "https://accounts.spotify.com/api/token"
;;   "Variable to define spotify API url for getting the access token."
;;   :type 'string :group 'counsel-spotify)

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

(defun counsel-spotify-basic-auth-credentials ()
  "Return the Basic auth string that should be sent to ask for an auth token."
  (concat "Basic " (base64-encode-string (concat counsel-spotify-client-id ":" counsel-spotify-client-secret) t)))

(defclass counsel-spotify-playable ()
  ((name :initarg :name :initform "" :reader name)
   (uri :initarg :uri :initform "" :reader uri)))

(defclass counsel-spotify-album (counsel-spotify-playable)
  ((artist-name :initarg :artist-name :initform "" :reader artist-name)))

(defclass counsel-spotify-track (counsel-spotify-playable)
  ((artist :initarg :artist :initform "" :reader artist)
   (album :initarg :album :initform "" :reader album)
   (duration-in-ms :initarg :duration :initform 0 :reader duration-in-ms)))


(cl-defgeneric counsel-spotify-parse-spotify-object (a-spotify-object type)
  "Parse A-SPOTIFY-OBJECT knowing it has the type TYPE.")

(cl-defmethod counsel-spotify-parse-spotify-object (spotify-object _type)
  "Parse a generic SPOTIFY-OBJECT of type _TYPE."
  (let ((name (alist-get 'name spotify-object))
        (uri (alist-get 'uri spotify-object)))
    (make-instance 'counsel-spotify-playable :name name :uri uri)))

(cl-defmethod counsel-spotify-parse-spotify-object (a-spotify-album-object (_type (eql albums)))
  "Parse A-SPOTIFY-ALBUM-OBJECT of _TYPE album."
  (let ((name (alist-get 'name a-spotify-album-object))
        (artist-name (alist-get 'name (elt (alist-get 'artists a-spotify-album-object) 0)))
        (uri (alist-get 'uri a-spotify-album-object)))
    (make-instance 'counsel-spotify-album :name name :uri uri :artist-name artist-name)))

(cl-defmethod counsel-spotify-parse-spotify-object (a-spotify-track-object (_type (eql tracks)))
  "Parse A-SPOTIFY-TRACK-OBJECT of _TYPE track."
  (let ((name (alist-get 'name a-spotify-track-object))
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

;; oauth2
;; This is taken straight from the test file
(defun as-utf8 (a-string)
  (decode-coding-string (string-make-unibyte a-string) 'utf-8))

(defun format-artists-name (artists)
  (->> artists
       (-map (lambda (artist) (->> artist (alist-get 'name) as-utf8)))
       (--reduce (concat acc ", " it))))

(defun get-artist-name (response)
  (->> response
    (alist-get 'item)
    (alist-get 'artists)
    format-artists-name))

(defun get-track-name (response)
  (->> response
    (alist-get 'item)
    (alist-get 'name)
    as-utf8))

(defun get-album-name (response)
  (->> response
    (alist-get 'item)
    (alist-get 'album)
    (alist-get 'name)
    as-utf8))

(defun counsel-spotify-oauth2-format-current-playback-track (a-spotify-alist-response)
  (let* ((artist-name (get-artist-name a-spotify-alist-response))
         (track-name (get-track-name a-spotify-alist-response))
         (album-name (get-album-name a-spotify-alist-response)))
    (concat track-name " - " artist-name " - " album-name " (Album)")))

;; format episodes
(defun get-episode-name (response)
  (->> response
      (alist-get 'item)
      (alist-get 'name)))

(defun get-show-name (response)
  (->> response
       (alist-get 'item)
       (alist-get 'show)
       (alist-get 'name)))

(defun counsel-spotify-oauth2-format-current-playback-episode (a-spotify-alist-response)
  (let* ((show-name (get-show-name a-spotify-alist-response))
         (episode-name (get-episode-name a-spotify-alist-response)))
    (concat show-name " - " episode-name)))

(defun counsel-spotify-oauth2-parse-items (a-spotify-alist-response a-type)
  (let ((items (alist-get 'items a-spotify-alist-response)))
    (mapcar (lambda (item) (counsel-spotify-parse-spotify-object item a-type))
            items)))

(defun counsel-spotify-oauth2-parse-new-releases (response)
  (counsel-spotify-oauth2-parse-items (alist-get 'albums response) 'album))

(cl-defmacro counsel-spotify-with-auth-token ((auth-variable) &body body)
  "Execute with AUTH-VARIABLE bound to the Spotify's auth token for the current user the BODY."
  `(let ((url-request-method "POST")
         (url-request-data "&grant_type=client_credentials")
         (url-request-extra-headers (list (cons "Content-Type" "application/x-www-form-urlencoded")
                                          (cons "Authorization" (counsel-spotify-basic-auth-credentials)))))
     (url-retrieve counsel-spotify-spotify-api-authentication-url
                   (lambda (_status)
                     (goto-char url-http-end-of-headers)
                     (let ((,auth-variable (alist-get 'access_token (json-read))))
                       ,@body)))))

(cl-defmacro counsel-spotify-with-query-results ((auth-token query-url results-variable) &body body)
  "Execute the BODY with the results of an api call to QUERY-URL with an AUTH-TOKEN bound to RESULTS-VARIABLE."
  `(let ((url-request-extra-headers (list (cons "Authorization" (concat "Bearer " ,auth-token)))))
     (url-retrieve ,query-url
                   (lambda (_status)
                     (goto-char url-http-end-of-headers)
                     (let ((,results-variable (json-read)))
                       ,@body)))))

(cl-defun counsel-spotify-make-query (search-term &key type filter)
  "Make a Spotify query to search for TERM of type TYPE with a FILTER."
  (when (null type) (error "Must supply a type of object to search for"))
  (format "%s/search?q=%s&type=%s"
          counsel-spotify-spotify-api-url
          (if filter (format "%s:%s" filter search-term) search-term)
          (mapconcat #'symbol-name type ",")))

(cl-defun counsel-spotify-oauth2-make-query (search-term &key type filter)
  "Make a Spotify query to search for TERM of type TYPE with a FILTER."
  (when (null type) (error "Must supply a type of object to search for"))
  (let ((search-type (mapconcat #'symbol-name type ",")))
    (cond
     ((string-equal search-type "user-playlist") (concat counsel-spotify-spotify-api-url "/me/playlists?limit=50"))
     ((string-equal search-type "current-playback") (concat counsel-spotify-spotify-api-url "/me/player/currently-playing?additional_types=episode"))
     ((string-equal search-type "new-releases") (concat counsel-spotify-spotify-api-url (concat "/browse/new-releases/?country=" counsel-spotify-new-releases-country)))
     ((string-equal search-type "top-artists") (concat counsel-spotify-spotify-api-url "/me/top/artists"))
     ((string-equal search-type "top-tracks") (concat counsel-spotify-spotify-api-url "/me/top/tracks"))
     (t (format "%s/search?q=%s&type=%s"
                counsel-spotify-spotify-api-url
                (if filter (format "%s:%s" filter search-term) search-term)
                search-type)))))

(cl-defun counsel-spotify-search (a-callback &rest rest)
  "Call A-CALLBACK with the parsed result of the query described by REST."
  (let ((query-url (apply #'counsel-spotify-make-query rest)))
    (counsel-spotify-with-auth-token (auth-token)
      (counsel-spotify-with-query-results (auth-token query-url results)
        (funcall a-callback (counsel-spotify-parse-response results))))))

;; oauth2
(defun counsel-spotify-oauth2-parse-response (a-spotify-alist-response category)
  (cond
   ((eq category 'user-playlist) (counsel-spotify-oauth2-parse-items a-spotify-alist-response category))
   ((eq category 'current-playback) (counsel-spotify-oauth2-format-current-playback a-spotify-alist-response))
   ((eq category 'new-releases) (counsel-spotify-oauth2-parse-new-releases a-spotify-alist-response))
   ((eq category 'top-artists) (counsel-spotify-oauth2-parse-items a-spotify-alist-response 'artists))
   ((eq category 'top-tracks) (counsel-spotify-oauth2-parse-items a-spotify-alist-response 'tracks))
   (t (counsel-spotify-parse-response a-spotify-alist-response))))

(defun get-last-element (l)
  (car (car (last l))))

(cl-defun counsel-spotify-oauth2-query-response-synchronously (&rest rest)
  (let* ((query-url (apply #'counsel-spotify-oauth2-make-query rest))
         (token (counsel-spotify-oauth-fetch-token))
         (results (counsel-spotify-oauth2-query-results-synchronously token query-url))
         (category (get-last-element rest)))
    (counsel-spotify-oauth2-parse-response results category)))

(cl-defun counsel-spotify-oauth2-search-synchronously (a-callback &rest rest)
  (let* ((query-url (apply #'counsel-spotify-oauth2-make-query rest))
         (token (counsel-spotify-oauth-fetch-token))
         (results (counsel-spotify-oauth2-query-results-synchronously token query-url))
         (category (get-last-element rest)))
    (funcall a-callback (counsel-spotify-oauth2-parse-response results category))))

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

(aio-defun counsel-spotify-oauth2-search-p (&rest rest)
  (let* ((query-url (apply #'counsel-spotify-oauth2-make-query rest))
         (token (counsel-spotify-oauth-fetch-token))
         (category (get-last-element rest))
         (result (aio-await
                  (counsel-spotify-promisified-oauth2-url-retrieve
                   token
                   query-url))))
    (counsel-spotify-oauth2-parse-response result category)))

(provide 'counsel-spotify-search)
;;; counsel-spotify-search.el ends here
