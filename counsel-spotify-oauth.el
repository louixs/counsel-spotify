;;; counsel-spotify-oauth.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 John Doe
;;
;; Author: John Doe <https://github.com/george>
;; Maintainer: John Doe <john@doe.com>
;; Created: February 24, 2021
;; Modified: February 24, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/george/counsel-spotify-oauth
;; Package-Requires: ((emacs "24.3") (oauth2 "0.16") (simple-httpd "1.5.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'oauth2)
(require 'json)
(require 'simple-httpd)

;; Moved from counsel-spotify-search
(defcustom counsel-spotify-spotify-api-authentication-url "https://accounts.spotify.com/api/token"
  "Variable to define spotify API url for getting the access token."
  :type 'string :group 'counsel-spotify)

(defcustom counsel-spotify-spotify-api-authorization-url "https://accounts.spotify.com/authorize"
  "Variable to define spotify API url for getting an access token and a refresh token."
  :type 'string :group 'counsel-spotify)

(defcustom counsel-spotify-spotify-api-redirect-url "http://localhost:8080"
  "Variable to define redirect url for retrieving auth token."
  :type 'string :group 'counsel-spotify)

(defcustom counsel-spotify-spotify-api-auth-token ""
  "Auth token data returned by oauth2-auth-and-store function from the oauth2 package."
  :type 'string :group 'counsel-spotify)

(defcustom counsel-spotify-spotify-api-scopes "playlist-read-private playlist-read-collaborative user-read-private user-read-email user-read-currently-playing user-read-playback-state"
  "Variable to define spotify API scopes.
   If adding new feature you may need to add new scope.
   Here is the list of scopes: https://developer.spotify.com/documentation/general/guides/scopes/"
  :type 'string :group 'counsel-spotify)

;; use counsel-spotify-spotify-api-authentication-url as the token-url

;; these variablesx are set in my init for testing purpuses
;; counsel-spotify-client-id
;; counsel-spotify-client-secret
;;

;; oauth2's entry point is oauth2-auth-and-store
;; call this to obtain and store toke nad refresh token

;; http server
(defun start-redirect-server ()
  (setq httpd-root "www/"
        httpd-port "8080")
  (httpd-start))

(defun stop-redirect-server ()
  (print "Stopping web server...")
  (httpd-stop)
  (kill-buffer "*httpd*"))

(defun counsel-spotify-oauth-fetch-token ()
  ""
  (when (not counsel-spotify-spotify-api-auth-token)
    (start-redirect-server)
    (setq
     counsel-spotify-spotify-api-auth-token
     (oauth2-auth-and-store
      counsel-spotify-spotify-api-authorization-url
      counsel-spotify-spotify-api-authentication-url
      counsel-spotify-spotify-api-scopes
      counsel-spotify-client-id
      counsel-spotify-client-secret
      counsel-spotify-spotify-api-redirect-url))
    (stop-redirect-server))
  counsel-spotify-spotify-api-auth-token)

(defun refresh-token ()
  (start-redirect-server)
  (setq
   counsel-spotify-spotify-api-code
   (oauth2-request-authorization
    counsel-spotify-spotify-api-authorization-url
    counsel-spotify-client-id
    counsel-spotify-spotify-api-scopes
    ""
    counsel-spotify-spotify-api-redirect-url))
  (stop-redirect-server)

  (setq
    counsel-spotify-spotify-api-auth-token
    (oauth2-request-access
     counsel-spotify-spotify-api-authentication-url
     counsel-spotify-client-id
     counsel-spotify-client-secret
     counsel-spotify-spotify-api-code
     counsel-spotify-spotify-api-redirect-url))

  (oauth2-refresh-access counsel-spotify-spotify-api-auth-token))

;; fetch token
(counsel-spotify-oauth-fetch-token)
;; It's stored in this variable
;; counsel-spotify-spotify-api-auth-token

;; refesh token if needed
(refresh-token)

;; then retrieve
;; For example, user-data can be retrieved and stored as user-data like this
(defun oauth2-query-results (token url)
  (with-current-buffer
    (oauth2-url-retrieve-synchronously token url)
    (goto-char url-http-end-of-headers)
    (json-read)))

;;(oauth2-query-results counsel-spotify-spotify-api-auth-token "https://api.spotify.com/v1/me")



(provide 'counsel-spotify-oauth)
;;; counsel-spotify-oauth.el ends here
