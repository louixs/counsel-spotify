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
;; Package-Requires: ((emacs "24.3") (oauth2 "0.16") (simple-httpd "1.5.1") (aio "1.0"))
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
(require 'aio)

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

(defcustom counsel-spotify-spotify-api-auth-token nil
  "Auth token data returned by oauth2-auth-and-store function from the oauth2 package."
  :type 'string :group 'counsel-spotify)

(defcustom counsel-spotify-spotify-api-scopes "playlist-read-private playlist-read-collaborative user-read-private user-read-email user-read-currently-playing user-read-playback-state user-library-modify user-top-read"
  "Variable to define spotify API scopes.
   If adding new feature you may need to add new scope.
   Here is the list of scopes: https://developer.spotify.com/documentation/general/guides/scopes/"
  :type 'string :group 'counsel-spotify)

;; http server
(defun start-redirect-server ()
  (setq httpd-root "www/"
        httpd-port "8080")
  (httpd-start))

(defun stop-redirect-server ()
  (print "Stopping web server...")
  (httpd-stop)
  (when-let ((httpd-buffer (get-buffer "*httpd*")))
    (print "Also killing the httpd buffer...")
    (kill-buffer "*httpd*")))

(defun counsel-spotify-oauth-fetch-token ()
  ""
  (when (eq counsel-spotify-spotify-api-auth-token nil)
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

(defun old--counsel-spotify-refresh-token ()
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

(defun counsel-spotify-refresh-oauth-token ()
  (interactive)
  (oauth2-refresh-access
   (counsel-spotify-oauth-fetch-token)))

(defun counsel-spotify-reset-oauth-token ()
  "Lets you re-do the authentication and re-fetch auth code from Spotify API in case
   something goes awry. It assumes that you haven't changed the default place where oauth2.plstore
   is placed."
  (interactive)
  (delete-file (concat user-emacs-directory "oauth2.plstore"))
  (setq counsel-spotify-spotify-api-auth-token nil)
  (counsel-spotify-refresh-oauth-token))

;; For example, user-data can be retrieved and stored as user-data like this
(defun counsel-spotify-oauth2-query-results-synchronously (token url &optional request-method request-data)
  (with-current-buffer
    (oauth2-url-retrieve-synchronously token url request-method request-data)
    (goto-char url-http-end-of-headers)
    (json-read)))

(defun counsel-spotify-oauth2-query-results (token url cb &optional request-method request-data)
  (oauth2-url-retrieve token url
                       (lambda (_status)
                         (goto-char url-http-end-of-headers)
                         (let ((results (json-read)))
                           (funcall cb results)))
                       nil
                       request-method
                       request-data))

(defun counsel-spotify-promisified-oauth2-url-retrieve (token url &optional request-method request-data)
  (let ((promise (aio-promise)))
    (prog1 promise
      (condition-case error
          (oauth2-url-retrieve token url
                               (lambda (status)
                                 (goto-char url-http-end-of-headers)
                                 (let ((results (json-read)))
                                   (aio-resolve promise (lambda () results))))
                               nil
                               request-method
                               request-data)
        (error (aio-resolve promise
                            (lambda ()
                              (signal (car error) (cdr error)))))))))

(provide 'counsel-spotify-oauth)
;;; counsel-spotify-oauth.el ends here
