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

(defcustom counsel-spotify-spotify-api-redirect-url "http://localhost:8080/counsel-spotify-oauth"
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
  (message "Stopping web server...")
  (httpd-stop)
  (when-let ((httpd-buffer (get-buffer "*httpd*")))
    (message "Also killing the httpd buffer...")
    (kill-buffer "*httpd*")))

(defun counsel-spotify-oauth2-request-authorization-p (auth-url client-id &optional redirect-uri scope state)
  "Promisified auth request. The implementaiton is largely based on aio-url-retrieve.

   Use it like this:
  (aio-defun fetch ()
   (let* ((res (aio-await
                (counsel-spotify-oauth2-request-p counsel-spotify-spotify-api-authorization-url
                                                  counsel-spotify-client-id
                                                  counsel-spotify-spotify-api-redirect-url
                                                  counsel-spotify-spotify-api-scopes))))
    (message \"resutlt: %s\" res)))

   ;; If outside of aio-defun, you can use this to wait for the result to return
   (aio-await-for (fetch))
   "
  (start-redirect-server)
  (let ((promise (aio-promise)))
    (prog1 promise
      (defservlet* counsel-spotify-oauth text/html (code)
        (when code
          (insert "<p> Connected. Return to emacs</p> <script type='text/javascript'>setTimeout(function() {close()}, 1500);</script>")
          (message "stopping the server")
          (stop-redirect-server)
          (aio-resolve promise
                       (lambda ()
                         code))))

      (browse-url (concat auth-url
                          (if (string-match-p "\?" auth-url) "&" "?")
                          "client_id=" (url-hexify-string client-id)
                          "&response_type=code"
                          "&redirect_uri=" (url-hexify-string (or redirect-uri "urn:ietf:wg:oauth:2.0:oob"))
                          (if scope (concat "&scope=" (url-hexify-string scope)) "")
                          (if state (concat "&state=" (url-hexify-string state)) ""))))))

(aio-defun counsel-spotify-oauth2-auth-p (auth-url token-url client-id client-secret &optional scope state redirect-uri)
  (let ((auth-code (aio-await  (counsel-spotify-oauth2-request-authorization-p
                                auth-url client-id redirect-uri scope state))))
    (oauth2-request-access
     token-url
     client-id
     client-secret
     auth-code
     redirect-uri)))

;; temporal
(aio-defun counsel-spotify-oauth2-auth-and-store-p (auth-url token-url scope client-id client-secret &optional redirect-uri state)
  "Request access to a resource and store it using `plstore'."
  ;; We store a MD5 sum of all URL
  (let* ((plstore (plstore-open oauth2-token-file))
         (id (oauth2-compute-id auth-url token-url scope))
         (plist (cdr (plstore-get plstore id))))
    ;; Check if we found something matching this access
    (if plist
        ;; We did, return the token object
        (make-oauth2-token :plstore plstore
                           :plstore-id id
                           :client-id client-id
                           :client-secret client-secret
                           :access-token (plist-get plist :access-token)
                           :refresh-token (plist-get plist :refresh-token)
                           :token-url token-url
                           :access-response (plist-get plist :access-response))
      (let ((token (aio-await (counsel-spotify-oauth2-auth auth-url token-url
                                                           client-id client-secret scope state redirect-uri))))
        ;; Set the plstore
        (setf (oauth2-token-plstore token) plstore)
        (setf (oauth2-token-plstore-id token) id)
        (plstore-put plstore id nil `(:access-token
                                      ,(oauth2-token-access-token token)
                                      :refresh-token
                                      ,(oauth2-token-refresh-token token)
                                      :access-response
                                      ,(oauth2-token-access-response token)))
        (plstore-save plstore)
        token))))

(aio-defun counsel-spotify-oauth-fetch-token-p ()
  (let ((token (aio-await (counsel-spotify-oauth2-auth-and-store-p counsel-spotify-spotify-api-authorization-url
                                                                   counsel-spotify-spotify-api-authentication-url
                                                                   counsel-spotify-spotify-api-scopes
                                                                   counsel-spotify-client-id
                                                                   counsel-spotify-client-secret
                                                                   counsel-spotify-spotify-api-redirect-url))))
    (setq counsel-spotify-spotify-api-auth-token token)
    token))

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

(aio-defun counsel-spotify-refresh-oauth-token ()
  (interactive)
  (message "Refreshing oauth token.")
  (oauth2-refresh-access
   (aio-await (counsel-spotify-oauth-fetch-token-p)))
  (message "Finished refreshing oauth token."))

(defun counsel-spotify-reset-oauth-token ()
  "Lets you re-do the authentication and re-fetch auth code from Spotify API in case
   something goes awry. It assumes that you haven't changed the default place where oauth2.plstore
   is placed."
  (interactive)
  (message "Resetting oauth token")
  (delete-file (concat user-emacs-directory "oauth2.plstore"))
  (setq counsel-spotify-spotify-api-auth-token nil)
  (counsel-spotify-refresh-oauth-token)
  (message "Finished resetting token"))

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
