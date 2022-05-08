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
;; Package-Requires: ((emacs "24.3") (oauth2 "0.16") (simple-httpd "1.5.1") (aio "1.0") (request "0.3.2"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'cl-lib)
(require 'oauth2)
(require 'json)
(require 'simple-httpd)
(require 'aio)
(require 'request)

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

(aio-defun counsel-spotify-oauth2-auth-bearer-p ()
  (let* ((token (aio-await (counsel-spotify-oauth-fetch-token-pkce-p))))
    `("Authorization" . ,(concat "Bearer " (oauth2-token-access-token token)))))

(aio-defun counsel-spotify-oauth-bearer-headers-p ()
  (let* ((bearer (aio-await (counsel-spotify-oauth2-auth-bearer-p))))
    `(("Content-Type" . "application/json")
      ,bearer)))

;; moved from counsel-spotify-search
(defun counsel-spotify-basic-auth-credentials ()
  "Return the Basic auth string that should be sent to ask for an auth token."
  (concat "Basic " (base64-encode-string (concat counsel-spotify-client-id ":" counsel-spotify-client-secret) t)))

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
  (let ((auth-code (aio-await (counsel-spotify-oauth2-request-authorization-p auth-url client-id redirect-uri scope state))))
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
      (let ((token (aio-await (counsel-spotify-oauth2-auth-p auth-url token-url
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

;; PKCE
;; Proof Key for Code Exchange (PKCE)
;; Pick the the character length for the random string
(defun counsel-spotify-oauth--random-char-length (start end)
  (let* ((allowed-char-lengths  (number-sequence start end))
         (allowed-char-lenghts-count (length allowed-char-lengths))
         (ind (% (random) allowed-char-lenghts-count)))
    (nth ind allowed-char-lengths)))

(defun counsel-spotify-oauth--pkce-random-char-length ()
  (counsel-spotify-oauth--random-char-length 43 128))

;; Construct a random string of random length chosen above
;; it can contain letters, digits, understcores, periods hyphens, or tildes
(defun counsel-spotify-oauth--get-random-char ()
  (let* ((allowed-chars "abcdefghijklmnopqrstuvwxyz1234567890_.-~")
         (allowed-chars-length (length allowed-chars))
         (ind (% (abs (random)) allowed-chars-length)))
    (substring allowed-chars ind (+ ind 1))))

(defun counsel-spotify-oauth--concat-random-str-times (times)
  (if (< times 1)
    ""
    (concat (counsel-spotify-oauth--get-random-char) (counsel-spotify-oauth--concat-random-str-times (- times 1)))))

(defun counsel-spotify-oauth--generate-code-verifier ()
  (counsel-spotify-oauth--concat-random-str-times (counsel-spotify-oauth--pkce-random-char-length)))

(defun counsel-spotify-oauth--generate-code-challenge (code-verifier)
  (secure-hash 'sha256 code-verifier nil nil 'binary))

(defun counsel-spotify-oauth2--request-pkce-authorization-p (auth-url client-id code-verifier redirect-uri &optional scope state)
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
                          "&code_challenge_method=S256"
                          (concat "&code_challenge=" (base64url-encode-string
                                                      (counsel-spotify-oauth--generate-code-challenge code-verifier)
                                                      'no-pad))
                          (if scope (concat "&scope=" (url-hexify-string scope)) "")
                          (if state (concat "&state=" (url-hexify-string state)) ""))))))

(cl-defun -counsel-spotify-request-p (url
                                      &key
                                      data
                                      type
                                      headers
                                      (parser #'json-read))
  "Make a non-blocking request to URL.
  Returns an aio-promise."
 (lexical-let* ((promise (aio-promise)))
   (prog1 promise
    (request url
      :type type
      :headers headers
      :data data
      :parser parser
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (aio-resolve promise (lambda () data))))
      :error (cl-function
              (lambda (&rest args &key error-thrown &allow-other-keys)
               (if (consp error-thrown)
                 (aio-resolve promise (lambda () (car (cdr (cdr error-thrown)))))
                 (aio-resolve promise (lambda () error-thrown)))))))))

(aio-defun counsel-spotify-request-p (&rest rest)
  (let* ((result (aio-await (apply #'-counsel-spotify-request-p rest))))
    (if (eq result 401)
      (progn
        (message "Got 401.")
        (aio-await (counsel-spotify-refresh-oauth-token-pkce))
        (counsel-spotify-oauth2-parse-responsen
         (aio-await (--counsel-spotify-request-p rest))))
      result)))

(cl-defun counsel-spotify-request-p-original (url
                                              &key
                                              data
                                              type
                                              headers
                                              (parser #'json-read))
  "Make a non-blocking request to URL.
  Returns an aio-promise."
 (let ((promise (aio-promise)))
   (prog1 promise
     (condition-case error
       (request url
         :type type
         :headers headers
         :data data
         :parser parser
         :success (cl-function
                   (lambda (&key data &allow-other-keys)
                     (aio-resolve promise (lambda () data))))
         :error (cl-function
                 (lambda (&rest args &key error &allow-other-keys)
                   (signal (car error) (cdr error)))))
       (error (aio-resolve promise
                           (lambda ()
                             (signal (car error) (cdr error)))))))))


(defun counsel-spotify-oauth2-make-access-request (url data)
  "Make a non-blocking access request to URL using DATA in POST.
  Returns aio-promise."
  (counsel-spotify-request-p url
                             :type "POST"
                             :headers `(("Content-Type" . "application/x-www-form-urlencoded")
                                        ("Authorization" . ,(counsel-spotify-basic-auth-credentials)))
                             :data data))

(aio-defun counsel-spotify-oauth2--request-access-pkce (token-url client-id code code-verifier &optional redirect-uri)
  ""
  (when code
    (let ((result (aio-await (counsel-spotify-oauth2-make-access-request
                              token-url
                              (concat
                               "&client_id=" client-id
                               "&code=" code
                               "&redirect_uri=" (url-hexify-string (or redirect-uri "urn:ietf:wg:oauth:2.0:oob"))
                               "&grant_type=authorization_code"
                               (concat "&code_verifier=" (url-hexify-string code-verifier)))))))
      (make-oauth2-token :client-id client-id
                         :access-token (cdr (assoc 'access_token result))
                         :refresh-token (cdr (assoc 'refresh_token result))
                         :token-url token-url
                         :access-response result))))

(aio-defun counsel-spotify-oauth2--auth-pkce-p (auth-url token-url client-id &optional scope state redirect-uri)
  (let* ((code-verifier (counsel-spotify-oauth--generate-code-verifier))
         (auth-code (aio-await (counsel-spotify-oauth2--request-pkce-authorization-p auth-url client-id code-verifier redirect-uri scope state))))
    (aio-await
     (counsel-spotify-oauth2--request-access-pkce
      token-url
      client-id
      auth-code
      code-verifier
      redirect-uri))))

(aio-defun counsel-spotify-oauth2-auth-and-store-pkce-p (auth-url token-url scope client-id &optional redirect-uri state)
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
                           :client-secret nil
                           :access-token (plist-get plist :access-token)
                           :refresh-token (plist-get plist :refresh-token)
                           :token-url token-url
                           :access-response (plist-get plist :access-response))
      ;; else
      (let ((token (aio-await (counsel-spotify-oauth2--auth-pkce-p auth-url token-url client-id scope state redirect-uri))))
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

(aio-defun counsel-spotify-oauth-fetch-token-pkce-p ()
  (let ((token (aio-await (counsel-spotify-oauth2-auth-and-store-pkce-p counsel-spotify-spotify-api-authorization-url
                                                                        counsel-spotify-spotify-api-authentication-url
                                                                        counsel-spotify-spotify-api-scopes
                                                                        counsel-spotify-client-id
                                                                        counsel-spotify-spotify-api-redirect-url))))
    (setq counsel-spotify-spotify-api-auth-token token)
    token))


(aio-defun counsel-spotify-oauth2-refresh-access-pkce (token)
  "Refresh OAuth access TOKEN.
TOKEN should be obtained with `oauth2-request-access'."
  (let* ((url (oauth2-token-token-url token))
         (data (concat "client_id=" (url-hexify-string (oauth2-token-client-id token))
                       "&refresh_token=" (url-hexify-string (oauth2-token-refresh-token token))
                       "&grant_type=refresh_token"))
         (access-token (aio-await (counsel-spotify-oauth2-make-access-request url data))))
    (setf (oauth2-token-access-token token) (alist-get 'access_token access-token))
    ;; need to renew refresh token as well when using pkce flow for spotify api
    ;; https://community.spotify.com/t5/Spotify-for-Developers/Refresh-token-revoked/td-p/5190755
    (setf (oauth2-token-refresh-token token) (alist-get 'refresh_token access-token))
    ;; If the token has a plstore, update it
    (let ((plstore (oauth2-token-plstore token)))
      (when plstore
        (plstore-put plstore (oauth2-token-plstore-id token)
                     nil `(:access-token
                           ,(oauth2-token-access-token token)
                           :refresh-token
                           ,(oauth2-token-refresh-token token)
                           :access-response
                           ,(oauth2-token-access-response token)))
        (plstore-save plstore)))
    token))

(aio-defun counsel-spotify-refresh-oauth-token-pkce ()
  (interactive)
  (message "Refreshing oauth token.")
  (let* ((token (aio-await (counsel-spotify-oauth-fetch-token-pkce-p)))
         (refreshed-token (aio-await (counsel-spotify-oauth2-refresh-access-pkce token))))
    (setq counsel-spotify-spotify-api-auth-token refreshed-token)
    (message "Finished refreshing oauth token.")
    refreshed-token))
    

(aio-defun counsel-spotify-reset-oauth-token-pkce ()
  "Lets you re-do the authentication and re-fetch auth code from Spotify API in case
   something goes awry. It assumes that you haven't changed the default place where oauth2.plstore
   is placed."
  (interactive)
  (message "Resetting oauth token")
  (delete-file (concat user-emacs-directory "oauth2.plstore"))
  (setq counsel-spotify-spotify-api-auth-token nil)
  (aio-await (counsel-spotify-refresh-oauth-token-pkce))
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

(defun counsel-spotify-oauth2-url-retrieve-p (token url &optional request-method request-data)
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
