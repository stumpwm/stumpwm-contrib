;;;; swm-ykman.lisp

(in-package #:swm-ykman)

(defvar *swm-ykman-xclip-selection* "clipboard"
  "X selection to use for clipboard operations. Can be \"clipboard\" or \"primary\".")

(defun ykman-oath-accounts ()
  "Return a list of OATH account names from Yubikey."
  (multiple-value-bind (output err-text err-code)
      (uiop:run-program "ykman oath accounts list"
                        :ignore-error-status t
                        :output '(:string :stripped t)
                        :error-output '(:string :stripped t))
    (if (zerop err-code)
        (when (and output (not (string= output "")))
          (cl-ppcre:split "\\n" output))
        (progn
          (message "ykman error: ~a" err-text)
          nil))))

(defun ykman-oath-code (account)
  "Get TOTP code for ACCOUNT from Yubikey."
  (multiple-value-bind (output err-text err-code)
      (uiop:run-program (format nil "ykman oath accounts code -s '~a'" account)
                        :ignore-error-status t
                        :output '(:string :stripped t)
                        :error-output '(:string :stripped t))
    (if (zerop err-code)
        (string-trim '(#\Space #\Newline #\Tab) output)
        (progn
          (message "ykman error: ~a" err-text)
          nil))))

(defun copy-to-clipboard (text)
  "Copy TEXT to clipboard using xclip."
  (with-input-from-string (s text)
    (uiop:run-program (format nil "xclip -selection ~a" *swm-ykman-xclip-selection*)
                      :input s
                      :ignore-error-status t)))

(defun capture-qr-code ()
  "Use maim to select screen area and decode QR code with zbarimg.
Returns the decoded URI string or NIL on failure."
  (stumpwm::unmap-message-window (current-screen))
  (multiple-value-bind (output err-text err-code)
      (uiop:run-program "maim --select | zbarimg -q --raw -"
                        :ignore-error-status t
                        :output '(:string :stripped t)
                        :error-output '(:string :stripped t)
                        :force-shell t)
    (if (and (zerop err-code) output (not (string= output "")))
        (string-trim '(#\Space #\Newline #\Tab) output)
        (progn
          (when (not (zerop err-code))
            (message "QR capture failed: ~a" (if (string= err-text "") "cancelled or no QR found" err-text)))
          nil))))

(defun parse-otpauth-uri (uri)
  "Parse otpauth:// URI and extract account name.
Returns the account label (issuer:account or just account)."
  (when (and uri (cl-ppcre:scan "^otpauth://" uri))
    (multiple-value-bind (match groups)
        (cl-ppcre:scan-to-strings "^otpauth://[^/]+/([^?]+)" uri)
      (when match
        (uiop:run-program (format nil "printf '%b' '~a'"
                                  (cl-ppcre:regex-replace-all "%" (aref groups 0) "\\x"))
                          :output '(:string :stripped t)
                          :force-shell t)))))

(defun ykman-add-account (uri &optional force)
  "Add OATH account from URI to Yubikey. If FORCE is T, overwrite existing."
  (multiple-value-bind (output err-text err-code)
      (uiop:run-program (format nil "ykman oath accounts uri ~a '~a'"
                                (if force "-f" "")
                                uri)
                        :ignore-error-status t
                        :output '(:string :stripped t)
                        :error-output '(:string :stripped t))
    (declare (ignore output))
    (values (zerop err-code) err-text)))

(stumpwm:defcommand swm-ykman-totp () ()
  "Select an OATH account and copy TOTP code to clipboard."
  (let* ((accounts (ykman-oath-accounts))
         (selection (when accounts
                      (select-from-menu (current-screen)
                                        (mapcar #'list accounts)
                                        "TOTP account: "))))
    (cond
      ((null accounts)
       (message "No OATH accounts found on Yubikey"))
      ((null selection)
       nil) ; user cancelled
      (t
       (let ((code (ykman-oath-code (first selection))))
         (when code
           (copy-to-clipboard code)
           (message "TOTP code copied to clipboard")))))))

(stumpwm:defcommand swm-ykman-add-qr () ()
  "Capture QR code from screen and add OATH account to Yubikey.
If account already exists, prompts for action: overwrite, rename, or cancel."
  (let ((uri (capture-qr-code)))
    (unless uri
      (return-from swm-ykman-add-qr))
    (unless (cl-ppcre:scan "^otpauth://" uri)
      (message "Invalid QR code: not an otpauth:// URI")
      (return-from swm-ykman-add-qr))
    (let* ((account-name (parse-otpauth-uri uri))
           (existing-accounts (ykman-oath-accounts))
           (exists-p (member account-name existing-accounts :test #'string=)))
      (if exists-p
          ;; Account exists - ask user what to do
          (let ((choice (select-from-menu
                         (current-screen)
                         '(("Overwrite existing account")
                           ("Save with different name")
                           ("Cancel"))
                         (format nil "Account '~a' exists: " account-name))))
            (cond
              ((null choice) nil) ; cancelled
              ((string= (first choice) "Overwrite existing account")
               (multiple-value-bind (success err)
                   (ykman-add-account uri t)
                 (if success
                     (message "Account '~a' updated" account-name)
                     (message "Failed to update: ~a" err))))
              ((string= (first choice) "Save with different name")
               (let ((new-name (read-one-line (current-screen)
                                              "New account name: "
                                              :initial-input account-name)))
                 (when (and new-name (not (string= new-name "")))
                   ;; Modify URI with new account name
                   (let ((new-uri (cl-ppcre:regex-replace
                                   "^(otpauth://[^/]+/)[^?]+"
                                   uri
                                   (format nil "\\1~a" (cl-ppcre:regex-replace-all " " new-name "%20")))))
                     (multiple-value-bind (success err)
                         (ykman-add-account new-uri nil)
                       (if success
                           (message "Account '~a' added" new-name)
                           (message "Failed to add: ~a" err)))))))))
          ;; Account doesn't exist - add directly
          (multiple-value-bind (success err)
              (ykman-add-account uri nil)
            (if success
                (message "Account '~a' added" account-name)
                (message "Failed to add: ~a" err)))))))
