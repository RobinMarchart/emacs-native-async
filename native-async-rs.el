;;;  native-async-rs.el --- enable native rust code to signal completion to emacs -*-lexical-binding: t; -*-
;;
;;Package-Requires: ((promise "1.1"))
;;; Commentary:
;;;
;;;
;;; Code:

(unless module-file-suffix (error "Missing native module support"))
(require 'promise)


(defvar native-async-rs--rootdir (file-name-directory (or load-file-name buffer-file-name)) "Local Directory of native-async-rs repo.")
(defvar native-async-rs--executable-file-name "setup-bin" "Name of the helper executable.")
(defvar native-async-rs--module-file-name "libnative_setup" "Name of the dynamic module file.")

(load native-async-rs--module-file-name)

(defun native-async-rs--wait-sync (promise) "Run PROMISE in other thread and suspend until completion."
       (let ((mutex (make-mutex)) (res (make-vector 2 'nil)))
         (mutex-lock mutex)
         (promise-chain promise
           (then (lambda (value) (aset res 0 't) (aset res 1 value)))
           (catch (lambda (reason) (aset res 1 reason)))
           (finally (lambda () (mutex-unlock mutex))))
         (mutex-lock mutex)
         (if (aref res 0) (aref res 1) (signal (car (aref res 1)) (cdr (aref res 1))))))

(intern "native-async-rs--notification-handler")

(defun native-async-rs--init () "Initialize the event handler.
This includes both the Emacs and rust side."
       (let ((table (make-hash-table :test eql)) (exec(expand-file-name (native-async-rs--executable-file-name) (native-async-rs--rootdir))))
         (record 'native-async-rs--notification-handler table (emacs-native-async-impl/setup
                                                               (lambda (fd)
                                                                 (make-process
                                                                  :buffer 'nil
                                                                  :command (exec (number-to-string fd))
                                                                  :coding "binary"
                                                                  :connection-type "pipe"
                                                                  :filter (lambda (proc string) (let ((index 0))
                                                                                                  (while ())))))))))

(defun native-async-rs-init () "Initialize notification handler.")

(defun native-async-rs-get () "Get default notification handler.")

(provide 'native-async-rs)
;;; native-async-rs.el ends here
