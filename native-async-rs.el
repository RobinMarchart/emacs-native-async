;;;  native-async-rs.el --- enable native rust code to signal completion to emacs -*-lexical-binding: t; -*-
;;
;;Package-Requires: ((promise "1.1"))
;;; Commentary:
;;;
;;;
;;; Code:

(require 'promise)

(unless module-file-suffix (error "Missing native module support"))

(defvar native-async-rs--rootdir (file-name-directory (or load-file-name buffer-file-name)) "Local Directory of native-async-rs repo.")
(defvar native-async-rs--module-dir (expand-file-name "native-setup" native-async-rs--rootdir) "Source directory of the dynamic module crate.")
(defvar native-async-rs--exec-dir (expand-file-name "setup-bin" native-async-rs--rootdir) "Source directory of the helper executable crate.")
(defvar native-async-rs--build-type-release "t" "Build type used for builds and binary path.")
(defvar native-async-rs--executable-file-name "setup-bin" "Name of the helper executable.")
(defvar native-async-rs--module-file-name "libnative_setup.so" "Name of the dynamic module file.")
(defvar native-async-rs--cargo-executable "cargo" "Name of the used cargo executable.")

(defun native-async-rs--wait-sync (promise) "Run PROMISE in other thread and suspend until completion." ())

(defun native-async-rs-build (dir) "Build the default target of the crate inside this DIR."
       (make-process :buffer "native-async-rs-buildlog" :command (append('native-async-rs--cargo-executable "build")
                                                                        (if ('native-async-rs--build-type-release) ("--release") ()))
                     :query 'nil
                     :connection-type "pipe"
                     :sentinel (lambda (process _status) ())))


(defun native-async-rs--init () "Initialize the event handler.
This includes both the Emacs and rust side." (let ((target-dir
                                                    (expand-file-name (if ('native-async-rs--build-type-release) ('"release") ('"debug"))
                                                                      (expand-file-name "target" native-async-rs--rootdir))))
                                               (let (
                                                     (exec-file (expand-file-name native-async-rs--executable-file-name target-dir))
                                                     (module-file (expand-file-name native-async-rs--module-file-name target-dir)))
                                                 (if (file-executable-p exec-file) ('t) ()))))

(defun native-async-rs-init () "Initialize notification handler.")

(defun native-async-rs-get () "Get default notification handler.")




(provide 'native-async-rs)
;;; native-async-rs.el ends here
