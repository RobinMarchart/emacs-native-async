;;;  native-async-rs.el --- enable native rust code to signal completion to emacs -*-lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Robin Marchart
;;
;; Author: Robin Marchart
;; URL: https://github.com/RobinMarchart/emacs-native-async
;; Package-Requires: ((promise "1.1") (emacs "27"))
;; Version: 0.2

;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;

;;; Commentary:
;;;
;;;
;;; Code:


(unless module-file-suffix (error "Missing native module support"))
(require 'promise)
(require 'bindat)
(if (= most-positive-fixnum 9223372036854775807) (error "Only available on 64 bit Emacs"))

(defvar native-async-rs-build-silent nil "Don't ask if library should be build if t.")

(defvar native-async-rs--rootdir (expand-file-name(file-name-directory (or load-file-name buffer-file-name)))"Local Directory of native-async-rs repo.")
(defvar native-async-rs--executable-path (expand-file-name "target/release/setup-bin" native-async-rs--rootdir) "Path of the helper executable.")
(defvar native-async-rs--module-path (expand-file-name (concat "target/release/libnative_setup" module-file-suffix) native-async-rs--rootdir) "Path of the native module.")
(defvar native-async-rs--compile-command '("cargo" "build" "--workspace" "--release") "Command to compile native module.")

(defvar native-async-rs--ensure-native-promise nil "Cached promise for ensure-native.")

(add-variable-watcher 'native-async-rs--rootdir
                      (lambda (_symbol value op _where)
                        (when (eq op 'set)
                          (set 'native-async-rs--executable-path (expand-file-name "target/release/setup-bin" value))
                          (set 'native-async-rs--module-path (expand-file-name (concat "target/release/libnative_setup" module-file-suffix) value)))))

(defun native-async-rs--setup-function (resolve reject)
  "Build the native module.
Calls RESOLVE with nil on success, REJECT on failure"
  (if (and (require 'native-async-rs-impl native-async-rs--module-path t) (file-executable-p native-async-rs--executable-path))
      (funcall resolve ())
    (if (or native-async-rs-build-silent (y-or-n-p "Native-async-rs needs to be build. do it now?"))
        (let (
              (buffer (get-buffer-create "native-async-rs-build"))
              (default-directory (file-name-as-directory native-async-rs--rootdir))
              (process-connection-type nil))
          (with-current-buffer buffer
            (compilation-mode)
            (setq-local
             default-directory (file-name-as-directory native-async-rs--rootdir)
             process-connection-type nil)
            (set-process-sentinel
             (apply #'start-process "native-async-rs-build" buffer native-async-rs--compile-command)
             (lambda (_process event)
               (pcase event
                 ("finished\n" (require 'native-async-rs-impl) (funcall resolve nil))
                 ((rx (| (seq "open" (* anychar)) "run\n")))
                 (_ (funcall reject event))))))
          (unless native-async-rs-build-silent (pop-to-buffer buffer))
          nil)
      (funcall reject "native-async-rs not build"))))

(defun native-async-rs--ensure-native () "Ensure, that all native components are compiled. Return promise."
       (unless native-async-rs--ensure-native-promise
         (setq native-async-rs--ensure-native-promise
               (promise-new #'native-async-rs--setup-function)))

       native-async-rs--ensure-native-promise)

(defun native-async-rs--wait-sync (promise) "Run PROMISE in other thread and suspend until completion."
       (unless (promise-class-p promise) (error "Invalid argument %s in native-async-rs--wait-sync" promise))
       (let* ((mutex (make-mutex)) (cond-var (make-condition-variable mutex)) (res nil))
         (promise-chain promise
           (then (lambda (value)
                   (with-mutex mutex
                     (setq res (make-vector 2 nil))
                     (aset res 0 't)
                     (aset res 1 value)
                     (condition-notify cond-var))))
           (catch (lambda (reason)
                    (with-mutex mutex
                      (setq res (make-vector 2 nil))
                      (aset res 1 reason)
                      (condition-notify cond-var)))))
         (with-mutex mutex
           (while (not res) (condition-wait cond-var)))
         (if (aref res 0)
             (aref res 1)
           (signal (car (aref res 1)) (cdr (aref res 1))))))

(intern "native-async-rs--notification-store")

(defvar native-async-rs--event-struct '((:low u32r) (:high u32r)) "Struct used for reading events from pipe.")

(defvar native-async-rs--default-event '[(lambda (_) ()) (lambda (_) ())] "Default event noop.")

(defun native-async-rs--parse-index (string index) "Parse event index from STRING at position INDEX."
       (let ((structure (bindat-unpack string index)))
         (logior (bindat-get-field structure :low) (ash (bindat-get-field structure :high) 32))))
(defun native-async-rs--accept (events notifications index)
  (let ((event (gethash index events 'native-async-rs--default-event)))
    (condition-case err
        (funcall (aref event 0) (native-async-rs-impl-retrieve notifications index))
      (t (funcall (aref event 1) err)))))

(defun native-async-rs-init-async ()
  "Initialize the event handler and this package asynchronously.
This includes both the Emacs and rust side."
  (promise-chain (native-async-rs--ensure-native)
    (then
     (lambda (_)
       (let
           ((events (make-hash-table))
            (notifications nil))
         (setq notifications
               (native-async-rs-impl-setup
                (lambda (fd)
                  (make-process
                   :name "native-async-worker"
                   :buffer nil
                   :command `(,native-async-rs--executable-path ,(number-to-string fd))
                   :coding 'binary
                   :connection-type 'pipe
                   :filter
                   (lambda (_proc string)
                     (let ((index 0))
                       (while (< (length string) index)
                         (make-thread
                          (lambda ()
                            (native-async-rs--accept events notifications (native-async-rs--parse-index string index))))
                         (set 'index (+ index 8)))))))))
         (record 'native-async-rs--notification-store events notifications))))))

(defun native-async-rs-init () "Initialize event handler and this package. Wait for completion synchronously."
       (native-async-rs--wait-sync (native-async-rs-init-async)))


(defvar native-async-rs--default-notification-store 'nil "Default notification store. Initialized to nil.")

(defun native-async-rs-get-async () "Get default notification store. Asynchronous version."
       (progn
         (unless native-async-rs--default-notification-store (setq native-async-rs--default-notification-store (native-async-rs-init-async)))
         native-async-rs--default-notification-store))

(defun native-async-rs-get () "Get default notification store. Synchronous version."
       (native-async-rs--wait-sync (native-async-rs-get-async)))

(defun native-async-rs-wait-for-async (notifications index)
  "Return a promise for the result from NOTIFICATIONS of the operation identified by INDEX."
  (promise-new (lambda (resolve reject) (puthash index (vector resolve reject) (aref notifications 1)))))

(defun native-async-rs-wait-for (notifications index) "Wait for the result from NOTIFICATIONS of the operation identified by INDEX."
       (native-async-rs--wait-sync (native-async-rs-wait-for-async notifications index)))

(provide 'native-async-rs)
;;; native-async-rs.el ends here
