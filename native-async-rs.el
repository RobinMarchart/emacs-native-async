;;;  native-async-rs.el --- enable native rust code to signal completion to emacs -*-lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Robin Marchart
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
;;Package-Requires: ((promise "1.1"))
;;; Commentary:
;;;
;;;
;;; Code:
;;;
;;;


(unless module-file-suffix (error "Missing native module support"))
(require 'promise)
(require 'bindat)
(require 'native-async-rs-impl)
(if (= most-positive-fixnum 9223372036854775807) (error "only available on 64 bit Emacs"))

(defvar native-async-rs--rootdir (file-name-directory (or load-file-name buffer-file-name)) "Local Directory of native-async-rs repo.")
(defvar native-async-rs--executable-file-name "setup-bin" "Name of the helper executable.")


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

(defvar native-async-rs--event-struct '((:low u32r) (:high u32r)) "struct used for reading events from pipe.")

(defvar native-async-rs--default-event '[(lambda (_) ()) (lambda (_) ())] "default event register")

(defun native-async-rs--parse-index (string index) "parse event index from string at position"
       (let ((struct (bindat-unpack string index)))
         (logior (bindat-get-field structure :low) (ash (bindat-get-field structure :high)))))
(defun native-async-rs--accept (events notifications index)
  (let ((event (gethash index events 'native-async-rs--default-event)))
    (condition-case err
        (funcall (aref event 0) (native-async-rs-impl-retrieve notifications index))
      (t (funcall (aref event 1) err)))))

(defun native-async-rs-init () "Initialize the event handler.
This includes both the Emacs and rust side."
       (let ((events (make-hash-table :test eql)) (exec(expand-file-name (native-async-rs--executable-file-name) (native-async-rs--rootdir))) (notifications 'nil))
         (setq notifications (native-async-rs-impl-setup (lambda (fd) (make-process) :buffer 'nil
                                                           :command (exec (number-to-string fd))
                                                           :coding "binary"
                                                           :connection-type "pipe"
                                                           :filter
                                                           (lambda (_proc string)
                                                             (let ((index 0))
                                                               (while (< (length string) index)
                                                                 (make-thread
                                                                  (lambda ()
                                                                    (native-async-rs--accept events notifications
                                                                                             (native-async-rs--parse-index string index))))
                                                                 (setq index (+ index 8))))))))
         (record 'native-async-rs--notification-handler table notifications)))

(defvar native-async-rs--default-handler 'nil "Default handler. Initialized to nil.")

(defun native-async-rs-get () "Get default notification handler."
       (progn
         (unless 'native-async-rs--default-handler(setq native-async-rs--default-handler (native-async-rs-init)))
         native-async-rs--default-handler))

(defun native-async-rs-wait-for (notifications index) "Wait for the result of the operation identified by index."
       (promise-new (lambda (resolve reject) (puthash index [resolve reject] (aref notifications 1)))))

(provide 'native-async-rs)
;;; native-async-rs.el ends here
