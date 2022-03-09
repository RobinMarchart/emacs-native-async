;;;  native-async-rs.el --- enable native rust code to signal completion to emacs -*-lexical-binding: t; -*-
;;
;;Package-Requires: ((promise "1.1"))
;;; Commentary:
;;;
;;;
;;; Code:
;;;
;;;


(funcan)

(unless module-file-suffix (error "Missing native module support"))
(require 'promise)
(require 'bindat)
(if (= most-positive-fixnum 9223372036854775807) (error "only available on 64 bit Emacs"))

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

(defvar native-async-rs--event-struct '((:low u32r) (:high u32r)) "struct used for reading events from pipe.")

(defvar native-async-rs--default-event '[(lambda (_) ()) (lambda (_) ())] "default event register")

(defun native-async-rs--parse-index (string index) "parse event index from string at position"
       (let ((struct (bindat-unpack string index)))
         (logior (bindat-get-field structure :low) (ash (bindat-get-field structure :high)))))
(defun native-async-rs--accept (events notifications index)
  (let ((event (gethash index events 'native-async-rs--default-event)))
    (condition-case err
        (funcall (aref event 0) (emacs-native-async-impl/retrieve notifications index))
      (t (funcall (aref event 1) err)))))

(defun native-async-rs-init () "Initialize the event handler.
This includes both the Emacs and rust side."
       (let ((events (make-hash-table :test eql)) (exec(expand-file-name (native-async-rs--executable-file-name) (native-async-rs--rootdir))) (notifications 'nil))
         (setq notifications (emacs-native-async-impl/setup (lambda (fd) (make-process) :buffer 'nil
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
