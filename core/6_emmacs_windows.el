;;; 6_emmacs_windows.el --- User Insterface

;;; Commentary:
;; This section contains specific customizations for Emacs Windows

;;; Code:
;; Windows utils
(when (string-equal system-type "windows-nt")

  (progn
    ;; Emacs cygwin configuration for Linux utils
    (setenv "PATH"
	    (concat (getenv "PATH") ";" "C:\\cygwin64\\bin"))
    ;; Configure Linux executables
    (setq find-program "C:/cygwin64/bin/find.exe"
            grep-program "c:/cygwin64/bin/grep.exe"))
  ;; Advice for setting null device in grep command
  (defun grep--change-null-dev (orig-fun &rest args)
    (let ((null-device "/dev/null"))
      (apply orig-fun args)))
  (advice-add 'grep-compute-defaults :around #'grep--change-null-dev))

(provide '6_emmacs_windows)
;;; 6_emmacs_windows.el ends here
