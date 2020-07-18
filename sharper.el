;;; sharper.el --- dotnet CLI wrapper, using Transient.  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Sebastian Monia
;;
;; Author: Sebastian Monia <smonia@outlook.com>
;; URL: https://github.com/sebasmonia/sharper
;; Package-Requires: ((emacs "25"))
;; Version: 1.0
;; Keywords: maint tool

;; This file is not part of GNU Emacs.

;;; License: MIT

;;; Commentary:

;; This package aims to be a complete package for dotnet tasks that aren't part
;; of the languages but needed for any project: Solution management, nuget, etc.
;;
;; Steps to setup:
;;   1. Place sharper.el in your load-path.  Or install from MELPA.
;;   2. Add a binding to start sharper's transient:
;;       (require 'sharper)
;;       (global-set-key (kbd "C-c n") 'sharper-main-transient) ;; For "n" for "dot NET"
;;
;; For a detailed user manual see:
;; https://github.com/sebasmonia/sharper/blob/master/README.md

;;; Code:


;;------------------Package infrastructure----------------------------------------

(require 'transient)
(require 'cl-lib)
(require 'cl-extra)
(require 'project)

(defun sharper--message (text)
  "Show a TEXT as a message and log it, if 'panda-less-messages' log only."
  (message "Sharper: %s" text)
  (panda--log "Package message:" text "\n"))

(defun sharper--log (&rest to-log)
  "Append TO-LOG to the log buffer.  Intended for internal use only."
  (let ((log-buffer (get-buffer-create "*sharper-log*"))
        (text (cl-reduce (lambda (accum elem) (concat accum " " (prin1-to-string elem t))) to-log)))
    (with-current-buffer log-buffer
      (goto-char (point-max))
      (insert text)
      (insert "\n"))))


;;------------------Customization options-----------------------------------------

(defgroup sharper nil
  "dotnet CLI wrapper, using Transient."
  :group 'extensions)

(defcustom sharper-project-extensions '("csproj" "fsproj")
  "Valid extensions for project files."
  :type 'list)

;; (defcustom panda-open-status-after-build 'ask
;;   "Open the build status for the corresponding branch after requesting a build.
;; If yes, automatically open it.  No to never ask.  Set to 'ask (default) to be prompted each time."
;;   :type '(choice (const :tag "No" nil)
;;                  (const :tag "Yes" t)
;;                  (const :tag "Ask" ask)))


;; (defvar panda--auth-string nil "Caches the credentials for API calls.")
;; (defvar panda--projects-cache nil "Caches all the build projects the user has access to, in one go.")
;; (defvar panda--plans-cache nil "Caches the plans for each build project the user has access to, in one go.")
;; (defvar panda--branches-cache nil "Caches the branches for each plan, as they are requested.")
;; (defvar panda--deploys-cache nil "Caches the deployment projects (not build projects) in one single call to /deploy/project/all.")


;;------------------Main transient------------------------------------------------


(define-transient-command sharper-main-transient ()
  "dotnet Menu"
  ["Actions"
   ;; global
   ("b" "build" sharper-transient-build)])


;; TODO: REMOVE IN FINAL PACKAGE
(define-key hoagie-keymap (kbd "n") 'sharper-main-transient)
;; TODO: REMOVE IN FINAL PACKAGE


;;------------------Argument parsing----------------------------------------------

(defun sharper--get-target (transient-params)
  "Extract & shell-quote from TRANSIENT-PARAMS the \"TARGET\" argument."
  (shell-quote-argument
   (cl-some
    (lambda (an-arg) (when (string-prefix-p "<TARGET>=" an-arg)
                       (replace-regexp-in-string "<TARGET>="
                                                 ""
                                                 an-arg)))
    transient-params)))

(defun shaper--option-split-quote (an-option)
  (let* ((equal-char-index (string-match "=" an-option))
         (name (substring an-option 0 equal-char-index)))
    (if equal-char-index
        (cons name (shell-quote-argument
                    (substring an-option (+ 1 equal-char-index))))
      name)))

(defun shaper--only-options (transient-params)
  "Extract from TRANSIENT-PARAMS the options (ie, start with -)."
  (mapcar #'shaper--option-split-quote
          (cl-remove-if-not (lambda (arg) (string-prefix-p "-" arg))
                            transient-params)))

;;------------------dotnet common-------------------------------------------------

(defun sharper--filename-proj-or-sln-p (filename)
  "Return non-nil if FILENAME is a project or solution."
  (let ((extension (file-name-extension filename)))
    (or
     (string= "sln" extension)
     (member extension sharper-project-extensions))))

(defun sharper--filename-proj-p (filename)
  "Return non-nil if FILENAME is a project."
  (let ((extension (file-name-extension filename)))
    (member extension sharper-project-extensions)))

(defun sharper--read-solution-or-project ()
  "Offer completion for project or solution files under the current project's root."
  (let ((all-files (project-files (project-current t))))
    (completing-read "Select project or solution: "
                     all-files
                     #'sharper--filename-proj-or-sln-p)))

(defun sharper--read--project ()
  "Offer completion for project files under the current project's root."
  (let ((all-files (project-files (project-current t))))
    (completing-read "Select project or solution: "
                     all-files
                     #'sharper--filename-proj-p)))

(defun sharper--read-solution-or-project ()
  "Offer completion for project or solution files under the current project's root."
  (let ((all-files (project-files (project-current t))))
    (completing-read "Select project or solution: "
                     all-files
                     #'sharper--filename-proj-or-sln-p)))

(define-infix-argument sharper--option-target-projsln ()
  :description "<PROJECT>|<SOLUTION>"
  :class 'transient-option
  :shortarg "T"
  :argument "<TARGET>="
  :reader (lambda (_prompt _initial-input _history)
            (sharper--read-solution-or-project)))

;;------------------dotnet build--------------------------------------------------

(defun sharper--build (&optional transient-params)
  "Run \"dotnet build\" using TRANSIENT-PARAMS as arguments & options."
  (interactive
   (list (transient-args 'sharper-transient-build)))
  (transient-set)
  (let ((target (sharper--get-target transient-params))
        (args (shaper--only-options transient-params)))
    (message "target %s" target)
    (message "args %s" (prin1-to-string args))
    (message "Whole thing%s" (prin1-to-string transient-params))))

(defun dotnet-build ()
  "Build a .NET project."
  (interactive)
  (let* ((target (dotnet--select-project-or-solution))
         (command (concat "dotnet build " (dotnet--verbosity-param)  " %s")))
    (let (directory-default (file-name-directory target))
      (compile (format command target)))))

(define-transient-command sharper-transient-build ()
  "dotnet build menu"
  :value '("--configuration=Debug" "--verbosity=minimal")
  ["Common Arguments"
   (sharper--option-target-projsln)
   ("-c" "Configuration" "--configuration=")
   ("-v" "Verbosity" "--verbosity=")]
  ["Other Arguments"
   ("-o" "Output" "-output=")
   ("-ni" "No incremental" "--no-incremental")
   ("-nd" "No dependencies" "--no-dependencies")
   ("-r" "Target runtime" "--runtime=")
   ("-s" "NuGet Package source URI" "--source")
   ("-es" "Version suffix" "--version-suffix=")]
  ["Actions"
   ("b" "build" sharper--build)
   ("q" "quit" transient-quit-all)])

(provide 'sharper)
;;; sharper.el ends here
