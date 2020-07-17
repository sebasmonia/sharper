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
;;   2. Add a binding to start sharper:
;;       (require 'sharper)
;;       (global-set-key (kbd "C-c n") 'sharper-main-transient) ;; For "n" for "dot NET"
;;
;; For a detailed user manual see:
;; https://github.com/sebasmonia/sharper/blob/master/README.md

;;; Code:


;;------------------Package infrastructure----------------------------------------

(require 'transient)
(require 'cl-lib)
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

(defcustom sharper-project-or-solution-regex ".*\\.\\(csproj\\|sln\\)$"
  "Regex to match filenames that are valid as project or solution files."
  :type 'string)

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
   ("b" "build" dotnet-menu-transient-build)])


;; TODO: REMOVE IN FINAL PACKAGE
(define-key hoagie-keymap (kbd "n") 'sharper-main-transient)
;; TODO: REMOVE IN FINAL PACKAGE


;;------------------dotnet common-------------------------------------------------

(defun sharper--read-solution-or-project ()
  (interactive)
  (cl-labels ((read-fn-predicate (filename)
                                 (string-match-p sharper-project-or-solution-regex
                                                 filename)))
    (let ((starting-directory (when (project-current)
                                  (project-root (project-current)))))
    (expand-file-name
     (read-file-name "Select solution or project: "
                     starting-directory ;; will be `default-directory' if nil
                     nil
                     t
                     nil
                     #'read-fn-predicate)))))

(define-infix-argument sharper--option-target-projsln ()
  :description "<PROJECT>|<SOLUTION>"
  :class 'transient-option
  :shortarg "T"
  :argument "<TARGET>="
  :reader (lambda (_prompt _initial-input _history)
            (sharper--read-solution-or-project)))

;;------------------dotnet build--------------------------------------------------

;; dotnet build [<PROJECT>|<SOLUTION>] [-c|--configuration <CONFIGURATION>]
;;     [-f|--framework <FRAMEWORK>] [--force] [--interactive] [--no-dependencies]
;;     [--no-incremental] [--no-restore] [--nologo] [-o|--output <OUTPUT_DIRECTORY>]
;;     [-r|--runtime <RUNTIME_IDENTIFIER>] [-s|--source <SOURCE>]
;;     [-v|--verbosity <LEVEL>] [--version-suffix <VERSION_SUFFIX>]

(defun sharper--build (&optional args)
  (interactive
   (list (transient-args 'sharper-transipent-build)))
  (transient-set)
  (message "args %s" args))

(define-transient-command sharper-transient-build ()
  "dotnet build menu"
  ["Arguments"
   (sharper--option-target-projsln)
   ("-c" "Configuration" "--configuration=Debug")
   ("-o" "Output" "-output=")
   ("-r" "Target runtime" "--runtime=")
   ("-ni" "No incremental" "--no-incremental")
   ("-nd" "No dependencies" "--no-dependencies")]
  ["Actions"
   ("b" "build" sharper-build)
   ("q" "quit" transient-quit-all)])

(provide 'sharper)
;;; sharper.el ends here
