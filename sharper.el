;;; sharper.el --- A dotnet CLI wrapper, using Transient  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Sebastian Monia
;;
;; Author: Sebastian Monia <smonia@outlook.com>
;; URL: https://github.com/sebasmonia/sharper
;; Package-Requires: ((emacs "26.3") (transient "20200601"))
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
;; Some commands show lists of items.  In those cases, "RET" shows the transient with the Actions
;; available.
;;
;; For a detailed user manual see:
;; https://github.com/sebasmonia/sharper/blob/master/README.md

;;; Code:

(require 'transient)
(require 'cl-lib)
(require 'cl-extra) ;; for cl-some
(require 'json)
(require 'project)

;;------------------Customization options-----------------------------------------

(defgroup sharper nil
  "dotnet CLI wrapper, using Transient."
  :group 'extensions)

(defcustom sharper-project-extensions '("csproj" "fsproj")
  "Valid extensions for project files."
  :type '(repeat string))

(defcustom sharper-RIDs-URL "https://raw.githubusercontent.com/dotnet/runtime/master/src/libraries/pkg/Microsoft.NETCore.Platforms/runtime.json"
  "URL to fetch the list of Runtime Identifiers for dotnet.  See https://docs.microsoft.com/en-us/dotnet/core/rid-catalog for more info."
  :type 'string)

(defcustom sharper--nuget-search-URL "https://azuresearch-usnc.nuget.org/query?q=%s&prerelease=true&semVerLevel=2.0.0&take=250"
  "URL to run a NuGet search.  Must contain a %s to replace with the search string the user will input."
  :type 'string)

;; Legend for the templates below:
;; %t = TARGET
;; %o = OPTIONS
;; %s = SOLUTION
;; %m = MS BUILD PROPERTIES (-p:Prop1=Val1 -p:Prop2=Val2 )
;; %a = RUN SETTINGS (dotnet test) or APPLICATION ARGUMENTS (dotnet run)
;; %p = PROJECT (when project != target)
;; %k = PACKAGE NAME
(defvar sharper--build-template "dotnet build %t %o %m" "Template for \"dotnet build\" invocations.")
(defvar sharper--test-template "dotnet test %t %o %a" "Template for \"dotnet test\" invocations.")
(defvar sharper--clean-template "dotnet clean %t %o" "Template for \"dotnet clean\" invocations.")
(defvar sharper--publish-template "dotnet publish %t %o" "Template for \"dotnet publish\" invocations.")
(defvar sharper--pack-template "dotnet pack %t %o %m" "Template for \"dotnet pack\" invocations.")
(defvar sharper--run-template "dotnet run --project %t %o %a" "Template for \"dotnet run\" invocations.")
(defvar sharper--sln-list-template "dotnet sln %t list" "Template for \"dotnet sln list\" invocations.")
(defvar sharper--sln-add-template "dotnet sln %t add %p" "Template for \"dotnet sln add\" invocations.")
(defvar sharper--sln-remove-template "dotnet sln %t remove %p" "Template for \"dotnet sln remove\" invocations.")
(defvar sharper--reference-list-template "dotnet list %t reference" "Template for \"dotnet list reference\" invocations.")
(defvar sharper--reference-add-template "dotnet add %t reference %p" "Template for \"dotnet add reference\" invocations.")
(defvar sharper--reference-remove-template "dotnet remove %t reference %p" "Template for \"dotnet remove reference\" invocations.")
(defvar sharper--package-list-template "dotnet list %t package" "Template for \"dotnet list package\" invocations.")
(defvar sharper--package-list-transitive-template "dotnet list %t package --include-transitive" "Template for \"dotnet list package\" including transitive packages.")
(defvar sharper--package-add-template "dotnet add %t package %k %o" "Template for \"dotnet add package\" invocations.")
(defvar sharper--package-remove-template "dotnet remove %t package %k" "Template for \"dotnet remove package\" invocations.")


;; NOTE: if I start needing more than just default-dir + command I might as well create
;; a struct that has directory, command, prop1, prop2 etc.
(defvar sharper--last-build nil "A cons cell (directory . last command used for a build).")
(defvar sharper--last-test nil "A cons cell (directory . last command used to run tests).")
(defvar sharper--last-publish nil "A cons cell (directory . last command used to run a publish).")
(defvar sharper--last-pack nil "A cons cell (directory . last command used to create a NuGet package).")
(defvar sharper--last-run nil "A cons cell (directory . last command used for \"dotnet run\").")

(defvar sharper--cached-RIDs nil "The list of Runtime IDs used for completion.")

(defvar-local sharper--solution-path nil "Used in `sharper--solution-management-mode' to store the current solution.")
(defvar-local sharper--project-path nil "Used in `sharper--project-references-mode' and `sharper--project-packages-mode' to store the current project.")

;;------------------Package infrastructure----------------------------------------

(defun sharper--message (text)
  "Show a TEXT as a message and log it, if 'panda-less-messages' log only."
  (message "Sharper: %s" text)
  (sharper--log "Package message:\n" text "\n"))

(defun sharper--log-command (title command)
  "Log a COMMAND, using TITLE as header, using the predefined format."
  (sharper--log "[command]" title "\n" command "\n"))

(defun sharper--log (&rest to-log)
  "Append TO-LOG to the log buffer.  Intended for internal use only."
  (let ((log-buffer (get-buffer-create "*sharper-log*"))
        (text (cl-reduce (lambda (accum elem) (concat accum " " (prin1-to-string elem t))) to-log)))
    (with-current-buffer log-buffer
      (goto-char (point-max))
      (insert text)
      (insert "\n"))))

(defun sharper--json-request (url &optional params method data)
  "Retrieve JSON result of calling URL with PARAMS and DATA using METHOD (default GET).  Return parsed objects."
  ;; Based on the Panda function for API calls to Bamboo
  (unless data
    (setq data ""))
  (let ((url-request-extra-headers
         `(("Accept" . "application/json")
           ("Content-Type" . "application/json")))
        (url-request-method (or method "GET"))
        (url-request-data (encode-coding-string data 'utf-8))
        (json-false :false))
    (when params
      (setq url (concat url "&" params)))
    (sharper--log "Requesting JSON data: " url-request-method "to "  url " with data " url-request-data)
    (sharper--message "Web request...")
    ;; 10 seconds timeout and silenced URL module included
    (with-current-buffer (url-retrieve-synchronously url t nil 10)
      (set-buffer-multibyte t)
      (goto-char url-http-end-of-headers)
      (let ((parsed-json 'error))
        (ignore-errors
          ;; if there's a problem parsing the JSON
          ;; parsed-json ==> 'error
          (if (fboundp 'json-parse-buffer)
              (setq parsed-json (json-parse-buffer
                                 :object-type 'alist))
            ;; Legacy :)
            (setq parsed-json (json-read))))
        (kill-buffer) ;; don't litter with API buffers
        (message nil) ;; clear echo area
        parsed-json))))

;;------------------Main transient------------------------------------------------

(define-transient-command sharper-main-transient ()
  "dotnet Menu"
  ["Build"
   ("B" "new build" sharper-transient-build)
   ("b" (lambda () (sharper--repeat-description sharper--last-build)) sharper--run-last-build)]
  ["Run test"
   ("T" "new test run" sharper-transient-test)
   ("t" (lambda () (sharper--repeat-description sharper--last-test)) sharper--run-last-test)]
  ["Run application"
   ("R" "new application run" sharper-transient-run)
   ("r" (lambda () (sharper--repeat-description sharper--last-run)) sharper--run-last-run)]
  ["Publish app"
   ("P" "new publish" sharper-transient-publish)
   ("p" (lambda () (sharper--repeat-description sharper--last-publish)) sharper--run-last-publish)]
  ["Generating a nuget package file"
   ("N" "new package" sharper-transient-pack)
   ("n" (lambda () (sharper--repeat-description sharper--last-pack)) sharper--run-last-pack)
   ;;("wp" "wizard for package metadata" sharper-transient-???)
   ]
  ["Solution & project management"
   ("ms" "Manage solution" sharper--manage-solution)
   ("mr" "Manage project references" sharper--manage-project-references)
   ("mp" "Manage project packages" sharper--manage-project-packages)]
  ["Misc commands"
   ("c" "clean" sharper-transient-clean)
   ("v" "version info (SDK & runtimes)" sharper--version-info)
   ("q" "quit" transient-quit-all)])

;; TODO: commands I haven't used and could (should?) be implemented:
;; dotnet store
;; ???

(defun sharper--repeat-description (the-var)
  "Format the command in THE-VAR for display in the main transient.
THE-VAR is one of the sharper--last-* variables."
  (if (not the-var)
      (propertize "[No previous invocation]"
                  'face
                  font-lock-doc-face)
    (concat "repeast last: "
            (propertize (cdr the-var)
                        'face
                        font-lock-doc-face))))

(defun sharper--run-last-build (&optional transient-params)
  "Run \"dotnet build\", ignore TRANSIENT-PARAMS, repeat last call via `sharper--last-build'."
  (interactive
   (list (transient-args 'sharper-transient-build)))
  (transient-set)
  (if sharper--last-build
      (let ((default-directory (car sharper--last-build))
            (command (cdr sharper--last-build)))
        (sharper--log-command "Build" command)
        (compile command))
    (sharper-transient-build)))

(defun sharper--run-last-test (&optional transient-params)
  "Run \"dotnet test\", ignore TRANSIENT-PARAMS, repeat last call via `sharper--last-test'."
  (interactive
   (list (transient-args 'sharper-transient-test)))
  (transient-set)
  (if sharper--last-test
      (let ((default-directory (car sharper--last-test))
            (command (cdr sharper--last-test)))
        (sharper--log-command "Test" command)
        (compile command))
    (sharper-transient-test)))

(defun sharper--run-last-publish (&optional transient-params)
  "Run \"dotnet publish\", ignore TRANSIENT-PARAMS, repeat last call via `sharper--last-publish'."
  (interactive
   (list (transient-args 'sharper-transient-publish)))
  (transient-set)
  (if sharper--last-publish
      (let ((default-directory (car sharper--last-publish))
            (command (cdr sharper--last-publish)))
        (sharper--log-command "Publish" command)
        (pop-to-buffer (sharper--run-async-shell command "*dotnet publish*"))
    (sharper-transient-publish)))

(defun sharper--run-last-pack (&optional transient-params)
  "Run \"dotnet build\", ignore TRANSIENT-PARAMS, repeat last call via `sharper--last-pack'."
  (interactive
   (list (transient-args 'sharper-transient-pack)))
  (transient-set)
  (if sharper--last-pack
      (let ((default-directory (car sharper--last-pack))
            (command (cdr sharper--last-pack)))
        (sharper--log-command "Pack" command)
        (compile command))
    (sharper-transient-pack)))

(defun sharper--run-last-run (&optional transient-params)
  "Run \"dotnet run\", ignore TRANSIENT-PARAMS, repeat last call via `sharper--last-run'."
  (interactive
   (list (transient-args 'sharper-transient-run)))
  (transient-set)
  (if sharper--last-run
      (let ((default-directory (car sharper--last-run))
            (command (cdr sharper--last-run)))
        (sharper--log-command "Run" command)
        (pop-to-buffer (sharper--run-async-shell command "*dotnet run*"))
    (sharper-transient-run)))

(defun sharper--version-info ()
  "Run version info for SDKs, runtime, etc."
  (interactive)
  (message "Compiling \"dotnet\" information...")
  (let ((dotnet-path (shell-command-to-string (if (string= system-type "windows-nt")
                                                  "where dotnet"
                                                "which dotnet")))
        (dotnet-version (shell-command-to-string "dotnet --version"))
        (dotnet-sdks (shell-command-to-string "dotnet --list-sdks"))
        (dotnet-runtimes (shell-command-to-string "dotnet --list-runtimes"))
        (buf (get-buffer-create "*dotnet info*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "dotnet path: "
              dotnet-path)
      (insert "dotnet version: "
              dotnet-version)
      (insert "\nInstalled SDKs:\n"
              dotnet-sdks)
      (insert "\nInstalled runtimes:\n"
              dotnet-runtimes)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

;;------------------Argument parsing----------------------------------------------

(defun sharper--get-target (transient-params)
  "Extract from TRANSIENT-PARAMS the \"TARGET\" argument."
  (sharper--get-argument "<TARGET>=" transient-params))

(defun sharper--get-argument (marker transient-params)
  "Extract from TRANSIENT-PARAMS the argument with  MARKER."
  (cl-some
   (lambda (an-arg) (when (string-prefix-p marker an-arg)
                      (replace-regexp-in-string marker
                                                ""
                                                an-arg)))
   transient-params))

(defun sharper--option-split-quote (an-option)
  "Split AN-OPTION and shell quote its value, or return it as-if if it is a string."
  (let* ((equal-char-index (string-match "=" an-option))
         (name (substring an-option 0 equal-char-index)))
    (if equal-char-index
        (cons name
              ;; quoting of the value for the parameter happens
              ;; later in sharper--option-alist-to-string
              (substring an-option (+ 1 equal-char-index)))
      name)))

(defun sharper--only-options (transient-params)
  "Extract from TRANSIENT-PARAMS the options (ie, start with -)."
  (mapcar #'sharper--option-split-quote
          (cl-remove-if-not (lambda (arg) (string-prefix-p "-" arg))
                            transient-params)))

(defun sharper--option-alist-to-string (options)
  "Convert the OPTIONS as parsed by `sharper--only-options' to a string."
  ;; Right now the alist intermediate step seems useless. But I think the alist
  ;; is a good idea in case we ever need to massage the parameters :)
  (mapconcat (lambda (str-or-pair)
               (if (consp str-or-pair)
                   (concat (car str-or-pair) " " (shell-quote-argument (cdr str-or-pair)))
                 str-or-pair))
             options
             " "))

(defun sharper--shell-quote-or-empty (param)
  "If PARAM nil or empty string, return empty string, else shell-quote PARAM."
  (if (or (string-empty-p param)
          (not param))
      ""
    (shell-quote-argument param)))

;;------------------format-spec facilities----------------------------------------

(defun sharper--strformat (template-name &rest args)
  "Apply `format-spec' to TEMPLATE-NAME using ARGS as key-value pairs.
Just a facility to make these invocations shorter."
  (format-spec template-name
               (sharper--as-alist args)))

(defun sharper--as-alist (the-list)
  "Convert THE-LIST to an alist by looping over the elements by pairs."
  ;; From this SO answer https://stackoverflow.com/a/19774752/91877
  (cl-loop for (head . tail) on the-list by 'cddr
           collect (cons head (car tail))))

;;------------------dotnet common-------------------------------------------------

(defun sharper--project-root (&optional path)
  "Get the project root from optional PATH or `default-directory'."
  ;; TODO: hello future self. This used to call `project-root', but it
  ;; wasn't available in Emacs < 28. I tried to do something smart about
  ;; this, but since it wasn't easy, it probably wasn't _that_ smart.
  ;; Decided instead to take advantage of the internals of project.el,
  ;; so if this breaks in the future, you know...fix it.
  (cdr (project-current nil
                        (or path
                            default-directory))))

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

(defun sharper--filename-sln-p (filename)
  "Return non-nil if FILENAME is a solution."
  (let ((extension (file-name-extension filename)))
    (string= "sln" extension)))

(defun sharper--read-solution-or-project ()
  "Offer completion for project or solution files under the current project's root."
  (let ((all-files (project-files (project-current t))))
    (completing-read "Select project or solution: "
                     all-files
                     #'sharper--filename-proj-or-sln-p)))

(defun sharper--read--project ()
  "Offer completion for project files under the current project's root."
  (let ((all-files (project-files (project-current t))))
    (completing-read "Select project: "
                     all-files
                     #'sharper--filename-proj-p)))

(defun sharper--read-solution ()
  "Offer completion for solution files under the current project's root."
  (let ((all-files (project-files (project-current t))))
    (completing-read "Select a solution: "
                     all-files
                     #'sharper--filename-sln-p)))

;; TODO: it would be really nice if this validated the format
(defun sharper--read-msbuild-properties ()
  "Read name-value pairs of MSBuild property strings."
  (let ((user-input (read-string
                     "Enter the MSBuild properties in the format p1=v1 p2=v2...pN=vN: ")))
    (mapconcat
     (lambda (pair)
       (concat "-p:" pair))
     (split-string user-input)
     " ")))

(define-infix-argument sharper--option-target-projsln ()
  :description "<PROJECT>|<SOLUTION>"
  :class 'transient-option
  :shortarg "T"
  :argument "<TARGET>="
  :reader (lambda (_prompt _initial-input _history)
            (sharper--read-solution-or-project)))

(define-infix-argument sharper--option-target-proj ()
  :description "<PROJECT>"
  :class 'transient-option
  :shortarg "T"
  :argument "<TARGET>="
  :reader (lambda (_prompt _initial-input _history)
            (sharper--read--project)))

(define-infix-argument sharper--option-msbuild-params ()
  :description "<MSBuildProperties>"
  :class 'transient-option
  :shortarg "-p"
  :argument "<MSBuildProperties>="
  :reader (lambda (_prompt _initial-input _history)
            (sharper--read-msbuild-properties)))

(defun sharper--run-async-shell (command buffer-name)
  "Call `async-shell-command' to run COMMAND using a buffer BUFFER-NAME.
Returns a reference to the output buffer."
  (let ((le-buffer (generate-new-buffer (generate-new-buffer-name
                                         buffer-name))))
    (async-shell-command command le-buffer le-buffer)
    le-buffer))

(defun sharper--shell-command-to-log (command)
  "Call `shell-command-to-string' to run COMMAND.  Log the output."
  (sharper--message "Running shell command...")
  (let ((cmd-output (shell-command-to-string command)))
    (sharper--log "[Command output]" "\n" cmd-output "\n"))
  ;; clear the echo area after we are done
  (message nil))

(defun sharper--list-solproj-all-packages ()
  "List ALL packages of the solution/project open in the calling buffer.
The solution or project is determined via the buffer local variables.
\"ALL\" packages refers to including the transitive references."
  (interactive)
  (let* ((slnproj (or sharper--solution-path sharper--project-path))
         (default-directory (file-name-directory slnproj))
         (command (sharper--strformat sharper--package-list-transitive-template
                                      ?t (shell-quote-argument slnproj))))
    (sharper--log-command "List solution/project packages (incl. transitive)" command)
    (sharper--run-async-shell command
                              (concat "*packages (full) "
                                      (file-name-nondirectory slnproj)
                                      "*"))))

(defun sharper--get-RIDs ()
  "Obtain the list of Runtimes IDs, format and return it.
After the first call, the list is cached in `sharper--cached-RIDs'."
  (unless sharper--cached-RIDs
    (let ((json-data (sharper--json-request sharper-RIDs-URL)))
      (unless (eq json-data 'error)
        (setq sharper--cached-RIDs
              (mapcar #'car
                      (alist-get 'runtimes json-data))))))
  sharper--cached-RIDs)

(define-infix-argument sharper--option-target-runtime ()
  :description "Target runtime"
  :class 'transient-option
  :shortarg "-r"
  :argument "--runtime="
  :reader (lambda (_prompt _initial-input _history)
            (completing-read "Runtime: "
                             (sharper--get-RIDs))))

;;------------------dotnet build--------------------------------------------------

(defun sharper--build (&optional transient-params)
  "Run \"dotnet build\" using TRANSIENT-PARAMS as arguments & options."
  (interactive
   (list (transient-args 'sharper-transient-build)))
  (transient-set)
  (let* ((target (sharper--get-target transient-params))
         (options (sharper--only-options transient-params))
         (msbuild-props (sharper--get-argument "<MSBuildProperties>=" transient-params))
         ;; We want *compilation* to happen at the root directory
         ;; of the selected project/solution
         (directory (sharper--project-root target)))
    (unless target ;; it is possible to build without a target :shrug:
      (sharper--message "No TARGET provided, will build in default directory."))
    (let ((command (sharper--strformat sharper--build-template
                                       ?t (sharper--shell-quote-or-empty target)
                                       ?o (sharper--option-alist-to-string options)
                                       ?m (sharper--shell-quote-or-empty  msbuild-props))))
      (setq sharper--last-build (cons directory command))
      (sharper--run-last-build))))

(define-transient-command sharper-transient-build ()
  "dotnet build menu"
  :value '("--configuration=Debug" "--verbosity=minimal")
  ["Common Arguments"
   (sharper--option-target-projsln)
   ("-c" "Configuration" "--configuration=")
   ("-v" "Verbosity" "--verbosity=")]
  ["Other Arguments"
   ("-w" "Framework" "--framework=")
   ("-o" "Output" "--output=")
   ("-ni" "No incremental" "--no-incremental")
   ("-nd" "No dependencies" "--no-dependencies")
   (sharper--option-target-runtime)
   (sharper--option-msbuild-params)
   ("-s" "NuGet Package source URI" "--source")
   ("-es" "Version suffix" "--version-suffix=")]
  ["Actions"
   ("b" "build" sharper--build)
   ("q" "quit" transient-quit-all)])

;;------------------dotnet test---------------------------------------------------

(defun sharper--test (&optional transient-params)
  "Run \"dotnet test\" using TRANSIENT-PARAMS as arguments & options."
  (interactive
   (list (transient-args 'sharper-transient-test)))
  (transient-set)
  (let* ((target (sharper--get-target transient-params))
         (options (sharper--only-options transient-params))
         (run-settings (sharper--get-argument "<RunSettings>=" transient-params))
         ;; We want *compilation* to happen at the root directory
         ;; of the selected project/solution
         (directory (sharper--project-root target)))
    (unless target ;; it is possible to test without a target :shrug:
      (sharper--message "No TARGET provided, will run tests in default directory."))
    (let ((command (sharper--strformat sharper--test-template
                                       ?t (sharper--shell-quote-or-empty target)
                                       ?o (sharper--option-alist-to-string options)
                                       ?a (if run-settings
                                              (concat "-- " runtime-settings)
                                            ""))))
      (setq sharper--last-test (cons directory command))
      (sharper--run-last-test))))

(define-infix-argument sharper--option-test-runsettings ()
  :description "<RunSettings>"
  :class 'transient-option
  :shortarg "rs"
  :argument "<RunSettings>="
  :reader (lambda (_prompt _initial-input _history)
            (read-string "RunSettings arguments: ")))

(define-transient-command sharper-transient-test ()
  "dotnet test menu"
  :value '("--configuration=Debug" "--verbosity=minimal")
  ["Common Arguments"
   (sharper--option-target-projsln)
   ("-c" "Configuration" "--configuration=")
   ("-v" "Verbosity" "--verbosity=")
   ("-f" "Filter" "--filter=")
   ("-l" "Logger" "--logger=")
   ("-t" "List tests discovered""--list-tests")
   ("-nb" "No build" "--no-build")]
  ["Other Arguments"
   ("-b" "Blame" "--blame")
   ("-a" "Test adapter path" "--test-adapter-path=")
   ("-w" "Framework" "--framework=")
   ("-b" "Blame" "--blame")
   ("-o" "Output" "--output=")
   ("-O" "Data collector name" "--collect")
   ("-d" "Diagnostics file" "--diag=")
   ("-nr" "No restore" "--no-restore")
   (sharper--option-target-runtime)
   ("-R" "Results directory" "--results-directory=")
   ("-s" "Settings" "--settings=")
   ("-es" "Version suffix" "--version-suffix=")
   (sharper--option-test-runsettings)]
  ["Actions"
   ("t" "test" sharper--test)
   ("q" "quit" transient-quit-all)])

;;------------------dotnet clean--------------------------------------------------

(defun sharper--clean (&optional transient-params)
  "Run \"dotnet clean\" using TRANSIENT-PARAMS as arguments & options."
  (interactive
   (list (transient-args 'sharper-transient-clean)))
  (transient-set)
  (let* ((target (sharper--get-target transient-params))
         (options (sharper--only-options transient-params))
         ;; We want async-shell-command to happen at the root directory
         ;; of the selected project/solution
         (default-directory (sharper--project-root target)))
    (unless target ;; it is possible to build without a target :shrug:
      (sharper--message "No TARGET provided, will run clean in default directory."))
    (let ((command (sharper--strformat sharper--clean-template
                                       ?t (sharper--shell-quote-or-empty target)
                                       ?o (sharper--option-alist-to-string options))))
      (sharper--log "Clean command\n" command "\n")
      (sharper--run-async-shell command "*dotnet clean*"))))

(define-transient-command sharper-transient-clean ()
  "dotnet clean menu"
  :value '("--configuration=Debug" "--verbosity=normal")
  ["Common Arguments"
   (sharper--option-target-projsln)
   ("-c" "Configuration" "--configuration=")
   ("-v" "Verbosity" "--verbosity=")]
  ["Other Arguments"
   ("-w" "Framework" "--framework=")
   ("-o" "Output" "--output=")
   (sharper--option-target-runtime)]
  ["Actions"
   ("c" "clean" sharper--clean)
   ("q" "quit" transient-quit-all)])


;;------------------dotnet publish------------------------------------------------

(defun sharper--publish (&optional transient-params)
  "Run \"dotnet publish\" using TRANSIENT-PARAMS as arguments & options."
  (interactive
   (list (transient-args 'sharper-transient-publish)))
  (transient-set)
  (let* ((target (sharper--get-target transient-params))
         (options (sharper--only-options transient-params))
         ;; We want async-shell-command to happen at the root directory
         ;; of the selected project/solution
         (directory (sharper--project-root target)))
    (unless target ;; it is possible to test without a target :shrug:
      (sharper--message "No TARGET provided, will run tests in default directory."))
    (let ((command (sharper--strformat sharper--publish-template
                                       ?t (sharper--shell-quote-or-empty target)
                                       ?o (sharper--option-alist-to-string options))))
      (setq sharper--last-publish (cons directory command))
      (sharper--run-last-publish))))

(define-transient-command sharper-transient-publish ()
  "dotnet publish menu"
  :value '("--configuration=Debug" "--verbosity=minimal")
  ["Common Arguments"
   (sharper--option-target-projsln)
   ("-c" "Configuration" "--configuration=")
   ("-v" "Verbosity" "--verbosity=")]
  ["Other Arguments"
   ("-f" "Force" "--force")
   ("-w" "Framework" "--framework=")
   (sharper--option-target-runtime)
   ("-o" "Output" "--output=")
   ;; There are somewhat odd rules governing these two
   ;; easier to include both self contained flags and
   ;; have users make sense of them
   ("-sf" "Self contained" "--self-contained")
   ("-ns" "No self contained" "--no-self-contained")
   ("-nb" "No build" "--no-build")
   ("-nd" "No dependencies" "--no-dependencies")
   ("-nr" "No restore" "--no-restore")
   ("-es" "Version suffix" "--version-suffix=")]
  ["Actions"
   ("p" "publish" sharper--publish)
   ("q" "quit" transient-quit-all)])

;;------------------dotnet pack---------------------------------------------------

(defun sharper--pack (&optional transient-params)
  "Run \"dotnet pack\" using TRANSIENT-PARAMS as arguments & options."
  (interactive
   (list (transient-args 'sharper-transient-pack)))
  (transient-set)
  (let* ((target (sharper--get-target transient-params))
         (options (sharper--only-options transient-params))
         (msbuild-props (sharper--get-argument "<MSBuildProperties>=" transient-params))
         ;; We want *compilation* to happen at the root directory
         ;; of the selected project/solution
         (directory (sharper--project-root target)))
    (unless target ;; it is possible to build without a target :shrug:
      (sharper--message "No TARGET provided, will pack in default directory."))
    (let ((command (sharper--strformat sharper--pack-template
                                       ?t (sharper--shell-quote-or-empty target)
                                       ?o (sharper--option-alist-to-string options)
                                       ?m (sharper--shell-quote-or-empty msbuild-props))))
      (setq sharper--last-pack (cons directory command))
      (sharper--run-last-pack))))

(define-transient-command sharper-transient-pack ()
  "dotnet build menu"
  :value '("--configuration=Debug" "--verbosity=minimal")
  ["Common Arguments"
   (sharper--option-target-projsln)
   ("-c" "Configuration" "--configuration=")
   ("-v" "Verbosity" "--verbosity=")]
  ["Other Arguments"
   ("-f" "Force" "--force")
   ("-w" "Framework" "--framework=")
   (sharper--option-target-runtime)
   ("-o" "Output" "--output=")
   (sharper--option-msbuild-params)
   ("-is" "Include source" "--include-source")
   ("-iy" "Include symbols" "--include-symbols")
   ("-nb" "No build" "--no-build")
   ("-nd" "No dependencies" "--no-dependencies")
   ("-nr" "No restore" "--no-restore")
   ("-s" "Serviceable" "--serviceable")
   ("-es" "Version suffix" "--version-suffix=")]
  ["Actions"
   ("p" "pack" sharper--pack)
   ("q" "quit" transient-quit-all)])

;;------------------dotnet run----------------------------------------------------

(defun sharper--run (&optional transient-params)
  "Run \"dotnet run\" using TRANSIENT-PARAMS as arguments & options."
  (interactive
   (list (transient-args 'sharper-transient-run)))
  (transient-set)
  (let* ((target (sharper--get-target transient-params))
         (options (sharper--only-options transient-params))
         (app-args (sharper--get-argument "<ApplicationArguments>=" transient-params))
         ;; For run we want this to execute in the same directory
         ;; that the project is, where the .settings file is
         (directory (file-name-directory target)))
    (unless target ;; it is possible to run without a target :shrug:
      (sharper--message "No TARGET provided, will run in default directory."))
    (let ((command (sharper--strformat sharper--run-template
                                       ?t (sharper--shell-quote-or-empty target)
                                       ?o (sharper--option-alist-to-string options)
                                       ?a (if app-args
                                              (concat "-- " app-args)
                                            ""))))
      (setq sharper--last-run (cons directory command))
      (sharper--run-last-run))))

(define-infix-argument sharper--option-run-application-arguments ()
  :description "Application arguments"
  :class 'transient-option
  :shortarg "aa"
  :argument "<ApplicationArguments>="
  :reader (lambda (_prompt _initial-input _history)
            (read-string "Application arguments: ")))

(define-transient-command sharper-transient-run ()
  "dotnet run menu"
  :value '("--configuration=Debug" "--verbosity=minimal")
  ["Common Arguments"
   (sharper--option-target-proj)
   (sharper--option-run-application-arguments)
   ("-c" "Configuration" "--configuration=")
   ("-v" "Verbosity" "--verbosity=")]
  ["Other Arguments"
   ("-lp" "Launch profile" "--launch-profile=")
   ("-f" "Force" "--force")
   ("-w" "Framework" "--framework=")
   (sharper--option-target-runtime)
   ("-o" "Output" "--output=")
   ("-nl" "No launch profile" "--no-launch-profile")
   ("-nr" "No restore" "--no-restore")
   ("-nb" "No build" "--no-build")
   ("-nd" "No dependencies" "--no-dependencies")]
  ["Actions"
   ("r" "run" sharper--run)
   ("q" "quit" transient-quit-all)])

;;------------------dotnet solution management------------------------------------

(defun sharper--format-solution-projects (path)
  "Get and format the projects for the solution in PATH for `sharper--solution-management-mode'."
  (cl-labels ((convert-to-entry (project)
                                (list project (vector project))))
    (let ((command (sharper--strformat sharper--sln-list-template
                                       ?t (shell-quote-argument path))))
      (sharper--log-command "List solution projects" command)
      (mapcar #'convert-to-entry
              (nthcdr 2 (split-string (shell-command-to-string command)
                                      "\n" t))))))

(defun sharper--manage-solution ()
  "Prompt for a solution and start `sharper--solution-management-mode' for it."
  (interactive)
  (let* ((solution-full-path (sharper--read-solution))
         (solution-filename (file-name-nondirectory solution-full-path))
         (buffer-name (format "*solution %s* " solution-filename)))
    (with-current-buffer (get-buffer-create buffer-name)
      (sharper--solution-management-mode)
      ;;buffer local variables
      (setq sharper--solution-path solution-full-path)
      (sharper--solution-management-refresh)
      (pop-to-buffer buffer-name)
      (sharper--message (concat "Listing projects in " solution-filename)))))

(define-derived-mode sharper--solution-management-mode tabulated-list-mode "Sharper solution management" "Major mode to manage a dotnet solution."
  (setq tabulated-list-format [("Projects" 200 nil)])
  (setq tabulated-list-padding 1)
  (tabulated-list-init-header))

(define-transient-command sharper-transient-solution ()
  "dotnet sln menu"
  ["Actions"
   ("a" "add project to solution" sharper--solution-management-add)
   ("r" "remove project at point from solution" sharper--solution-management-remove)
   ("e" "manage project references" sharper--solution-management-open-proj-ref)
   ("p" "manage project packages" sharper--solution-management-open-proj-pack)
   ("l" "list packages for all projects in the solution" sharper--solution-management-packages)
   ("L" "list packages for all projects in the solution (including transitive packages)" sharper--list-solproj-all-packages)
   ("q" "quit" transient-quit-all)])

(define-key sharper--solution-management-mode-map (kbd "RET") 'sharper-transient-solution)
(define-key sharper--solution-management-mode-map (kbd "g") 'sharper--solution-management-refresh)

(defun sharper--solution-management-refresh ()
  "Update the tablist view in `sharper--solution-management-mode'."
  (interactive)
  (setq tabulated-list-entries
        (sharper--format-solution-projects sharper--solution-path))
  (tabulated-list-print))

(defun sharper--solution-management-add ()
  "Add a project to the current solution."
  (interactive)
  (let ((default-directory (file-name-directory sharper--solution-path))
        (command (sharper--strformat sharper--sln-add-template
                                     ?t (shell-quote-argument sharper--solution-path)
                                     ?p (shell-quote-argument (sharper--read--project)))))
    (sharper--log-command "Add to solution" command)
    (sharper--message (string-trim (shell-command-to-string command)))
    (sharper--solution-management-refresh)))

(defun sharper--solution-management-remove ()
  "Remove the project at point from the solution."
  (interactive)
  (let ((default-directory (file-name-directory sharper--solution-path))
        (command (sharper--strformat sharper--sln-remove-template
                                     ?t (shell-quote-argument sharper--solution-path)
                                     ?p (shell-quote-argument (tabulated-list-get-id)))))
    (sharper--log-command "Remove from solution" command)
    (sharper--message (string-trim (shell-command-to-string command)))
    (sharper--solution-management-refresh)))

(defun sharper--solution-management-packages ()
  "List the packages of all projects in the solution."
  (interactive)
  (let ((default-directory (file-name-directory sharper--solution-path))
        (command (sharper--strformat sharper--package-list-template
                                     ?t (shell-quote-argument sharper--solution-path))))
    (sharper--log-command "List solution packages" command)
    (sharper--run-async-shell command
                              (concat "*packages "
                                      (file-name-nondirectory sharper--solution-path)
                                      "*"))))

(defun sharper--solution-management-open-proj-ref ()
  "Open `sharper--project-references-mode' for the project at point."
  (interactive)
  ;; SOOOO...in Windows sln-directory has / separators
  ;; and project-relative-path has \\ separators, but _somehow_
  ;; the concatenation still works as intended :thinking:
  (let ((sln-directory (file-name-directory sharper--solution-path))
        (project-relative-path (tabulated-list-get-id)))
    (sharper--manage-project-references (concat sln-directory
                                                project-relative-path))))

(defun sharper--solution-management-open-proj-pack ()
  "Open `sharper--project-references-mode' for the project at point."
  (interactive)
  ;; same path note as above applies... :shrug:
  (let ((sln-directory (file-name-directory sharper--solution-path))
        (project-relative-path (tabulated-list-get-id)))
    (sharper--manage-project-packages (concat sln-directory
                                              project-relative-path))))

;;------------------dotnet project references-------------------------------------

(defun sharper--manage-project-references (project-full-path)
  "Prompt for PROJECT-FULL-PATH if not provided,  and start `sharper--project-references-mode' for it."
  (interactive
   (list (sharper--read--project)))
  (let* ((project-filename (file-name-nondirectory project-full-path))
         (buffer-name (format "*references %s* " project-filename)))
    (with-current-buffer (get-buffer-create buffer-name)
      (sharper--project-references-mode)
      ;;buffer local variables
      (setq sharper--project-path project-full-path)
      (sharper--project-references-refresh)
      (pop-to-buffer buffer-name)
      (sharper--message (concat "Listing projects in "
                                project-filename)))))

(defun sharper--format-project-references (path)
  "Get and format the project reference for the proejct in PATH for `sharper--project-references-mode'."
  (cl-labels ((convert-to-entry (reference)
                                (list reference (vector reference))))
    (let ((command (sharper--strformat sharper--reference-list-template
                                       ?t (shell-quote-argument path))))
      (sharper--log-command "List project references" command)
      (mapcar #'convert-to-entry
              (nthcdr 2 (split-string
                         (shell-command-to-string command)
                         "\n" t))))))

(define-derived-mode sharper--project-references-mode tabulated-list-mode "Sharper project references" "Major mode to manage project references."
  (setq tabulated-list-format [("Reference" 200 nil)])
  (setq tabulated-list-padding 1)
  (tabulated-list-init-header))

(define-transient-command sharper-transient-project-references ()
  "Project references menu"
  ["Actions"
   ("a" "add reference" sharper--project-reference-add)
   ("r" "remove reference at point" sharper--project-reference-remove)
   ("s" "switch to packages view" sharper--project-reference-switch-to-packages)
   ("q" "quit" transient-quit-all)])

(define-key sharper--project-references-mode-map (kbd "RET") 'sharper-transient-project-references)
(define-key sharper--project-references-mode-map (kbd "g") 'sharper--project-references-refresh)

(defun sharper--project-references-refresh ()
  "Update the tablist view in `sharper--project-references-mode'."
  (interactive)
  (setq tabulated-list-entries
        (sharper--format-project-references sharper--project-path))
  (tabulated-list-print))

(defun sharper--project-reference-switch-to-packages ()
  "Switch from references view to packages view."
  (interactive)
  (sharper--manage-project-packages sharper--project-path))

(defun sharper--project-reference-add ()
  "Add a project reference to the current project."
  (interactive)
  (let ((default-directory (file-name-directory sharper--project-path))
        (command (sharper--strformat sharper--reference-add-template
                                     ?t (shell-quote-argument sharper--project-path)
                                     ?p (shell-quote-argument (sharper--read--project)))))
    (sharper--log-command "Add project reference" command)
    (sharper--message (string-trim (shell-command-to-string command)))
    (sharper--project-references-refresh)))

(defun sharper--project-reference-remove ()
  "Remove the project at point from the current project's references."
  (interactive)
  (let ((default-directory (file-name-directory sharper--project-path))
        (command (sharper--strformat sharper--reference-remove-template
                                     ?t (shell-quote-argument sharper--project-path-path)
                                     ?p (shell-quote-argument (tabulated-list-get-id)))))
    (sharper--log-command "Remove project reference" command)
    (sharper--message (string-trim (shell-command-to-string command)))
    (sharper--project-references-refresh)))

;;------------------dotnet project packages---------------------------------------

(defun sharper--manage-project-packages (project-full-path)
  "Prompt for PROJECT-FULL-PATH if not provided,  and start `sharper--project-packages-mode' for it."
  (interactive
   (list (sharper--read--project)))
  (let* ((project-filename (file-name-nondirectory project-full-path))
         (buffer-name (format "*packages %s* " project-filename)))
    (with-current-buffer (get-buffer-create buffer-name)
      (sharper--project-packages-mode)
      ;;buffer local variables
      (setq sharper--project-path project-full-path)
      (sharper--project-packages-refresh)
      (pop-to-buffer buffer-name)
      (sharper--message (concat "Listing packages in "
                                project-filename)))))

(defun sharper--format-project-packages (path)
  "Get and format the project reference for the project in PATH for `sharper--project-references-mode'."
  (sharper--message "Running restore and retrieving list of packages...")
  (let ((restore-command (concat "dotnet restore "
                                 (shell-quote-argument path))))
    (sharper--log-command "Restore project packages" restore-command)
    (sharper--shell-command-to-log restore-command))
  (cl-labels ((extract-pack-name (line)
                                 (when (string-match-p " >" line)
                                   (cl-second (split-string line))))
              (convert-to-entry (line)
                                (list (extract-pack-name line) (vector line))))
    (let ((command (sharper--strformat sharper--package-list-template
                                       ?t (shell-quote-argument path))))
      (sharper--log-command "List project packages" command)
      (mapcar #'convert-to-entry
              (nthcdr 2 (split-string (shell-command-to-string command)
                                      "\n" t))))))

(define-derived-mode sharper--project-packages-mode tabulated-list-mode "Sharper project packages" "Major mode to manage project packages."
  (setq tabulated-list-format [("Packages info" 300 nil)])
  (setq tabulated-list-padding 1)
  (tabulated-list-init-header))

(define-transient-command sharper-transient-project-packages ()
  "Project packages menu"
  ["Actions"
   ("n" "search NuGet package" sharper--project-package-nuget)
   ("a" "add package" sharper-transient-add-package)
   ("r" "remove package at point" sharper--project-package-remove)
   ("s" "switch to references view" sharper--project-package-switch-to-references)
   ("L" "Show a listing including transitive packages" sharper--list-solproj-all-packages)
   ("q" "quit" transient-quit-all)])
;; TODO: package updates
;; Trying to add the package again is enough to get it bumped to latest version
;; HOWEVER, picking up versions with completion depends on nuget search and other unfinished features.
;; so the time being, we can leave this disabled
;; ("v" "change package at point to specific (or latest version)" sharper--project-reference-remove)

(define-key sharper--project-packages-mode-map (kbd "RET") 'sharper-transient-project-packages)
(define-key sharper--project-packages-mode-map (kbd "g") 'sharper--project-packages-refresh)

(defun sharper--project-package-nuget ()
  "Start a NuGet search to add a package to the current project."
  (interactive)
  (sharper--nuget-search sharper--project-path))

(defun sharper--project-package-switch-to-references ()
  "Switch from packages view to references view."
  (interactive)
  (sharper--manage-project-references sharper--project-path))

(defun sharper--project-packages-refresh ()
  "Update the tablist view in `sharper--project-packages-mode'."
  (interactive)
  (setq tabulated-list-entries
        (sharper--format-project-packages sharper--project-path))
  (tabulated-list-print))

(defun sharper--project-package-remove ()
  "Remove the package at point from the current project."
  (interactive)
  (let ((package-name (tabulated-list-get-id)))
    (if (not package-name)
        (sharper--message "No package to remove under point.")
      (let ((default-directory (file-name-directory sharper--project-path))
            (command (sharper--strformat sharper--package-remove-template
                                         ?t (shell-quote-argument sharper--project-path)
                                         ?k (shell-quote-argument package-name))))
        (sharper--log-command "Remove project package" command)
        (sharper--shell-command-to-log command)
        (sharper--project-packages-refresh)))))

(define-infix-argument sharper--option-add-package-name ()
  :description "Package name"
  :class 'transient-option
  :shortarg "pn"
  :argument "<PackageName>="
  :reader (lambda (_prompt _initial-input _history)
            (read-string "Package name: ")))

(define-transient-command sharper-transient-add-package ()
  "dotnet add package  menu"
  ["Common Arguments"
   (sharper--option-add-package-name)
   ("-v" "Version" "--version=")]
  ["Other Arguments"
   ("-w" "Framework" "--framework=")
   ("-nr" "No restore" "--no-restore")
   ("-s" "Source" "--source=")
   ("-pd" "Package directory" "--package-directory")]
  ["Actions"
   ("a" "add" sharper--package-add)
   ("q" "quit" transient-quit-all)])

(defun sharper--package-add (&optional transient-params)
  "Run \"dotnet add package\" using TRANSIENT-PARAMS as arguments & options."
  (interactive
   (list (transient-args 'sharper-transient-add-package)))
  (transient-set)
  (sharper--message "Adding package...")
  (let* ((package-name (sharper--get-argument "<PackageName>=" transient-params))
         (options (sharper--only-options transient-params))
         (command (sharper--strformat sharper--package-add-template
                                      ?t (shell-quote-argument sharper--project-path)
                                      ?k (shell-quote-argument package-name)
                                      ?o (sharper--option-alist-to-string options))))
    (sharper--log-command "Add project package" command)
    (sharper--shell-command-to-log command)
    (sharper--project-packages-refresh)))


(defun sharper--nuget-search-request (term)
  "Return the results of a search for TERM in NuGet.
Format of the returned data is (PackageId . [PackageId Verified Tags Versions-List])"
  (let* ((search-url (format sharper--nuget-search-URL (url-hexify-string term)))
         (packages-found (sharper--json-request search-url)))
    (setq meh packages-found)
    (mapcar #'sharper--format-nuget-entry (alist-get 'data packages-found))))

(defun sharper--format-nuget-entry (element)
  "Format ELEMENT to be used in a tablist."
  (let-alist element
    (list .id
          (vector .id
                  (if (eq .verified :false)
                      "No"
                    "Yes")
                  (number-to-string .totalDownloads)
                  (mapconcat #'identity .tags " ")
                  (replace-regexp-in-string "\n" " " .description)
                  .version
                  (nreverse (mapcar (lambda (v) (cdr (car v))) .versions))))))

(define-derived-mode sharper--nuget-results-mode tabulated-list-mode "Sharper nuget search results" "Major mode to install NuGet packages based on search results."
  (setq tabulated-list-format [("Package" 40 nil)
                               ("Verified" 8 nil)
                               ("Downloads" 9 nil)
                               ("Tags" 30 nil)
                               ("Description" 0 nil)])
  (setq tabulated-list-padding 1)
  (tabulated-list-init-header))

(define-key sharper--nuget-results-mode-map (kbd "RET") 'sharper--nuget-search-install)

(defun sharper--nuget-search (&optional project-path)
  "Search and add NuGet packages to PROJECT-PATH."
  (interactive)
  (let* ((term (read-string "Search term(s): "))
         (buffer-name (format "*NuGet search: %s* " term)))
    (with-current-buffer (get-buffer-create buffer-name)
      (sharper--nuget-results-mode)
      ;; if  nil, it will be prompted later
      (setq sharper--project-path project-path) ;; buffer local
      (setq tabulated-list-entries
            (sharper--nuget-search-request term))
      (tabulated-list-print)
      (switch-to-buffer buffer-name))))

(defun sharper--nuget-search-install ()
  "Install the package at point in the project that started the search flow."
  (interactive)
  (let* ((package-data (tabulated-list-get-entry))
         (name (elt package-data 0))
         (last-ver (elt package-data 5))
         (all-vers (elt package-data 6))
         (version (completing-read "Version to install: "
                                            all-vers
                                            nil
                                            t
                                            last-ver))
         (y-n-prompt (format "Add package %s to project %s?"
                             name
                             sharper--project-path))
         (command (sharper--strformat sharper--package-add-template
                                      ?t (shell-quote-argument sharper--project-path)
                                      ?k (shell-quote-argument name)
                                      ?o (concat "--version " version)))
         ;; Wanna keep around this buffer's name to close
         ;; it after adding the package
         (nuget-buffer-name (buffer-name)))
    (when (yes-or-no-p y-n-prompt)
      ;; TODO: project-path should be prompted it nil
      (sharper--log-command "Add project package" command)
      (sharper--shell-command-to-log command)
      ;; this will either open or refresh the existing buffer for project packages
      (sharper--manage-project-packages sharper--project-path)
      ;; since there's no way to start another search, after adding a package
      ;; this buffer's purpose in life has been fulfilled. Farewell, dear buffer!
      (kill-buffer nuget-buffer-name))))

(provide 'sharper)
;;; sharper.el ends here
