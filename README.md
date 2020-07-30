# sharper
dotnet CLI wrapper, using Transient

This is a [Transient-based](https://github.com/magit/transient) menu for the [dotnet CLI](https://docs.microsoft.com/en-us/dotnet/core/tools/). It aims to cover the most common scenarios, but I expect eventually all of the `dotnet` commans will be implemented.

## Table of contents

<!--ts-->

   * [Installation and configuration](#installation-and-configuration)
   * [Usage](#usage)
   * [Beta status](#beta-status)

<!--te-->

## Installation and configuration

_NOTE: the package is not in MELPA yet :)_

Place `sharper.el` somewhere in your load path, or `load` it manually. Then bind `shaper-main-transient` to a key of your preference:

```elisp
(load "/path/to/sharper.el")
(require 'shaper)
(global-set-key (kbd "C-c n") 'sharper-main-transient) ;; Example "n" for "dot NET"
```

If are a `use-package` user, then:

```elisp
(use-package sharper :load-path "path/to/sharper_directory"
  :demand t
  :bind
  ("C-c n" . sharper-main-transient))
```

## Usage

Follow the prompts on the menus as needed.  
Any list of items (tabulated-list) has a binding to RET (Enter) that opens a transient that operates on the item under point or allows adding more.  
All the commands executed are logged to the `*sharper-log*` buffer, so it's easy to debug issues (and report them too!).  
The package has been tested on Linux and Windows, with ample use of quoting for shell arguments, still, something might have slipped under the radar... :)  
  
Some screenshots follow, so you know what to expect.

### Main menu

![Main menu](/screenshots/mainmenu.png)

After running a build for the first time, we can re-run it with the same parameters without going back to the Build menu. This applies to other common
commands (test, publish, etc).

### Build menu

![Build menu](/screenshots/buildmenu.png)

The `--runtime` parameter provides completion by querying the runtime catalog in the dotnet repos (without internet you lose completion but can 
still provide the parameter manually).

### Searching NuGet packages

![Nuget packages](/screenshots/nugetpackage.png)

You can search nuget.org, and browse the first 250 results for your search terms. Hitting RET over a package will provide completion on the possible version 
numbers. Here we see the last confirmation before adding the package to the project.

### Managing project packages

![Project packages](/screenshots/projectpackages.png)

There are modes similar to the one pictured that allow adding and removing local project references. There's also a solution management view to add/remove projects,
and see a complete (but static) listing of all the packages for all the projects in the solution.

### Test menu

![Test menu](/screenshots/testmenu.png)

When selecting a target for a command, there's completion for projects or solutions within the current `project.el` project. That usually means the current repository.

## Beta status

This package is in beta status, and active development. I consider it stable, but breaking changes _could_ happen based on feedback and more extensive daily usage.


