# sharper
dotnet CLI wrapper, using Transient

This is a [Transient-based](https://github.com/magit/transient) menu for the [dotnet CLI](https://docs.microsoft.com/en-us/dotnet/core/tools/). It aims to cover the most common scenarios, but I expect eventually all of the `dotnet` commands will be implemented.

## Table of contents

<!--ts-->

   * [Installation and configuration](#installation-and-configuration)
   * [Usage](#usage)
   * [Beta status](#beta-status)

<!--te-->

## Installation and configuration

Place `sharper.el` somewhere in your load path, or (preferred) install from MELPA. Then bind `shaper-main-transient` to a key of your preference:

```elisp
(load "/path/to/sharper.el")
(require 'shaper)
(global-set-key (kbd "C-c n") 'sharper-main-transient) ;; Example "n" for "dot NET"
```

If are a `use-package` user, then:

```elisp
(use-package sharper
  :demand t
  :bind
  ("C-c n" . sharper-main-transient))
```

## Usage

Follow the prompts on the menus as needed.  
Any list of items (tabulated-list) has a binding to RET (Enter) that opens a transient to operate on the item under point or to add more elements.  
All the commands executed are logged to the `*sharper-log*` buffer, so it's easy to debug issues (and report them too!).  
The package has been tested on Linux and Windows, with ample use of quoting for shell arguments, still, something might have slipped under the radar... :)  
  
Some screenshots follow, so you know what to expect.

### Main menu

![Main menu](/screenshots/mainmenu.png)

After running a build for the first time, we can re-run it with the same parameters without going back to the Build menu. This applies to other common
commands (test, publish, etc).

There are two convenience commands for builds and tests:

-`sb` stands for single build, and it will `dotnet build` the nearest project in the directory tree. For example if you 
are editing a source file and want to build only the project you are currently working on rather the whole solution.

-`st` or single test, that will run `dotnet test` filtering to the current method's name. This is useful if you are modifying a test and would like to 
run only that method instead of your whole test suite. It uses the same logic as `sb` to find the project. The method name is detected using csharp-mode,
and as a fallback the word at point instead.

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

This package is in beta status, and active development. However I consider it stable and will avoid breaking changes as much as possible.


