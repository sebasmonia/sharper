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

Place `shaper.el` in your load-path, and `(require shaper)`. Or, preferred, install from MELPA. _NOTE: the package is not in MELPA yet, it is in BETA state :)_

Then bind `shaper-main-transient` to a key of your preference:

```elisp
(global-set-key (kbd "C-c n") 'sharper-main-transient) ;; Example "n" for "dot NET"
```

# Usage

Follow the prompts on the menus as needed.
Any list of items (tabulated-list) has a binding to RET (Enter) that opens a transient that operates on the item under point or allows adding more.

Main menu:

![Main menu](/screenshots/mainmenu.png)

After running a build for the first time, we can re-run it with the same parameters without going back to the Build menu. This applies to other common
commands (test, publish, etc).

Build menu:

![Build menu](/screenshots/buildmenu.png)

Build menu:

![Build menu](/screenshots/buildmenu.png)

Showing that the `--runtime` parameter provides completion by querying the runtime catalog in the dotnet repos (without internet you lose completion but can 
still provide the parameter manually).

Searching NuGet packages:

![Nuget packages](/screenshots/nugetpackage.png)

You can search nuget.org, and browse the first 250 results for your search terms. Hitting RET over a package will provide completion on the possible version 
numbers. Here we see the last confirmation before adding the package to the project.

Managing project packages:

![Project packages](/screenshots/projectpackages.png)

There are modes similar to the one pictures that allow adding and remove local project references. There's also a solution management view to add/remove projects,
and see a complete (but static) listing of all the packages for all the projects in the solution.

Test menu:

![Test menu](/screenshots/testmenu.png)

When selecting a target for a command, there's completion for projects or solutions within the current `project.el` project. That usually means the current repo.


## Beta status

This package is in beta status, and active development. I consider it stable, but breaking changes _could_ happen based on feedback or more extensive daily usage.


