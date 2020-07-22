# sharper
dotnet CLI wrapper, using Transient

This is a [Transient-based](https://github.com/magit/transient) menu for the [dotnet CLI](https://docs.microsoft.com/en-us/dotnet/core/tools/). It aims to cover the most common scenarios, but I expect eventually all of the `dotnet` commans will be covered.

## Table of contents

<!--ts-->

   * [Installation and configuration](#installation-and-configuration)
   * [Usage](#usage)
   * [Future features](#future-features)

<!--te-->

## Installation and configuration

Place `shaper.el` in your load-path, and `(require shaper)`. Or, preferred, install from MELPA. _NOTE: the package is not in MELPA yet, it is in ALPHA state :)_

Then bind `shaper-main-transient` to a key of your preference:

```elisp
(global-set-key (kbd "C-c n") 'sharper-main-transient) ;; For "n" for "dot NET"
```

# Usage

Follow the prompts on the menus as needed.

**Screenshots**

Main menu (as of 2020-07-21):

![Main menu](/screenshots/mainmenu.png)

Build menu (as of 2020-07-21):

![Build menu](/screenshots/buildmenu.png)


# Future features:

* Whatever I keep adding in the issues :)
* Solution management (add projects to solution, remove them)
* Adding/removing references between projects
* `dotnet nuget` commands. This will probably use a tablist-based UI to list the installed packages etc.
