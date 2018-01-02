# How to contribute

:+1::tada: First off, thanks for taking the time to contribute to this course! :tada::+1:

## Table of contents:
* [General guidelines](#general-guidelines)
* [How to contribute to the codebase](#how-to-contribute-to-the-codebase)
* [How to create an issue](#how-to-create-an-issue)
* [Coding style and conventions](#coding-style-and-conventions)

## General guidelines
This repository contains the code for a course in *functional and concurrent programming* at KTH, Royal Institute of Technology. The course originally used Erlang as programming languge but it has switched to [Elixir](http://elixir-lang.github.io/) since spring term 2018. The assignment files and the lecture slides are written in [LaTex](https://www.latex-project.org/). GitHub-flavoured Markdown and Makefile are also used.

If you are knowedgable about Elixir or about the topic in general or you feel you can contribute to the improvement of the course you are very welcome to help. Below are more informations about [how to contribute to the codebase](#how-to-contribute-to-the-codebase).

If you find any mistake, bug or error and you do not know how to fix it, you can file an issue. We will try to solve the problem as quickly as possible. More about how to *properly* file an issue in the section [how to create an issue](#how-to-create-an-issue) below. 

**IMPORTANT: issues are for problems in the code or in the structure of the repository. DO NOT file an issue to ask for an explanation of a part of the topic you do not understand!**

Two general rules for any contribution:
1) Communication is fundamental: get in contact with the mantainers of this repository and explain the intentions and reasons behind your work. Get feedback on how the others see your work. **If you do not know something, simply ask!**.
2) Keep the code clean, modular and observe general rules for writing good code. More information about coding style in the [specific section](#coding-style-and-conventions) below.

## How to contribute to the codebase

### Repository structure
The repository is structured into three main folders plus few information files.
* `erlang`: contains old Erlang code for exercises and lectures
* `exercises`: contains the exercises in Elixir organized into folders. Each exercise folder is divided inside in `src` for the source files and `assignment.tex` for the LaTex exercise description.
* `lectures`: contains the lectures organized into folders.

The repository is sprinkled with Makefiles used for LaTex compilation among other things. Do not remove them!

### Making a Pull Request
**All contributions needs to be made through pull requests.** We highly recommend to use a *gitflow-like* workflow. Here is a basic example:
1) Fork the repository
2) Make the changes/additions to the codebase **in a specific branch in your fork**. You can use the following branch name convention `feature/<title>` (e.g. `feature/add-binary-tree`).
3) Open a pull request from your branch to the main repository master branch `johanmon/master`.
4) Clearly explain content and intention behind the pull request. Do not be afraid of extensively describe your work.
5) Remmber to synchronize your fork with the main repository (pull/rebase/merge) when the pull request has been accepted.

### Tips
* If the pull request is a resolution of an issue, please include the issue number in the title.
* If you want to make an addition or consistent change to the code contact the mantainers before. If you just want to fix a bug, go ahead and file a pull request.
* Be expressive in the name of the branch: use prefixes `feature/`and `bugfix/` based on needs.
* Try to mantain a clean and uniform style. Check section [below](#coding-style-and-conventions)!

## How to create an issue

If you find a bug, an error, or you want simply to suggest an improvement to the code or structure of the repository you can **file an issue**. You find the Issue panel on the top part of the repository view. When you create an issue, keep in mind the following points:
* Write a clear and concise title: what is the goal (fix bug or improvement), what is the general goal?
* Be clear about where the problem is located: specify folders and files.
* If the issue is about a bug you can use the following pattern: expected behaviour - actual behaviour - steps to reproduce the behaviour
* If available include error messages, stack traces, images, screenshots. Copy-pasting the code or error message is often preferred over a screenshot since it allows to easily copy the text.
* Use [GitHub-flavoured Markdown](https://guides.github.com/features/mastering-markdown/) to write better issues. 

## Coding style and conventions
