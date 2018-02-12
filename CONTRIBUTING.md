# How to contribute

:+1::tada: First off, thanks for taking the time to contribute to this course! :tada::+1:

## Table of contents:
* [General guidelines](#general-guidelines)
* [How to contribute to the codebase](#how-to-contribute-to-the-codebase)
* [How to create an issue](#how-to-create-an-issue)
* [Coding style and conventions](#coding-style-and-conventions)

---

## General guidelines
This repository contains the code for a course in *functional and concurrent programming* at KTH, Royal Institute of Technology. The programming language used for this course was originally Erlang, but was changed to [Elixir](http://elixir-lang.github.io/) in the spring term 2018. The assignment files and the lecture slides are written in [LaTex](https://www.latex-project.org/). GitHub-flavoured Markdown and Makefile are also used.

If you are knowledgeable about Elixir or about the topic in general or you feel you can contribute to the improvement of the course you are very welcome to help. Below you can find more information about [how to contribute to the codebase](#how-to-contribute-to-the-codebase).

If you find any mistake, bug or error and you do not know how to fix it, you can file an issue. We will try to solve the problem as quickly as possible. More about how to *properly* file an issue in the section [how to create an issue](#how-to-create-an-issue) below. 

**IMPORTANT: issues are for problems in the code or in the structure of the repository. DO NOT file an issue to ask for an explanation of a part of the topic you do not understand!**

Two general rules for any contribution:
1) Communication is fundamental: get in contact with the maintainers of this repository and explain the intentions and reasons behind your work. Get feedback on how the others see your work. **If you do not know something, simply ask!**.
2) Keep the code clean, modular and observe general rules for writing good code. More information about coding style in the [specific section](#coding-style-and-conventions) below.

---

## How to contribute to the codebase

### Repository structure
The repository is structured into three main folders plus few information files.
* `erlang`: contains old Erlang code for exercises and lectures
* `exercises`: contains the exercises in Elixir organized into folders. Each exercise folder is further divided into `src` for the source files and `assignment.tex` for the LaTex exercise description.
* `lectures`: contains the lectures organized into folders.

The repository contains various Makefiles used for LaTex compilation among other things. Do not remove them!

### Making a Pull Request
**All contributions need to be made through pull requests.** We highly recommend to use a *gitflow-like* workflow. Here is a basic example:
1) Fork the repository
2) Make the changes/additions to the codebase **in a specific branch in your fork**. You can use the following branch name convention `feature/<title>` (e.g. `feature/add-binary-tree`).
3) Open a pull request from your branch to the main repository master branch `ID1019/functional-programming/master`.
4) Clearly explain content and intention behind the pull request. Do not be afraid of extensively describe your work.
5) Remember to synchronize your fork with the main repository (pull/rebase/merge) when the pull request has been accepted.

### Tips
* If the pull request is a resolution of an issue, please include the issue number in the title.
* If you want to make an addition or consistent change to the code contact the maintainers before. If you just want to fix a bug, go ahead and file a pull request.
* Be expressive in the name of the branch: use prefixes `feature/`and `bugfix/` based on needs.
* Try to maintain a clean and uniform style. Check section [below](#coding-style-and-conventions)!

---

## How to create an issue

If you find a bug, an error, or you want simply to suggest an improvement to the code or structure of the repository you can **file an issue**. You find the Issue panel on the top part of the repository view. When you create an issue, keep in mind the following points:
* Write a **clear and concise title**: what is the goal (fix bug or improvement), what is the general goal?
* Be clear about **where the problem is located**: specify folders and files.
* If the issue is about a bug you can use the following pattern: expected behaviour - actual behaviour - steps to reproduce the behaviour
* If available **include error messages, stack traces, images, screenshots**. Copy-pasting the code or error message is often preferred over a screenshot since it allows to easily copy the text.
* Use [**GitHub-flavoured Markdown**](https://guides.github.com/features/mastering-markdown/) to write better issues.

---

## Coding style and conventions

When writing Elixir code try to keep a modular and clean coding style. Check the other files in the repository as examples.
Here you will find some basic coding conventions we decided to adopt. We are following the [styleguide by Christopher Adams](https://github.com/christopheradams/elixir_style_guide) which you can consult for more extensive examples.

*In some cases these conventions are not adopted in the repository for pedagogical reasons. For example we avoid some forms of declaring functions in the exercises that occur early in the course since students may not be accustomed at all with the language.*

#### Naming

* Use `snake_case` for atoms, functions and variables.

  ```elixir
  :some_atom

  some_var = 5

  def some_function do
    ...
  end
  ```

* Use `snake_case` file names for `CamelCase` module names.

  ```elixir
  # file is called some_module.ex

  defmodule SomeModule do
  end
  ```
  
#### Source Code Layout

* Use two **spaces** per indentation level.
  No hard tabs.

  ```elixir
  def some_function do
    do_something
  end
  ```

* Use Unix-style line endings (\*BSD/Solaris/Linux/OSX users are covered by
  default, Windows users have to be extra careful).
  
* Use spaces around operators, after commas, colons and semicolons.
  Do not put spaces around matched pairs like brackets, parentheses, etc.
  Whitespace might be (mostly) irrelevant to the Elixir runtime, but its proper
  use is the key to writing easily readable code.

  ```elixir
  sum = 1 + 2
  {a, b} = {2, 3}
  [first | rest] = [1, 2, 3]
  Enum.map(["one", <<"two">>, "three"], fn num -> IO.puts(num) end)
  ```

* Do not use spaces after non-word operators that only take one argument; or
  around the range operator.

  ```elixir
  0 - 1 == -1
  ^pinned = some_func()
  5 in 1..10
  ```

* Use blank lines between `def`s to break up a function into logical
  paragraphs.

  ```elixir
  def some_function(some_data) do
    some_data |> other_function() |> List.first()
  end

  def some_function do
    result
  end

  def some_other_function do
    another_result
  end

  def a_longer_function do
    one
    two

    three
    four
  end
  ```

* Run single-line `def`s that match for the same function together, but separate
  multiline `def`s with a blank line.

  ```elixir
  def some_function(nil), do: {:error, "No Value"}
  def some_function([]), do: :ok

  def some_function([first | rest]) do
    some_function(rest)
  end
  ```

* If you have more than one multiline `def`, do not use single-line `def`s.
 
  ```elixir
  def some_function(nil) do
    {:error, "No Value"}
  end

  def some_function([]) do
    :ok
  end

  def some_function([first | rest]) do
    some_function(rest)
  end

  def some_function([first | rest], opts) do
    some_function(rest, opts)
  end
  ```

* If a list, map, or struct spans multiple lines, put each element, as well as
  the opening and closing brackets, on its own line.
  Indent each element one level, but not the brackets.
  When assigning a list, map, or struct, keep the opening bracket on the same
  line as the assignment.
 
  ```elixir
  list = [
    :first_item,
    :second_item,
    :next_item,
    :final_item
  ]
  ```
  
* Avoid trailing whitespace.
  
* End each file with a newline.

#### Syntax

* Use parentheses when a `def` has arguments, and omit them when it doesn't.

  ```elixir
  def some_function(arg1, arg2) do
    # body omitted
  end

  def some_function do
    # body omitted
  end
  ```

* Use `true` as the last condition of the `cond` special form when you need a
  clause that always matches.

  ```elixir
  cond do
    1 + 2 == 5 ->
      "Nope"

    1 + 3 == 5 ->
      "Uh, uh"

    true ->
      "OK"
  end
  ```

* Never put a space between a function name and the opening parenthesis.

  ```elixir
  f(3 + 2)
  ```

* Use parentheses in function calls, especially inside a pipeline.

  ```elixir
  f(3)

  2 |> rem(3) |> g
  ```

* Always use the special syntax for keyword lists.

  ```elixir
  some_value = [a: "baz", b: "qux"]
  ```
  
  #### Comments

* Write expressive code and try to convey your program's intention through
  control-flow, structure and naming.

* Place comments above the line they comment on. Use one space between the leading `#` character of the comment and the text of the comment.

  ```elixir
  # comment
  String.first(some_string)
  ```

* Comments longer than a word are capitalized, and sentences use punctuation.
  Use [one space][Sentence Spacing] after periods.
 
  ```elixir
  # Capitalization example
  # Use punctuation for complete sentences.
  ```


