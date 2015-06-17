<div class="right">
  <a href="https://github.com/fniessen/refcard-org-babel/blob/master/README.org" class="fa fa-github"> Edit on GitHub</a>
</div>

<a href="http://opensource.org/licenses/GPL-3.0">
  <img src="http://img.shields.io/:license-gpl-blue.svg" alt=":license-gpl-blue.svg" />
</a>

<a href="https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=VCVAS6KPDQ4JC&lc=BE&item_number=refcard%2dorg%2dbabel&currency_code=EUR&bn=PP%2dDonationsBF%3abtn_donate_LG%2egif%3aNonHosted">
  <img src="https://www.paypalobjects.com/en_US/i/btn/btn_donate_LG.gif" alt="btn_donate_LG.gif" />
</a>

Welcome to Org Babel reference card. It contains the reference documentation that describes how to perform **code execution** within Org mode documents using Org Babel 8.

In a nutshell, Org Babel is like Sweave (for [reproducible research](id:1663ff41-af51-4b07-abc8-6bfed9395b2b)) but it takes a **large number of possible languages** (C, Java, Python, Ruby, R, ...) and Org mode can produce **HTML** as well as **PDF**.

Prolonged use may cause addiction!

Compendium principles
=====================

Literate Programming
--------------------

**Literate programming** (LP) offers 2 classical operations:

Tangle  
Extract the source code blocks and **generate real working code** files for further compilation or execution, eventually outside of Emacs.

Weave  
**Export** the whole Org file as literate, **human-readable documentation** (generally in HTML or LaTeX).

Reproducible Research
---------------------

Above those, Org Babel adds *in situ* code evaluation:

-   during **interactive** use (in the Org buffer itself),
-   during **tangle**, and/or
-   during **weave** (code blocks with `:exports` set to `results` or `both`)

This allows you to insert in your Org document:

-   all **data** (that can reasonably be included),
-   all **code** you used, and
-   the full set of **outputs** you got,

following the principles of **reproducible research** (RR).

------------------------------------------------------------------------

Code evaluation
===============

Org mode basically just runs the code every time you export the document. But, if you've changed some code and want a refresh, you can press [`C-c C-v C-b`](id:b1b4977c-5c27-4385-a9ce-2fb8346822b5) and it will run it for sure then.

Embedding code blocks
---------------------

A **code block** is some sort of subprogram which does the desired job.

### Defining a code block

You can **define** and **call** it at the same time: the code block definition itself acts as an **implicit call**.

#### Syntax

The code block is a block element which can be **anonymous** (without a label) or **named** (with a label).

``` org
#+name: <LABEL>
#+begin_src <LANGUAGE> <HEADER-ARGS>
<BODY>
#+end_src
```

Anonymous code blocks will be immediately followed by the results block upon evaluation.

``` shell
printf "I'm anonymous"
```

``` example
I'm anonymous
```

Named source code blocks will refresh the corresponding named **results blocks anywhere** in the file.

``` shell
printf "As I'm named, my results may live anywhere in the file."
```

It doesn't matter whether the code block and the results block are "disconnected", such as here, as the results is a **named data** which Babel can locate.

``` example
As I'm named, my results may live anywhere in the file.
```

The name can be 20 characters long, and contain...XXX

#### Language

The following language strings are currently recognized:

:RESULTS: Awk, C, R, Asymptote, Calc, Clojure, CSS, Ditaa, Dot, Emacs Lisp, Fortran, Gnuplot, Haskell, IO, J, Java, Javascript, LaTeX, Ledger, Lilypond, Lisp, Makefile, Maxima, Matlab, Mscgen, Ocaml, Octave, Org, Perl, Pico Lisp, PlantUML, Python, Ruby, Sass, Scala, Scheme, Screen, Shell Script, Shen, Sql, Sqlite, ebnf2ps. :END:

You can also add support for new languages:

``` commonlisp
(add-to-list 'org-src-lang-modes '("<LANGUAGE>" . "<MAJOR-MODE>"))
```

so that font lock and editing source do work.

XXX Currently, a `bash` code block will be run with `bash`, and a `shell` code block will be run with `sh`. Both will use `shell-script-mode`. XXX

#### Code block arguments

You can create a code block with optional parameters by specifying a **default value** for optional parameters. When the code block is executed, the default value is used if no other value has been specified in the call.

The way to define arguments is to declare them on the `#+begin_src` line.

``` commonlisp
(* x x)
```

Specifying default values is necessary because each variable must be initialized when the code block is executed.

If the variable semantics vary by language (as they do), just say so (e.g. when defining a SQL function, vars are substituted into the body by prefixing the names with $, but in python they are local vars in non-session mode and global vars in session mode and so on. Maybe this doc section shouldn't try to enumerate all those languages, but just redirect to the proper Worg Babel language page for details on arg handling.

Pass by value.

##### Keyword arguments

##### Default arguments

#### Scope of Variables

##### Global variables

##### Local variables

#### Remarks

``` commonlisp
(setq  org-babel-min-lines-for-block-output 10)
(print 1)
```

``` commonlisp
(setq  org-babel-min-lines-for-block-output 0)
(print 1)
```

``` example
1
```

#### Examples

##### Using :headers

Code can (possibly) be easier to read/write when splitting header arguments among multiple lines, by writing the options above the code block.

``` org
#+header: :file test.png :fit yes
#+header: :imagemagick yes :iminoptions -density 600 :imoutoptions -geometry 400
#+header: :results (if (and (boundp 'backend) (eq backend 'latex)) "latex" "file")
#+begin_src latex :exports results :noweb yes :headers '("\\usepackage{tikz}")
  \begin{tikzpicture}
    \node[red!50!black] (a) {A};
    \node (b) [right of=a] {B};
    \draw[->] (a) -- (b);
  \end{tikzpicture}
#+end_src
```

##### Do stuff conditional to the export backend

Maybe you could do something like the following...

``` commonlisp
(message "do stuff")
```

##### Backend-conditional results

You can replace

``` example
(:results . "html")
```

with

``` example
(:results . (or (and org-export-current-backend "html")  "none"))
```

in the `defvar` to get the desired result.

##### Cross-referencing a results block

\>\> \#+results are never used for cross-references. This is a Babel internal \>\> keyword used to refer to the source that generated this element. \>\> \>\> Cross-references only react to \#+name keyword. \> \> Sorry, this is confusing. Is it then the case that we are naming the source \> block to ensure that captions stick to the corresponding results block?

Source block captions apply to the source block, not to the results. You have to define a separate caption for the results.

Source block name will be used both as a label for cross referencing and as a Babel internal code for results correspondence.

\> And then, we need to separately name the results block, and use \> a different name for it, so that the cross-references pick it up \> correctly?

Yes, the name given to the results block doesn't depend on the results keyword. You can give it any name, as long as it is unique.

Here is an example:

<span class="label">Src block caption</span>
``` commonlisp
(+ 1 2)
```

``` example
3
```

##### Other explanation

You need to apply `caption` and `name` keywords on the results, not the source code.

### Calling a code block

You can **define** a code block somewhere and then **call** it **explicitly** elsewhere --- provided the code block has a `#+name:` meta data to label it.

#### Syntax

`#+call:` is for **standalone lines**: it lives on a block by itself.

A `#+call:` line can be **named**, in order for its **results** (for the arguments used) to be **referenced**.

It has the following syntax, where each header argument portion is optional.

``` org
#+name: <CALL-LINE-NAME>
#+call: <NAME>[<HEADER-ARGS-FOR-BLOCK>](<ARGUMENTS>) <HEADER-ARGS-FOR-CALL-LINE>
```

No square bracket for the "end header arguments"!

NAME  
Name of the code block to be evaluated.

ARGUMENTS  
XXX Describe how to pass args.

HEADER-ARGS-FOR-BLOCK ("inside header argument")  
Header arguments applied to the **evaluation of the code block**. They *affect how the code block is evaluated*: they **change the inputs**.

For example, `:session *org-R*` or `:results output`.

HEADER-ARGS-FOR-CALL-LINE ("end header argument")  
Header arguments applied to the **evaluation of the \#+call: line**. They do not affect evaluation of the named code block; instead, they *affect how the results are incorporated* into the Org mode buffer.

For example, `:exports results` or `:results html`.

#### Remarks

`#+call:` lines recently got `#+names`, hopefully soon they will get `#+header` arguments as well.

Press `C-c C-v C-e` on the **call line** to **execute** the block.

When **evaluating a call line**, it is converted into an ephemeral Emacs Lisp code block equivalent to the call line (and created at the point of the call line):

``` org
#+begin_src emacs-lisp :var result=<NAME>(<ARGUMENTS>) <HEADER-ARGS-FOR-BLOCK>
,  result
#+end_src
```

which is evaluated in place.

The result of the called function is passed into this ephemeral block, and the output of the block is inserted into the buffer.

This is why call lines have **two** possible sets of header arguments:
-   one to pass header arguments to the original code block being called, and
-   one for local effect in the ephemeral block.

    Advice (from Rick Frankel): As to the "call" lines, think of the output of the "called" block as being input to an anonymous block (the \#+call), so the hlines are stripped.

Code blocks are sometimes located in a separate file (called "library of Babel") which can be included in other Org files that wish to use the code blocks.

The result of named code blocks evaluated with a `#+call:` line is wrapped according to the value of `org-babel-inline-result-wrap`, which by default is

``` example
"=%s="
```

for markup that produces *verbatim* text.

#### Examples

##### Relying on the default value of the arguments

##### Providing explicit values to the arguments

``` org
#+call: foo(bar=1)
```

is equivalent to

``` org
#+begin_src emacs-lisp :var results=foo(bar=1)
,  results
#+end_src
```

##### Recursive

``` commonlisp
  (+ x 1)
```

``` example
5
```

``` example
7
```

``` example
11
```

##### Other

It is possible to pass the `:dir` header argument through a call line.

``` shell
  pwd
```

``` example
/tmp
```

Call the above from somewhere else.

``` example
/
```

##### Call by name

Let's assume, the original code block takes an argument.

``` shell
  echo "input=$input"
```

``` example
input=original
```

If I want to "get rid of" that argument (to avoid typing), I can to name the result of calling that code block with a specific argument.

``` example
input=new
```

As `#+call:` lines can be named, it is possible to reference that result.

``` shell
  echo "this=$input"
```

##### Raw results

\>\> \#+call: org-figure-to-slide[:exports none :results raw]() \> \> Does the following call line do what you want? \> \> \#+call: org-figure-to-slide() :results raw

No square bracket for the "end header arguments"!

Thanks for your quick answer. Nevertheless, adding :results raw at the end changes the formatting output but embeds everything within paren. Given your advice, I am pretty closed to what I want to do by adding another :results raw command either as an inside header arguments or directly when declaring the `org-figure-to-slide` code like

``` shell
# ...
```

### Calling a code block from other elements

Using the `org-sbe` (for "source block evaluate") macro, you may call arbitrary code blocks

-   in a **table formula**,
-   in **file local variables**,
-   inside of an **elisp link**, or
-   in any **[header argument](id:e9f52887-3230-4b26-98a2-97169036b1d0)**.

#### Syntax

Return the results of calling `NAME` with `VARIABLES`.

``` org
(org-sbe <NAME> <VARIABLES>)
```

Don't quote the `NAME` (or, optionally, double quote it).

`(org-sbe 'foo)` is wrong.

Each element of `VARIABLES` should be a two element list, whose

-   first element is the name of the variable and
-   second element is a **string** of its value.

By default, string variable names are interpreted as references to source-code blocks. To force interpretation of a cell's value as a string, prefix the identifier with a `$` (e.g., `$$2` instead of `$2` or `$@2$2` instead of `@2$2`).

Babel apparently supports (undocumented) "filename:reference" syntax for foreign references. In your case, "tab:my<sub>data</sub>" is mistakenly seen as a reference to "my<sub>data</sub>" in the file "tab".

In order to differentiate between strings and reference names, we surround all strings in double quotes...

From [Re: {Orgmode} {babel} passing strings in - msg\#00651 - emacs-orgmode-gnu](http://osdir.com/ml/emacs-orgmode-gnu/2010-03/msg00651.html)

... or double the $ sign: \[1

#+name: leftover
#+begin_src emacs-lisp :var prefix=""
 (-
  ;; length w/o .el
  (- 13 (length ".el"))
  ;; length of prefix
  (length prefix))
#+end_src

| prefix   | remaining characters |
|----------+----------------------|
| ob-      |                  nil |
| org-b-   |                  nil |
| orgb-    |                  nil |
| org-bbl- |                  nil |
| bbl-     |                  nil |
| babel-   |                  nil |
#+TBLFM: $2='(org-sbe leftover (prefix \]1))

#### Remarks

``` org
(org-sbe NAME (n $2) (m 3))
```

The preceding call to `org-sbe` is equivalent to the following source code block.

``` org
#+begin_src emacs-lisp :var results=NAME(n=val_at_col_2, m=3) :results silent
results
#+end_src
```

There is no way to **map** a **code block** over the **rows of a table**. You could use the `org-sbe` macro and a spreadsheet formula to call a code block on multiple table cells, but the results would be inserted back into the table.

|---------|-----|
| A       | nil |
| B       | nil |
| eric    | nil |
| schulte | nil |

`:results org`  
(implies `verbatim` results, unless otherwise stated)
 **Wraps raw Org mode results** in a `#+begin_src org` block (dead data, **comma-escaped lines**). This will be **exported as Org code block** (as `verbatim` or `listings` to LaTeX).

\#

`:results drawer`  
(replacement of `:results wrap`, since Org 7.9.2)
 (implies `verbatim` results, unless otherwise stated)
 **Wraps code block results** in a `RESULTS` drawer. This has the benefit of delimiting your results, and can be useful for really inserting pure **Org mode syntax** in the buffer (live data, **not escaped**) --- which can be included correctly **in all export targets**.

You can put anything in your drawer besides a headline and another drawer.

Why are drawers not the default for results? Because the block may insert a headline, or another drawer, within the results drawer, thus breaking the document!

\#+begin<sub>src</sub> shell :results drawer :exports both cat \<\<EOF

| a   | b   |
|-----|-----|
| 1   | 2   |

[./plots/file1.pdf](./plots/file1.pdf)

[./plots/file2.pdf](./plots/file2.pdf) EOF

\#+end<sub>src</sub>

:RESULTS:

| a   | b   |
|-----|-----|
| 1   | 2   |

[./plots/file1.pdf](./plots/file1.pdf)

[./plots/file2.pdf](./plots/file2.pdf) :END:

`:results code`  
This will be **exported as \<LANG\> code block** (as `verbatim` or `listings` to LaTeX).

\#

`:results pp`  
Prints data. Puts the output in an `EXAMPLE` block? XXX

You might try

``` commonlisp
(cons (org-entry-get (point) "header-args" t)
      (nth 2 (org-babel-get-src-block-info)))
```

\#

\#+begin<sub>verse</sub> \>\> \#+begin<sub>src</sub> R :results pp replace \>\> c("**\* New header2", "[./graph1.pdf](./graph1.pdf)", "", "\*** and second header", "and some text" ) \>\> \#+end<sub>src</sub> \>\> \>\> \#+results: \>\> : \*\* New header2 \>\> : [./graph1.pdf](./graph1.pdf) \>\> : \>\> : \*\* and second header \>\> : and some text

\>\>\>\> I don't quite understand your code below and what it is supposed to do. \>\>\> \>\>\> Drawers are probably better in most cases, but this code lets you use \>\>\> ':results pp replace' while developing, but converts the fixed-width pp \>\>\> results into raw results when desired (e.g. in a buffer copy before some \>\>\> action is taken). \>\> \>\> OK - I see. But what is the advantage in this approach? Drawer do \>\> perfectly what I want, i.e. encapsulating an org structure in a for \>\> replacement when re-calculated, inside the drawer they behave like \>\> normal org structure (folding, ...) and upon export they are exported as \>\> org code would be - or am I missing something? \> \> There are none (for you) then, but it might be useful anyway in some \> situations, at least the results look more like part of the document \> when you care about the looks of the org file too (not only the export \> results).

\#+end<sub>verse</sub>

### Remarks

With `raw`, as there are **no obvious markers to delimit the results** in the Org mode buffer, there is no way to know where raw results begin or end. So, `:results
raw replace` will never work as expected: **raw results cannot be removed** (they become part of the buffer), and you'll get duplicates when re-evaluating the code block.

The sole purpose of `raw` results is to allow inserting an headline (a real headline, not comma protected) in the buffer, because headlines cannot be contained in anything else than headlines.

`raw` behaviour is only "useful" (i.e., mandatory) when you want to insert a headline (or a drawer) as a result of a code block evaluation. But then, you lose the ability to replace results.

If the generated code doesn't contain an headline, you don't need `raw` results.

In any other case, `drawer+replace` is the superior choice.

The **best and simplest solution** for automatically generating org headlines for export is...

...never to execute the source block by hand in org source file. That way, the results will appear only in the **temporary** copy of the buffer is parsed for export and one does need to worry about demarcating the output with a :RESULTS: drawer...

Workaround: a block wrapper makes it possible for the entirety of the results to be clearly located, regardless of their contents, and replaced (with new results) by Babel upon code block re-evaluation.

No matter how special the results drawer is, it cannot (and shouldn't) contain headlines.

\#+begin<sub>verse</sub> \> Or wrap the results in a drawer when you type C-c C-c, but render them as \> raw on export (which removes the drawer and replaces with raw results). \> \> Like so: \> \> \#+header: :results (if (boundp 'backend) "raw" "drawer") \> \#+begin<sub>src</sub> emacs-lisp :exports both \> \> (format "\* headline125") \> \#+end<sub>src</sub>

That's a very nice tip - one small weakness is that it'll do the wrong thing if you just happen to have a binding for "backend" outside of the export mechanism.

\#+end<sub>verse</sub>

A naked "`tildes`" will be marked up as "`tildes`" under `:results raw` or `:results latex raw`, and as "`tildes`" under `:results latex`.

### Examples

#### Interpreting the results as a file path

### See also

[:exports](#exports)
[:file](#file)
[:wrap](#wrap)

------------------------------------------------------------------------

:rownames
---------

Handles row names in tables.

### Options

`:rownames no`  
(default)

`:rownames yes`  
Tells Org that your first column contains row names.

### See also

[:colnames](#colnames)
[:wrap](#wrap)

------------------------------------------------------------------------

:sep
----

Specifies a delimiter for reading or writing **tabular results**.

### Options

nothing  
Uses TAB as default separator.

`:sep <SEPARATOR>`  
Sets separator to `<SEPARATOR>`.

### Examples

#### Saving the tabular output to a CSV file

Save the output of `ls -l` as a `.csv` file.

``` shell
ls -l
```

[file:dirlisting.csv](dirlisting.csv)

Recall that `:results value` is the default.

### See also

[:file](#file)

------------------------------------------------------------------------

:session
--------

Shares data and persists state between (evaluation of) different code blocks.

### Options

`:session none`  
Disables session-based evaluation.
 (default)

`:session <NAME>`  
Performs evaluation using a persistently running inferior process to which the code block is submitted.
 (default for Screen code blocks: session name set to `default`)

### Remarks

-   If a code block has a `:session` header argument, then it is assumed that the block should be evaluated regardless of its `:exports` header argument, because it could change the state of the session.

-   Even if you don't need to share data between blocks, you can still use sessions in order not to wait for the interpreter to start up for each block!

-   This also allows for manual inspection of the results of evaluation.

-   When you work on different files at the same time, you may want to use:
    -   the same session, or
    -   differently named sessions (running their own process).
-   Adding session to a shell source block has the following impact: commands will be run from `~` directory (instead of the local one).

### See also

[:eval](#eval)
[:exports](#exports)

------------------------------------------------------------------------

:shebang
--------

Uses preamble for tangled files (and **make** them **executable**).

### Options

`:shebang <SHEBANG>`  
Specifies the shebang.

### Remarks

The preamble line is only used for tangling, not during evaluation.

Note that whenever a file is tangled which includes a shebang line, Org Babel will make the file executable, so there is good reason to **only add shebangs at the source-code level**.

### Examples

Set the shebang.

    printf "with a shebang line, I can be run as a script!\n"

|---------------------|---------------------------|
| with a shebang line | I can be run as a script! |

------------------------------------------------------------------------

:tangle
-------

Toggles tangling and specify file name.

### Options

`:tangle no`  
(default)

`:tangle yes`  
Tangles to a target file named after the name of the Org mode file (`$(basename).<MODE-EXT>`).

`:tangle <FILENAME>`  
Specifies an alternate target file.

### Remarks

Tangling works for **any** programming language (even those which have yet to be created and have no explicit Emacs or Org mode support) because, on tangling, the code block is simply treated as text.

Blocks to the same target file are **concatenated** during tangling, IN THE ORDER AT WHICH THEY APPEAR IN THE ORG SOURCE FILE.

Blocks which are under a `COMMENT`'ed heading (including parents) are not tangled.

Propagating changes back from tangled code to Org mode blocks (aka "**detangling**") is possible with the function `org-babel-detangle`.

### See also

[:noweb](#noweb)

------------------------------------------------------------------------

:tangle-mode
------------

Controls the permissions of tangled files.

### Example

    plot(1)

------------------------------------------------------------------------

:var
----

**Passes arguments** to code blocks.

### Options

`:var <NAME>=<VALUE>`  
Assigns a **default** value (literal or reference to a literal, a table, a list or a code block) to the argument.

### Remarks

Multiple `var` specifications behind a single `:var` are allowed. The multiple var arguments must be comma-separated:

``` org
#+PROPERTY: var foo=1, bar=2

#+begin_src emacs-lisp
(+ foo bar)
#+end_src

#+results:
: 3
```

and

``` org
#+begin_src emacs-lisp :var foo="hello", bar="world"
(concat foo " " bar)
#+end_src

#+results:
: hello world
```

############### TODO Question about prefix char

So, the prefix char of such lines should be `|`, `:` or `-`?

############### END

The value passed to an argument can be:

-   a **literal** value:
    -   string (value wrapped in double quotes)
    -   number
-   a **reference** to **named data**, such as:
    -   literal **example block**
    -   (part of) a **table** (when naming a table, `#+name:` should replace `#+tblname:`, which still exists for backwards compatibility)
    -   (level-1) **list**
-   a **reference** to the results of a **named code block**

-   **Emacs Lisp code**

    Look in the manual at the description of what causes header arguments to be interpreted as Emacs Lisp.

    **Note --** Eric means about '(...) or (...)

    Arbitrary Emacs Lisp can be placed inside of header arguments, and the `org-sbe` take the name of a code block and returns its results.

To specify a **subset** of a table, give a `[row,column]` index.

-   Indices are numerical and begin at 0.
-   Column names, hlines and row names do count in the indexes.
-   Negative numbers imply counting back from the end.
-   Ranges (consecutive rows or columns) are specified using the colon notation `first:last`.
-   If row or column is empty, then all rows or all columns are taken.

To reference a variable in an external file, use the syntax `:var
data=<FILE>:<VAR>`. Note that, in order to work, the files must be in the same directory.

`:var` introduces code block dependencies. include anything in the Org mode file that takes a `#+name:`, `#+begin_example` blocks, other code blocks, and the results of other code blocks. When a reference is made to another code block, the referenced block will be evaluated whenever needed, in order to supply its value to the referencing block. If the referenced block is cached (see @ref{cache}), its value will be reused if possible, instead of being re-calculated. If the referring code block is cached, its hash value will depend on the value of all the code blocks it references.

### Examples

#### Literal string

``` example
value
```

#### Literal number

``` example
42
```

#### Reference to a literal example block

``` example
Les sanglots longs
des violons de l'automne
```

|--------------------------|
| Les sanglots longs       |
| des violons de l'automne |

#### Reference to part of a table

|-----|------|------|
| 0   | foo1 | bar1 |
| 1   | foo2 | bar2 |
| 2   | foo3 | bar3 |

|-----|------|------|
| 1   | foo2 | bar2 |
| 2   | foo3 | bar3 |

|-----|------|------|
| 1   | foo2 | bar2 |
| 2   | foo3 | bar3 |

| id  | var1 | var2 |
|-----|------|------|
| 0   | foo1 | bar1 |
| 1   | foo2 | bar2 |
| 2   | foo3 | bar3 |

``` example
hline
```

#### Reference to a list

-   foo
-   bar
-   baz

|-----|-----|-----|
| foo | bar | baz |

#### Reference to a code block

It is possible to **chain code blocks** (possibly in different languages) in `:var` lines, as shown:

|-----|-----|-----|-----|
| 1   | 4   | 7   | 10  |
| 2   | 5   | 8   | 11  |
| 3   | 6   | 9   | 12  |

``` r
rowSums(x)
```

|-----|
| 22  |
| 26  |
| 30  |

------------------------------------------------------------------------

:wrap
-----

Delimit the results (of source block evaluation).

### Options

nothing  
Tells Org Babel **not** to **wrap** the results.
 (default)

`:wrap`  
Specifies to wrap the results in a `#+begin/end_results` block.

`:wrap <MARKUP>`  
Specifies the name of the block (`#+begin/end_<markup>`) with which to wrap the results.

\*XXX How to unset it (when set in a higher level)?\*

### Remarks

The `:wrap` header argument gives you control over the formatting of results from code blocks.

### Examples

#### Using `:wrap` with no value

The following example wraps the results in `#+begin_results` ... `#+end_results` block.

    x <- 1:12 ; dim(x) <- c(3,4)
    x

|-----|-----|-----|-----|
| 1   | 4   | 7   | 10  |
| 2   | 5   | 8   | 11  |
| 3   | 6   | 9   | 12  |

#### Using `:wrap` with a string value

The following examples puts the **output in an `example` block**.

``` example
Some results wrapped in an example block.
```

#### Using `:wrap` to produce a source code block in a named language

The following examples puts the results into a `SRC` code block (associated to the `c++` mode).

\#

On LaTeX export, the `SRC` block will be exported as `c++` code under `listings` instead of being wrapped in a `\begin{verbatim}..\end{verbatim}` environment.

Alternatively, you can use the :post header argument to wrap the results in a source block.

\#

\#

### See also

[:results](#results)

------------------------------------------------------------------------

Extra language-specific header arguments
========================================

The following table lists the header arguments that some languages use.

|----------------|-----------------------------|
| :border        | :java                       |
| :buffer        | :main                       |
| :classname     | :nullvalue                  |
| :cmd           | :out-file                   |
| :cmd-line      | :package                    |
| :cmpflag       | :packages                   |
| :colname-names | :pdf                        |
| :database      | :pdfheight                  |
| :db            | :pdfpng                     |
| :dbhost        | :pdfwidth                   |
| :dbpassword    | [:preamble](#preamble)      |
| :dbuser        | :result-params              |
| :defines       | :result-type                |
| :engine        | :results<sub>switches</sub> |
| :eps           | :return                     |
| :filetype      | :rowname-names              |
| :fit           | :scheme                     |
| :flags         | [:separator](#separator)    |
| :headers       | **:stdin**                  |
| :imagemagick   | :tangle                     |
| :iminoptions   | :term                       |
| :imoutoptions  | :terminal                   |
| :in-file       | :xmp-option                 |
| **:includes**  |                             |

XXX Title disappears!?

:preamble
---------

Specifies code block prefix for code evaluation.

:separator
----------

Specifies a different results separator.

------------------------------------------------------------------------

Data types
==========

------------------------------------------------------------------------

Hooks
=====

`org-babel-post-tangle-hook`  
When the hook is executed, the current buffer (as identified by the `current-buffer` function) will be a buffer visiting the **file** of the **tangled code**.

Example applications could include post-processing, compilation or evaluation of tangled code files.

------------------------------------------------------------------------

Subtleties
==========

Shell mode
----------

\#+begin<sub>verse</sub> \> Why is the \`diff' command alone not exported while the piped \`diff' via \> \`cat' works?

The exit code is not zero since diff found differences, so Babel assumes the script produced an error. Try this instead when you don't know what the return code of the last command will be or if you know that it isn't zero even when no error occured:

\> Is this this a bug?

I don't think so, although Babel could give a more enlightening message about why it didn't evaluate STDOUT. It gives this on your original example:

Babel evaluation exited with code 1 Code block produced no output.

\#+end<sub>verse</sub>

------------------------------------------------------------------------

Colophon
========

References
----------

The primary sources for this documentation are:

-   [The Org Manual](http://orgmode.org/org.pdf)

-   [Worg](http://orgmode.org/worg/) site

-   Mailing list <emacs-orgmode@gnu.org>

Contributing
------------

### Issues

Report issues and suggest features and improvements on the [GitHub issue tracker](https://github.com/fniessen/refcard-org-babel /issues/new).

### Patches

I love contributions! Patches under any form are always welcome!

### Donations

If you like the refcard-org-babel project, you can show your appreciation and help support future development by making a [donation](https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=VCVAS6KPDQ4JC&lc=BE&item_number=refcard%2dorg%2dbabel&currency_code=EUR&bn=PP%2dDonationsBF%3abtn_donate_LG%2egif%3aNonHosted) through PayPal.

Regardless of the donations, refcard-org-babel will always be free both as in beer and as in speech.

License
-------

Copyright (C) 2014-2015 Free Software Foundation, Inc.

Author: Fabrice Niessen
Keywords: org-mode org-babel reference card

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program. If not, see <http://www.gnu.org/licenses/>.

Change Log
----------

<a href="http://opensource.org/licenses/GPL-3.0">
  <img src="http://img.shields.io/:license-gpl-blue.svg" alt=":license-gpl-blue.svg" />
</a>

<a href="https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=VCVAS6KPDQ4JC&lc=BE&item_number=refcard%2dorg%2dbabel&currency_code=EUR&bn=PP%2dDonationsBF%3abtn_donate_LG%2egif%3aNonHosted">
  <img src="https://www.paypalobjects.com/en_US/i/btn/btn_donate_LG.gif" alt="btn_donate_LG.gif" />
</a>


