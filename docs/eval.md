Org mode basically just runs the code every time you export the document.  But,
if you&rsquo;ve changed some code and want a refresh, you can press [`C-c C-v C-b`](key-bindings.md) and
it will run it for sure then.

# Embedding code blocks

A **code block** is some sort of subprogram which does the desired job.

## Defining a code block

You can **define** and **call** it at the same time: the code block definition itself
acts as an **implicit call**.

### Syntax

The code block is a block element which can be **anonymous** (without a label) or
**named** (with a label).

    #+name: <LABEL>
    #+begin_src <LANGUAGE> <HEADER-ARGS>
    <BODY>
    #+end_src

Anonymous code blocks will be immediately followed by the results block upon
evaluation.

    printf "I'm anonymous"

    I'm anonymous

Named source code blocks will refresh the corresponding named **results blocks
anywhere** in the file.

    printf "As I'm named, my results may live anywhere in the file."

It doesn&rsquo;t matter whether the code block and the results block are
&ldquo;disconnected&rdquo;, such as here, as the results is a **named data** which Babel can
locate.

    As I'm named, my results may live anywhere in the file.

The name can be 20 characters long, and contain&#x2026;XXX

### Language

The following language strings are currently recognized:

Awk, C, R, Asymptote, Calc, Clojure, CSS, Ditaa, Dot, Emacs Lisp, Forth, Fortran, Gnuplot, Haskell, IO, J, Java, Javascript, LaTeX, Ledger, Lilypond, Lisp, Makefile, Maxima, Matlab, Mscgen, Ocaml, Octave, Org, Perl, Pico Lisp, PlantUML, Python, Ruby, Sass, Scala, Scheme, Screen, Shell Script, Shen, Sql, Sqlite, ebnf2ps.

You can also add support for new languages:

    (add-to-list 'org-src-lang-modes '("<LANGUAGE>" . "<MAJOR-MODE>"))

so that font lock and editing source do work.

XXX Currently, a `bash` code block will be run with `bash`, and a `shell` code block will
be run with `sh`.  Both will use `shell-script-mode`. XXX

### Code block arguments

You can create a code block with optional parameters by specifying a **default
value** for optional parameters.  When the code block is executed, the default
value is used if no other value has been specified in the call.

The way to define arguments is to declare them on the `#+begin_src` line.

    (* x x)

Specifying default values is necessary because each variable must be
initialized when the code block is executed.

If the variable semantics vary by language (as they do), just say so (e.g.  when
defining a SQL function, vars are substituted into the body by prefixing the
names with $, but in python they are local vars in non-session mode and global
vars in session mode and so on.  Maybe this doc section shouldn&rsquo;t try to
enumerate all those languages, but just redirect to the proper Worg Babel
language page for details on arg handling.

Pass by value.

1.  Keyword arguments

2.  Default arguments

### Scope of Variables

1.  Global variables

2.  Local variables

### Remarks

    (setq  org-babel-min-lines-for-block-output 10)
    (print 1)

    (setq  org-babel-min-lines-for-block-output 0)
    (print 1)

### Examples

1.  Using :headers

    Code can (possibly) be easier to read/write when splitting header arguments
    among multiple lines, by writing the options above the code block.
    
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

2.  Do stuff conditional to the export backend

    Maybe you could do something like the following&#x2026;
    
        (message "do stuff")

3.  Backend-conditional results

    You can replace
    
        (:results . "html")
    
    with
    
        (:results . (or (and org-export-current-backend "html")  "none"))
    
    in the `defvar` to get the desired result.

4.  Cross-referencing a results block

    >> #+results are never used for cross-references.  This is a Babel internal
    >> keyword used to refer to the source that generated this element.
    >>
    >> Cross-references only react to #+name keyword.
    >
    > Sorry, this is confusing.  Is it then the case that we are naming the source
    > block to ensure that captions stick to the corresponding results block?
    
    Source block captions apply to the source block, not to the results.  You have
    to define a separate caption for the results.
    
    Source block name will be used both as a label for cross referencing and
    as a Babel internal code for results correspondence.
    
    > And then, we need to separately name the results block, and use
    > a different name for it, so that the cross-references pick it up
    > correctly?
    
    Yes, the name given to the results block doesn&rsquo;t depend on the results keyword.
    You can give it any name, as long as it is unique.
    
    Here is an example:
    
        (+ 1 2)

5.  Other explanation

    You need to apply `caption` and `name` keywords on the results, not the source
    code.

## Calling a code block

You can **define** a code block somewhere and then **call** it **explicitly** elsewhere
&#x2014; provided the code block has a `#+name:` meta data to label it.

### Syntax

`#+call:` is for **standalone lines**: it lives on a block by itself.

A `#+call:` line can be **named**, in order for its **results** (for the arguments used)
to be **referenced**.

It has the following syntax, where each header argument portion is optional.

    #+name: <CALL-LINE-NAME>
    #+call: <NAME>[<HEADER-ARGS-FOR-BLOCK>](<ARGUMENTS>) <HEADER-ARGS-FOR-CALL-LINE>

<div class="warning">
No square bracket for the &ldquo;end header arguments&rdquo;!

</div>

-   **NAME:** Name of the code block to be evaluated.

-   **ARGUMENTS:** XXX Describe how to pass args.

-   **HEADER-ARGS-FOR-BLOCK (&ldquo;inside header argument&rdquo;):** Header arguments applied to the **evaluation of the code block**.  They
    *affect how the code block is evaluated*: they **change the inputs**.
    
    For example, `:session *org-R*` or `:results output`.

-   **HEADER-ARGS-FOR-CALL-LINE (&ldquo;end header argument&rdquo;):** Header arguments applied to the **evaluation of the #+call: line**.  They do
    not affect evaluation of the named code block; instead, they *affect how
    the results are incorporated* into the Org mode buffer.
    
    For example, `:exports results` or `:results html`.

### Remarks

`#+call:` lines recently got `#+names`, hopefully soon they will get `#+header`
arguments as well.

Press `C-c C-v C-e` on the **call line** to **execute** the block.

When **evaluating a call line**, it is converted into an ephemeral Emacs Lisp code
block equivalent to the call line (and created at the point of the call line):

    #+begin_src emacs-lisp :var result=<NAME>(<ARGUMENTS>) <HEADER-ARGS-FOR-BLOCK>
    ,  result
    #+end_src

which is evaluated in place.

The result of the called function is passed into this ephemeral block, and the
output of the block is inserted into the buffer.

This is why call lines have **two** possible sets of header arguments:

-   one to pass header arguments to the original code block being called, and
-   one for local effect in the ephemeral block.
    
    Advice (from Rick Frankel):
    As to the &ldquo;call&rdquo; lines, think of the output of the &ldquo;called&rdquo; block as being
    input to an anonymous block (the #+call), so the hlines are stripped.

Code blocks are sometimes located in a separate file (called &ldquo;library of
Babel&rdquo;) which can be included in other Org files that wish to use the code
blocks.

The result of named code blocks evaluated with a `#+call:` line is wrapped
according to the value of `org-babel-inline-result-wrap`, which by default is

    "=%s="

for markup that produces *verbatim* text.

### Examples

1.  Relying on the default value of the arguments

2.  Providing explicit values to the arguments

        #+call: foo(bar=1)
    
    is equivalent to
    
        #+begin_src emacs-lisp :var results=foo(bar=1)
        ,  results
        #+end_src

3.  Recursive

        (+ x 1)
    
        5
    
        7
    
        11

4.  Other

    It is possible to pass the `:dir` header argument through a call line.
    
        pwd
    
    Call the above from somewhere else.
    
        /

5.  Call by name

    Let&rsquo;s assume, the original code block takes an argument.
    
        echo "input=$input"
    
    If I want to &ldquo;get rid of&rdquo; that argument (to avoid typing), I can to name the
    result of calling that code block with a specific argument.
    
        input=new
    
    As `#+call:` lines can be named, it is possible to reference that result.
    
        echo "this=$input"
    
    See [how to name the results of a function call and reuse them](http://emacs.stackexchange.com/questions/12546/with-org-babel-how-to-name-the-results-of-a-function-call-and-reuse-them).

6.  Raw results

    <p class="verse">
    >> #+call: org-figure-to-slide[:exports none :results raw]()<br  />
    ><br  />
    > Does the following call line do what you want?<br  />
    ><br  />
    > #+call: org-figure-to-slide() :results raw<br  />
    </p>
    
    <div class="warning">
    No square bracket for the &ldquo;end header arguments&rdquo;!
    
    </div>
    
    Thanks for your quick answer.  Nevertheless, adding :results raw at the end
    changes the formatting output but embeds everything within paren.  Given your
    advice, I am pretty closed to what I want to do by adding another :results raw
    command either as an inside header arguments or directly when declaring the
    `org-figure-to-slide` code like
    
        # ...

## Calling a code block from other elements

Using the `org-sbe` (for &ldquo;source block evaluate&rdquo;) macro, you may call arbitrary
code blocks

-   in a **table formula**,
-   in **file local variables**,
-   inside of an **elisp link**, or
-   in any **header argument**.

### Syntax

Return the results of calling `NAME` with `VARIABLES`.

    (org-sbe <NAME> <VARIABLES>)

Don&rsquo;t quote the `NAME` (or, optionally, double quote it).

<div class="warning">
`(org-sbe 'foo)` is wrong.

</div>

Each element of `VARIABLES` should be a two element list, whose

-   first element is the name of the variable and
-   second element is a **string** of its value.

By default, string variable names are interpreted as references to source-code
blocks.  To force interpretation of a cell&rsquo;s value as a string, prefix the
identifier with a `$` (e.g., `$$2` instead of `$2` or `$@2$2` instead of
`@2$2`).

<div class="warning">
Babel apparently supports (undocumented) &ldquo;filename:reference&rdquo; syntax for
foreign references. In your case, &ldquo;tab:my\_data&rdquo; is mistakenly seen as
a reference to &ldquo;my\_data&rdquo; in the file &ldquo;tab&rdquo;.

</div>

In order to differentiate between strings and reference names, we surround all
strings in double quotes&#x2026;

From [Re: {Orgmode} {babel} passing strings in - msg#00651 - emacs-orgmode-gnu](http://osdir.com/ml/emacs-orgmode-gnu/2010-03/msg00651.html)

&#x2026; or double the $ sign: $$1

    (-
     ;; length w/o .el
     (- 13 (length ".el"))
     ;; length of prefix
     (length prefix))

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">prefix</th>
<th scope="col" class="org-left">remaining characters</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">ob-</td>
<td class="org-left">nil</td>
</tr>


<tr>
<td class="org-left">org-b-</td>
<td class="org-left">nil</td>
</tr>


<tr>
<td class="org-left">orgb-</td>
<td class="org-left">nil</td>
</tr>


<tr>
<td class="org-left">org-bbl-</td>
<td class="org-left">nil</td>
</tr>


<tr>
<td class="org-left">bbl-</td>
<td class="org-left">nil</td>
</tr>


<tr>
<td class="org-left">babel-</td>
<td class="org-left">nil</td>
</tr>
</tbody>
</table>

### Remarks

    (org-sbe NAME (n $2) (m 3))

The preceding call to `org-sbe` is equivalent to the following source code block.

    #+begin_src emacs-lisp :var results=NAME(n=val_at_col_2, m=3) :results silent
    results
    #+end_src

There is no way to **map** a **code block** over the **rows of a table**.  You could
use the `org-sbe` macro and a spreadsheet formula to call a code block on
multiple table cells, but the results would be inserted back into the table.

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">A</td>
<td class="org-left">nil</td>
</tr>


<tr>
<td class="org-left">B</td>
<td class="org-left">nil</td>
</tr>


<tr>
<td class="org-left">eric</td>
<td class="org-left">nil</td>
</tr>


<tr>
<td class="org-left">schulte</td>
<td class="org-left">nil</td>
</tr>
</tbody>
</table>

    (format "-->%s<--" in)

Also remember you can use ELisp in formulas:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-right" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Date 1</th>
<th scope="col" class="org-left">Date 2</th>
<th scope="col" class="org-right">Duration</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left"><span class="timestamp-wrapper"><span class="timestamp">[2013-12-21 Sat 00:00]</span></span></td>
<td class="org-left"><span class="timestamp-wrapper"><span class="timestamp">[2013-12-22 Sun 00:00]</span></span></td>
<td class="org-right">86400.0</td>
</tr>
</tbody>
</table>

Note the quotes around &ldquo;$2&rdquo; and &ldquo;$1&rdquo;, they are necessary so that the date is
not literally inserted in the formulas when Org is computing it.

1.  Tangle the results of a code blocks

    To tangle #+RESULTS: block, name the block:
    
        (+ 1 2)
    
    and use:
    
        nil

### Examples

1.  Calling a code block in a table formula, relying on the default value of the arguments

2.  Calling a code block in a table formula, providing explicit values to the arguments

        (let ((num (car l))
              (nums (cdr l)))
          (/ (float (+ num (apply #'+ nums)))
             (1+ (length nums))))
    
    <table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">
    
    
    <colgroup>
    <col  class="org-right" />
    
    <col  class="org-right" />
    
    <col  class="org-right" />
    
    <col  class="org-right" />
    </colgroup>
    <thead>
    <tr>
    <th scope="col" class="org-right">x</th>
    <th scope="col" class="org-right">y</th>
    <th scope="col" class="org-right">z</th>
    <th scope="col" class="org-right">mean</th>
    </tr>
    </thead>
    
    <tbody>
    <tr>
    <td class="org-right">2</td>
    <td class="org-right">3</td>
    <td class="org-right">5</td>
    <td class="org-right">0.00</td>
    </tr>
    </tbody>
    </table>

3.  Passing header arguments

    It is also possible to pass header arguments to the code block.  In this case,
    a table cell should hold the string value of the header argument which can then
    be passed before all variables.
    
        (+ x y)
    
    <table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">
    
    
    <colgroup>
    <col  class="org-right" />
    
    <col  class="org-right" />
    
    <col  class="org-left" />
    
    <col  class="org-left" />
    </colgroup>
    <tbody>
    <tr>
    <td class="org-right">1</td>
    <td class="org-right">2</td>
    <td class="org-left">:file nothing.png</td>
    <td class="org-left">nil</td>
    </tr>
    </tbody>
    </table>

4.  Using `org-sbe` in a local variables line

    You can run some preparatory code (in any language) when the file is opened for
    the first time by using `(org-sbe NAME)` (where `NAME` is a double-quoted string)
    in **Local Variables** lines, at the end of the Org file.
    
        #+name: init
        #+begin_src R :session *R*
        # initialize some stuff
        #+end_src
        
        # Local variables:
        # eval: (org-sbe "init")
        # End:
    
    Emacs will evaluate the set-up block(s), after asking for confirmation.

5.  Using `org-sbe` inside of an elisp link

        for i in $(seq 1 $to); do
           printf $i;
        done
    
    Clicking on the following hyperlink will execute the code block.
    
        [[elisp:(org-sbe counter (to "3"))][count to 3]]

6.  Using `org-sbe` to assign header arguments

    You can use arbitrary Lisp forms to assign header arguments.  For example, to
    dynamically **compute the file name**.
    
    The following generates a file which is conditional to the export target:
    
    -   a `.pdf` image for LaTeX export and
    -   a `.png` image for HTML export.
    
        (if (and (boundp 'latexp) latexp)
            "Rplots.pdf"
          "Rplots.png")
    
        x <- seq(-pi, pi, by=0.05)
        plot(x, sin(x))
    
        rev-3.14
    
        x <- seq(-pi, pi, by=0.05)
        plot(x, sin(x))
    
    The following works great.  (note the tick in the `(boundp 'backend)`).
    
    And you could wrap up the extra-long Emacs Lisp in a function or macro in your
    init to avoid the overlength header argument.

# Embedding inline code

You can also evaluate code inline as follows.

## Inline code blocks

An **inline code block** (a.k.a. inline source block) is a code block which is
placed *inline* within textual elements such as paragraphs of text or lists.

Its sole purpose is to include **results inline** in textual elements.

### Syntax

The basic syntax structure for inline code blocks is:

    src_<LANGUAGE>{<BODY>}

without header arguments, and:

    src_<LANGUAGE>[<HEADER-ARGS>]{<BODY>}

with header arguments.

### Remarks

-   **Inline code blocks don't associate themselves with their results**, they are
    **only expected to be evaluated during export**.  They are not intended to be
    executed in the buffer during normal use.

-   Inline code blocks are treated just like code blocks, however they have
    different default header arguments: see
    `org-babel-default-inline-header-args`.

-   If you set the following
    
        (setq org-babel-inline-result-wrap "$%s$")
    
    then inline code snippets will be wrapped into the formatting string.

-   Currently, inline code blocks are not fontified even when
    `org-src-fontify-natively` is non-nil.

-   Inline code block results are replaceable (i.e., removable) &#x2013; since commit
    `85ff663`, on <span class="timestamp-wrapper"><span class="timestamp">[2015-01-29 Thu] </span></span> &#x2013; if they is wrapped in a `{{{results(.*)}}}` macro
    call.
    
    Insert current value in `results` macro possibly wrapping RESULT in an export
    snippet or inline code block first.

### Examples

This file was exported on
.

The answer to 2 + 3 is .

One plus one equals .

Two plus two equals .

A definition returns &ldquo;&rdquo;.

The following code and its result src\_emacs-lisp[:exports both]{(message
&ldquo;foo!&rdquo;)}  should be inline.

The following ``eval `cat ~/.emacs`;`` should also be
inline.

Ibid for `(let ((x 10)) (< (* x 3) 2))` and
`(message "foo!\nbar!")` (as expected?).

## Inline Babel calls

### Syntax

The syntax for inline evaluation of named code blocks is the following, where
each header argument portion is optional (so are the square brackets).

    ... call_<NAME>(<ARGUMENTS>) ...
    ... call_<NAME>[<HEADER-ARGS-FOR-BLOCK>](<ARGUMENTS>)[<HEADER-ARGS-FOR-CALL-LINE>] ...

### Remarks

-   The result of named code blocks evaluated inline within a block of text is
    wrapped according to the value of `org-babel-inline-result-wrap`, which by
    default is
    
        "=%s="
    
    for markup that produces verbatim text.

-   When using `:results raw`, you can use properties so that the `call_foo()` part
    stays a reasonable length:
    
        * Description of the Hawaiian Stone Axes
        
        Here is a call_square(x=4), stuck in the middle of some prose.

-   Babel call results are also replaceable.

### Examples

Simple examples for inline call:

    (* x x)

I should be able to put the output inline using .

Here is a , stuck in the middle of some prose.

The following exports as a normal call line:

Now here is an inline call stuck in the middle of some prose.

This one should not be exported `call_square(x=2)` because it is quoted.

Finally this next one should export, even though it starts a line
because sometimes inline blocks fold with a paragraph.

And, a **call with raw results** should not have
quoted results.

)

Final alternative: here is a , from an inline source
block.

1.  Similar code in three languages

        (+ 1 1)
    
        expr 1 + 1
    
        1 + 1
    
    Will lisp-2 export with a newline?
    
    Will shell-2 export with a newline?
    
    Will r-2 export with a newline?
    
    ---
