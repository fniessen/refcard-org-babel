<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#orgheadline3">Compendium principles</a>
<ul>
<li><a href="#orgheadline1">Literate Programming</a></li>
<li><a href="#orgheadline2">Reproducible Research</a></li>
</ul>
</li>
<li><a href="#orgheadline51">Code evaluation</a>
<ul>
<li><a href="#orgheadline40">Embedding code blocks</a></li>
<li><a href="#orgheadline50">Embedding inline code</a></li>
</ul>
</li>
<li><a href="#orgheadline60">Key bindings</a>
<ul>
<li><a href="#orgheadline52">Help</a></li>
<li><a href="#orgheadline53">Edit the code</a></li>
<li><a href="#orgheadline54">SHA1 hash</a></li>
<li><a href="#orgheadline55">Navigate</a></li>
<li><a href="#orgheadline56">Execute (or &ldquo;evaluate&rdquo;)</a></li>
<li><a href="#orgheadline57">Tangle</a></li>
<li><a href="#orgheadline58">View the results</a></li>
<li><a href="#orgheadline59">Library of Babel</a></li>
</ul>
</li>
<li><a href="#orgheadline212">Header arguments</a>
<ul>
<li><a href="#orgheadline67">Inheritance</a></li>
<li><a href="#orgheadline68">Standard header arguments</a></li>
<li><a href="#orgheadline75">:cache</a></li>
<li><a href="#orgheadline79">:cmdline</a></li>
<li><a href="#orgheadline92">:colnames</a></li>
<li><a href="#orgheadline95">:comments</a></li>
<li><a href="#orgheadline100">:dir</a></li>
<li><a href="#orgheadline103">:epilogue</a></li>
<li><a href="#orgheadline107">:eval</a></li>
<li><a href="#orgheadline111">:exports</a></li>
<li><a href="#orgheadline121">:file</a></li>
<li><a href="#orgheadline122">:file-desc</a></li>
<li><a href="#orgheadline127">:hlines</a></li>
<li><a href="#orgheadline129">:mkdirp</a></li>
<li><a href="#orgheadline132">:no-expand</a></li>
<li><a href="#orgheadline140">:noweb</a></li>
<li><a href="#orgheadline145">:noweb-ref</a></li>
<li><a href="#orgheadline148">:noweb-sep</a></li>
<li><a href="#orgheadline152">:padline</a></li>
<li><a href="#orgheadline157">:post</a></li>
<li><a href="#orgheadline160">:prologue</a></li>
<li><a href="#orgheadline171">:results</a></li>
<li><a href="#orgheadline174">:rownames</a></li>
<li><a href="#orgheadline179">:sep</a></li>
<li><a href="#orgheadline183">:session</a></li>
<li><a href="#orgheadline187">:shebang</a></li>
<li><a href="#orgheadline191">:tangle</a></li>
<li><a href="#orgheadline193">:tangle-mode</a></li>
<li><a href="#orgheadline203">:var</a></li>
<li><a href="#orgheadline211">:wrap</a></li>
</ul>
</li>
<li><a href="#orgheadline215">Extra language-specific header arguments</a>
<ul>
<li><a href="#orgheadline213">:preamble</a></li>
<li><a href="#orgheadline214">:separator</a></li>
</ul>
</li>
<li><a href="#orgheadline216">Data types</a></li>
<li><a href="#orgheadline217">Hooks</a></li>
<li><a href="#orgheadline219">Subtleties</a>
<ul>
<li><a href="#orgheadline218">Shell mode</a></li>
</ul>
</li>
<li><a href="#orgheadline227">Colophon</a>
<ul>
<li><a href="#orgheadline220">References</a></li>
<li><a href="#orgheadline224">Contributing</a></li>
<li><a href="#orgheadline225">License</a></li>
<li><a href="#orgheadline226">Change Log</a></li>
</ul>
</li>
</ul>
</div>
</div>

<div class="right">
  <a href="https://github.com/fniessen/refcard-org-babel/blob/master/README.org" class="fa fa-github"> Edit on GitHub</a>
</div>

<a href="http://opensource.org/licenses/GPL-3.0">
  <img src="http://img.shields.io/:license-gpl-blue.svg" alt=":license-gpl-blue.svg" />
</a>

<a href="https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=VCVAS6KPDQ4JC&lc=BE&item_number=refcard%2dorg%2dbabel&currency_code=EUR&bn=PP%2dDonationsBF%3abtn_donate_LG%2egif%3aNonHosted">
  <img src="https://www.paypalobjects.com/en_US/i/btn/btn_donate_LG.gif" alt="btn_donate_LG.gif" />
</a>

Welcome to Org Babel reference card.  It contains the reference documentation
that describes how to perform **code execution** within Org mode documents using Org
Babel 8.

In a nutshell, Org Babel is like Sweave (for reproducible research (See section Reproducible Research)) but it takes
a **large number of possible languages** (C, Java, Python, Ruby, R, &#x2026;) and Org
mode can produce **HTML** as well as **PDF**.

<div class="warning">
Prolonged use may cause addiction!

</div>

# Compendium principles<a id="orgheadline3"></a>

## Literate Programming<a id="orgheadline1"></a>

**Literate programming** (LP) offers 2 classical operations:

-   **Tangle:** Extract the source code blocks and **generate real working code** files for
    further compilation or execution, eventually outside of Emacs.

-   **Weave:** **Export** the whole Org file as literate, **human-readable documentation**
         (generally in HTML or LaTeX).

## Reproducible Research<a id="orgheadline2"></a>

Above those, Org Babel adds *in situ* code evaluation:

-   during **interactive** use (in the Org buffer itself),
-   during **tangle**, and/or
-   during **weave** (code blocks with `:exports` set to `results` or `both`)

This allows you to insert in your Org document:

-   all **data** (that can reasonably be included),
-   all **code** you used, and
-   the full set of **outputs** you got,

following the principles of **reproducible research** (RR).

---

# Code evaluation<a id="orgheadline51"></a>

Org mode basically just runs the code every time you export the document.  But,
if you&rsquo;ve changed some code and want a refresh, you can press `C-c C-v C-b` (See section Execute (or &ldquo;evaluate&rdquo;)) and
it will run it for sure then.

## Embedding code blocks<a id="orgheadline40"></a>

A **code block** is some sort of subprogram which does the desired job.

### Defining a code block<a id="orgheadline19"></a>

You can **define** and **call** it at the same time: the code block definition itself
acts as an **implicit call**.

#### Syntax<a id="orgheadline4"></a>

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

#### Language<a id="orgheadline5"></a>

The following language strings are currently recognized:

Awk, C, R, Asymptote, Calc, Clojure, CSS, Ditaa, Dot, Emacs Lisp, Forth, Fortran, Gnuplot, Haskell, IO, J, Java, Javascript, LaTeX, Ledger, Lilypond, Lisp, Makefile, Maxima, Matlab, Mscgen, Ocaml, Octave, Org, Perl, Pico Lisp, PlantUML, Python, Ruby, Sass, Scala, Scheme, Screen, Shell Script, Shen, Sql, Sqlite, ebnf2ps.

You can also add support for new languages:

    (add-to-list 'org-src-lang-modes '("<LANGUAGE>" . "<MAJOR-MODE>"))

so that font lock and editing source do work.

XXX Currently, a `bash` code block will be run with `bash`, and a `shell` code block will
be run with `sh`.  Both will use `shell-script-mode`. XXX

#### Code block arguments<a id="orgheadline8"></a>

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

-   Keyword arguments

-   Default arguments

#### Scope of Variables<a id="orgheadline11"></a>

-   Global variables

-   Local variables

#### Remarks<a id="orgheadline12"></a>

    (setq  org-babel-min-lines-for-block-output 10)
    (print 1)

    (setq  org-babel-min-lines-for-block-output 0)
    (print 1)

#### Examples<a id="orgheadline18"></a>

-   Using :headers

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

-   Do stuff conditional to the export backend

    Maybe you could do something like the following&#x2026;
    
        (message "do stuff")

-   Backend-conditional results

    You can replace
    
        (:results . "html")
    
    with
    
        (:results . (or (and org-export-current-backend "html")  "none"))
    
    in the `defvar` to get the desired result.

-   Cross-referencing a results block

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

-   Other explanation

    You need to apply `caption` and `name` keywords on the results, not the source
    code.

### Calling a code block<a id="orgheadline29"></a>

You can **define** a code block somewhere and then **call** it **explicitly** elsewhere
&#x2014; provided the code block has a `#+name:` meta data to label it.

#### Syntax<a id="orgheadline20"></a>

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

#### Remarks<a id="orgheadline21"></a>

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

#### Examples<a id="orgheadline28"></a>

-   Relying on the default value of the arguments

-   Providing explicit values to the arguments

        #+call: foo(bar=1)
    
    is equivalent to
    
        #+begin_src emacs-lisp :var results=foo(bar=1)
        ,  results
        #+end_src

-   Recursive

        (+ x 1)
    
        5
    
        7
    
        11

-   Other

    It is possible to pass the `:dir` header argument through a call line.
    
        pwd
    
    Call the above from somewhere else.
    
        /

-   Call by name

    Let&rsquo;s assume, the original code block takes an argument.
    
        echo "input=$input"
    
    If I want to &ldquo;get rid of&rdquo; that argument (to avoid typing), I can to name the
    result of calling that code block with a specific argument.
    
        input=new
    
    As `#+call:` lines can be named, it is possible to reference that result.
    
        echo "this=$input"

-   Raw results

    >> #+call: org-figure-to-slide[:exports none :results raw]()
    >
    > Does the following call line do what you want?
    >
    > #+call: org-figure-to-slide() :results raw
    
    <div class="warning">
    No square bracket for the &ldquo;end header arguments&rdquo;!
    
    </div>
    
    Thanks for your quick answer.  Nevertheless, adding :results raw at the end
    changes the formatting output but embeds everything within paren.  Given your
    advice, I am pretty closed to what I want to do by adding another :results raw
    command either as an inside header arguments or directly when declaring the
    `org-figure-to-slide` code like
    
        # ...

### Calling a code block from other elements<a id="orgheadline39"></a>

Using the `org-sbe` (for &ldquo;source block evaluate&rdquo;) macro, you may call arbitrary
code blocks

-   in a **table formula**,
-   in **file local variables**,
-   inside of an **elisp link**, or
-   in any **header argument (See section Header arguments)**.

#### Syntax<a id="orgheadline30"></a>

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

#### Remarks<a id="orgheadline31"></a>

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

#### Examples<a id="orgheadline38"></a>

-   Calling a code block in a table formula, relying on the default value of the arguments

-   Calling a code block in a table formula, providing explicit values to the arguments

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

-   Passing header arguments

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

-   Using `org-sbe` in a local variables line

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

-   Using `org-sbe` inside of an elisp link

        for i in $(seq 1 $to); do
           printf $i;
        done
    
    Clicking on the following hyperlink will execute the code block.
    
        [[elisp:(org-sbe counter (to "3"))][count to 3]]

-   Using `org-sbe` to assign header arguments

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

## Embedding inline code<a id="orgheadline50"></a>

You can also evaluate code inline as follows.

### Inline code blocks<a id="orgheadline44"></a>

An **inline code block** (a.k.a. inline source block) is a code block which is
placed *inline* within textual elements such as paragraphs of text or lists.

Its sole purpose is to include **results inline** in textual elements.

#### Syntax<a id="orgheadline41"></a>

The basic syntax structure for inline code blocks is:

    src_<LANGUAGE>{<BODY>}

without header arguments, and:

    src_<LANGUAGE>[<HEADER-ARGS>]{<BODY>}

with header arguments.

#### Remarks<a id="orgheadline42"></a>

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

#### Examples<a id="orgheadline43"></a>

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

### Inline Babel calls<a id="orgheadline49"></a>

#### Syntax<a id="orgheadline45"></a>

The syntax for inline evaluation of named code blocks is the following, where
each header argument portion is optional (so are the square brackets).

    ... call_<NAME>(<ARGUMENTS>) ...
    ... call_<NAME>[<HEADER-ARGS-FOR-BLOCK>](<ARGUMENTS>)[<HEADER-ARGS-FOR-CALL-LINE>] ...

#### Remarks<a id="orgheadline46"></a>

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

#### Examples<a id="orgheadline48"></a>

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

-   Similar code in three languages

        (+ 1 1)
    
        expr 1 + 1
    
        1 + 1
    
    Will lisp-2 export with a newline?
    
    Will shell-2 export with a newline?
    
    Will r-2 export with a newline?
    
    ---

# Key bindings<a id="orgheadline60"></a>

Babel commands can be used as **speed commands** when the point is at the beginning
of a code block (specifically, at the beginning of the `#+begin_src` line, in
column `0`).

Org-babel is making use of the `C-c C-v` key binding.  All Org-babel keybindings
are located behind this prefix.

## Help<a id="orgheadline52"></a>

-   **`C-c C-v h`:** **Describe** all **key bindings** behind Org Babel key prefix.

## Edit the code<a id="orgheadline53"></a>

-   **`C-c '`:** **Edit** the code block in a **dedicated buffer**.

-   **`C-c C-v C-x` (or `C-c C-v x`):** Read **key sequence** and **execute** the command in edit buffer (in the language
    major mode).
    
    For example:
    
    -   **`C-c C-v C-x TAB` (or `C-c C-v x TAB`):** Insert a language-native **TAB** in code block.
    
    -   **`C-c C-v C-x M-;` (or `C-c C-v x M-;`):** **Comment** region according to language.
    
    -   **`C-c C-v C-x C-M-\` (or `C-c C-v x C-M-\`):** **Indent** region according to language.

-   **`C-c C-v d` (or `C-c C-v C-d`):** **Demarcate code block** (i.e., wrap the region in a block, or split the
    block around point).

-   **`C-c C-v C-M-h`:** **Mark** (= select) the **whole** code block.

-   **`C-c C-v I` (or `C-c C-v C-I`):** **Display information** on the current source block.  This includes header
    arguments, language and name.

-   **`C-c C-v j` (or `C-c C-v C-j`):** **Insert a header argument** and its value using completing read in the
    minibuffer.
    
        (defun my-org-babel-deactivate ()
          (interactive)
          (org-babel-insert-header-arg "eval" "never")
          (org-babel-insert-header-arg "tangle" "no"))

-   **`C-c C-v c` (or `C-c C-v C-c`):** **Check** for *suspicious* (that is, unknown or **misspelled**) header arguments.
    This may prove useful if you get into the habit of using it as your first
    step in **debugging** code blocks.
    
    Languages are permitted to add and use **any** arbitrary new header argument, so
    there is no *wrong* (that is, disallowed) header argument.

## SHA1 hash<a id="orgheadline54"></a>

-   **`C-c C-v a` (or `C-c C-v C-a`):** **View SHA1 hash** of the current code block.

You could use the hash returned by that function to manually replace the
original one in the block, and avoid reevaluating the block in case of purely
cosmetic changes.

## Navigate<a id="orgheadline55"></a>

-   **`C-c C-v g`:** Go to a **named code block** (with completing read).
    
    The target src block is guessed from:
    
    1.  noweb reference
    2.  `#+call:`
    3.  `#+results:`
    4.  `symbol-at-point`
    
    if one of these is found (in that order).
    
    The point being pushed to the `org-mark-ring`, you can return to it with
    `C-c &`.

-   **`C-c C-v r` (or `C-c C-v C-r`):** Go to a **named result** (with completing read).

-   **`C-c C-v n` (or `C-c C-v C-n`):** Go to the **next** code block.

-   **`C-c C-v p` (or `C-c C-v C-p`):** Go to the **previous** code block.

-   **`C-c C-v u` (or `C-c C-v C-u`):** Go to the **beginning** of the current code block (jump to the enclosing
    structural block).

## Execute (or &ldquo;evaluate&rdquo;)<a id="orgheadline56"></a>

-   **`C-c C-v b` (or `C-c C-v C-b`):** Evaluate all the code blocks **in the current buffer**.

-   **`C-c C-v s` (or `C-c C-v C-s`):** Evaluate all the code blocks **in the current subtree**.

-   **`C-c C-v e` (or `C-c C-v C-e`):** Evaluate the code block **at point**.
    
    With `C-u C-c C-v C-e`, forcibly re-evaluate the block (marked `:cache`).

## Tangle<a id="orgheadline57"></a>

-   **`C-c C-v t` (or `C-c C-v C-t`):** **Tangle all the code blocks** in current file.
    
    Run the tangle with a prefix argument (e.g., `C-u C-c C-v t`) to only
    tangle the **block under your cursor** (current code block).
    
    Press `C-u C-u C-c C-v t` to tangle **all blocks** belonging to the **target file
    of the block at point** (that is, having the same tangle file name).

-   **`C-c C-v f` (or `C-c C-v C-f`):** **Tangle all the code blocks** in supplied file.

In many languages constructs like <a id="orgtarget1"></a> are valid code, so it would be
inappropriate for tangling to raise errors by default.  It is possible to turn
on such errors on a language-by-language basis, by customizing the variable
`org-babel-noweb-error-langs` (list of languages for which Babel will raise
literate programming errors when the source code block satisfying a noweb
reference in this language can not be resolved).

## View the results<a id="orgheadline58"></a>

-   **`C-c C-v v` (or `C-c C-v C-v`):** **View the expanded body** of the code block at point.  This view of code
    blocks is not editable.
    
    This facility of **previewing** the expanded contents is useful for **debugging**.

-   **`C-c C-v o` (or `C-c C-v C-o`):** **Open the results** of the code block at point.

-   **`C-c C-v C-z` (or `<M-down>`):** **Switch to the session buffer** of the current code block (first, you need to
    add `:session` to it).
    
    Use `C-u C-c C-v C-z` to bring up the session with the input variable
    pre-loaded.

-   **`C-c C-v z` (variant of `C-c C-v C-z`):** **Split** the window between:
    -   the **session buffer**, and
    -   a language major-mode **edit buffer** for the code block in question.
    
    This can be convenient for using language major mode for interacting with
    the session buffer.

-   **`C-c C-v k`:** **Delete** code block **results**, inline code block results, and call line
    results.
    
    When called without a prefix argument, it only applies to the **current code
    block**.
    
    When called with a prefix argument, it applies to the **entire buffer**.

## Library of Babel<a id="orgheadline59"></a>

-   **`C-c C-v i` (or `C-c C-v C-i`):** **Ingest all named code blocks** from supplied file into the Library of Babel
    (in memory).

-   **`C-c C-v l` (or `C-c C-v C-l` or `<M-up>`):** **Load the current code** block into the Library of Babel and enter the
    **session**.

---

# Header arguments<a id="orgheadline212"></a>

## Inheritance<a id="orgheadline67"></a>

Header arguments can be set at different levels of a hierarchy:

1.  **Default** header arguments (See section Default header arguments shipped with Org mode) shipped with Org mode
2.  Default **languages-specific** header arguments (See section Default languages-specific header arguments shipped with Org mode) shipped with Org mode
3.  **Buffer** (or file) level header arguments (See section Buffer (or file) level header arguments)
4.  **Subtree** header arguments (See section Subtree header arguments)
5.  **Code block** header arguments (See section Code block header arguments)
6.  **Call line** header arguments (See section Call line header arguments)

At the top of the hierarchy, default header arguments shipped with Org mode
are the most general of all: they define behavior common to all code blocks,
unless set otherwise, inherited by all lower levels.

Header arguments near the bottom of the hierarchy provide behavior more
specific to a (group of) code block(s).

### Default header arguments shipped with Org mode<a id="orgheadline61"></a>

-   Variable `org-babel-default-header-args` for source blocks
-   Variable `org-babel-default-inline-header-args` for inline code blocks
-   Variable `org-babel-default-lob-header-args` for `#+call` lines

These default header arguments can be set by the user:

    ;; add default arguments to use when evaluating a source block
    (add-to-list 'org-babel-default-header-args
                 '(:noweb . "yes"))

This can also be done file-wide through the use of *file local variables*.

### Default languages-specific header arguments shipped with Org mode<a id="orgheadline62"></a>

-   Variable `org-babel-default-header-args:emacs-lisp` for Emacs Lisp
-   Variable `org-babel-default-header-args:R` for R
-   Variable `org-babel-default-header-args:org` for Org
-   Etc.

<div class="seealso">
`org-babel-common-header-args-w-values`

</div>

Header arguments which have different defaults between languages:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Languages</th>
<th scope="col" class="org-left">:exports</th>
<th scope="col" class="org-left">:hlines</th>
<th scope="col" class="org-left">:results</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">general-purpose languages + *shell*</td>
<td class="org-left">code</td>
<td class="org-left">no</td>
<td class="org-left">replace</td>
</tr>


<tr>
<td class="org-left">*ledger*</td>
<td class="org-left">code</td>
<td class="org-left">no</td>
<td class="org-left">output replace</td>
</tr>


<tr>
<td class="org-left">*org*</td>
<td class="org-left">code</td>
<td class="org-left">no</td>
<td class="org-left">silent raw</td>
</tr>


<tr>
<td class="org-left">*latex*</td>
<td class="org-left">results</td>
<td class="org-left">no</td>
<td class="org-left">latex replace</td>
</tr>


<tr>
<td class="org-left">graphics-only languages</td>
<td class="org-left">results</td>
<td class="org-left">no</td>
<td class="org-left">file replace</td>
</tr>
</tbody>
</table>

These default language-specific header arguments can be changed by the user:

    (add-to-list 'org-babel-default-header-args:R
                 '(:session . "*org-R*"))
    
    (add-to-list 'org-babel-default-header-args:R
                 '((:width . 640) (:height . 640)))

This can also be done file-wide (for certain files) through the use of:

-   property lines (See section Buffer (or file) level header arguments) or
-   *file local variables*.

### Buffer (or file) level header arguments<a id="orgheadline63"></a>

A `#+PROPERTY:` line located **anywhere** in a buffer affects the entire file: it
sets a **global** property.

<div class="warning">
Don&rsquo;t forget to `C-c C-c` on these arguments (just one of them in your preamble)
to make sure they&rsquo;re updated before trying another export.

</div>

Header arguments can be specified with the **header-args property**.

For example, when you want to tangle all source blocks in an Org file, include
the line:

    #+PROPERTY: header-args :tangle yes

<div class="note">
Before, **header arguments** could also be specified as **separate properties**
(`#+PROPERTY: tangle yes`, for example), but that syntax has been **deprecated** in
the commit `90b16870` of 2013-06-23 as that was slower to fetch block properties.

</div>

You can also specify **multiple header arguments as one Org mode property** through
the use of:

    #+PROPERTY: header-args :results output :cache yes

That can be over-ridden on a per-subtree (See section Subtree header arguments) or per-block (See section Code block header arguments) basis.

Any property specification, unless it is postfixed with a `+`, will *reset* the
value of that property to its current value: in the case of two `#+PROPERTY:`
lines for the same property, the property will have the later value.

But there is a general mechanism for the **concatenation of property** strings
(**accumulated values**):

    #+PROPERTY: header-args:R :exports results
    #+PROPERTY: header-args:R+ :width 800

### Subtree header arguments<a id="orgheadline64"></a>

In contrast to property lines, a `:PROPERTIES:` block is only valid **for the given
tree (and subtrees)**:

    * Outline heading
      :PROPERTIES:
      :header-args:    :results output :cache yes
      :END:

### Code block header arguments<a id="orgheadline65"></a>

    #+header: :exports code :var data=2
    #+begin_src emacs-lisp
    ... some code ...
    #+end_src

### Call line header arguments<a id="orgheadline66"></a>

## Standard header arguments<a id="orgheadline68"></a>

The following table lists the standard header arguments that Org Babel uses.

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">:cache (See section :cache)</td>
<td class="org-left">:noweb-ref (See section :noweb-ref)</td>
</tr>


<tr>
<td class="org-left">:cmdline (See section :cmdline)</td>
<td class="org-left">:noweb-sep (See section :noweb-sep)</td>
</tr>


<tr>
<td class="org-left">:colnames (See section :colnames)</td>
<td class="org-left">:padline (See section :padline)</td>
</tr>


<tr>
<td class="org-left">:comments (See section :comments)</td>
<td class="org-left">:post (See section :post)</td>
</tr>


<tr>
<td class="org-left">:dir (See section :dir)</td>
<td class="org-left">:prologue (See section :prologue)</td>
</tr>


<tr>
<td class="org-left">:epilogue (See section :epilogue)</td>
<td class="org-left">:results (See section :results)</td>
</tr>


<tr>
<td class="org-left">:eval (See section :eval)</td>
<td class="org-left">:rownames (See section :rownames)</td>
</tr>


<tr>
<td class="org-left">:exports (See section :exports)</td>
<td class="org-left">:sep (See section :sep)</td>
</tr>


<tr>
<td class="org-left">:file (See section :file)</td>
<td class="org-left">:session (See section :session)</td>
</tr>


<tr>
<td class="org-left">:file-desc (See section :file-desc)</td>
<td class="org-left">:shebang (See section :shebang)</td>
</tr>


<tr>
<td class="org-left">:hlines (See section :hlines)</td>
<td class="org-left">:tangle (See section :tangle)</td>
</tr>


<tr>
<td class="org-left">:mkdirp (See section :mkdirp)</td>
<td class="org-left">:tangle-mode (See section :tangle-mode)</td>
</tr>


<tr>
<td class="org-left">:no-expand (See section :no-expand)</td>
<td class="org-left">:var (See section :var)</td>
</tr>


<tr>
<td class="org-left">:noweb (See section :noweb)</td>
<td class="org-left">:wrap (See section :wrap)</td>
</tr>
</tbody>
</table>

<div class="note">
The argument of any header option can be replaced by an ELisp form &#x2013; which
should return a string (or a list of strings, depending on the case).

</div>

---

## :cache<a id="orgheadline75"></a>

Avoids re-evaluating unchanged code blocks.

### Options<a id="orgheadline69"></a>

-   **`:cache no`:** (default)

-   **`:cache yes`:** Avoids re-evaluating unchanged code blocks by associating a **hash** of the
    *expanded* code block (= code block and parameters) with the **results**.  It
    allows the results to be returned without having to re-run the code
    block &#x2014; unless the code or the input parameters have changed.

### Remarks<a id="orgheadline70"></a>

In order for caching to work (i.e., **no evaluation** when triggered either
**interactively** or **during export**), the results of the code block must be present
in the Org mode file: you must first evaluate it manually, leaving the results
(with the hash tag) saved within the Org mode file.

Note that same input does **not** always **guarantee the same output**, e.g.,

    date

Though, this code block shouldn&rsquo;t be marked `:cache` unless the desired (and
odd) behavior is to have a datestamp that is only updated when the user
forcibly re-evaluates the block (with `C-u C-c C-v C-e`).

### Examples<a id="orgheadline73"></a>

#### Avoid re-evaluating unchanged code blocks<a id="orgheadline71"></a>

The following example uses the cache optimization for a very long running code
block.

    #+begin_src shell :eval yes :results verbatim :cache yes
    printf "Please wait (this can take a while)...\n"
    sleep 5
    printf "Done!\n"
    #+end_src

Here is the results block, evaluated within no time, except for the very first
time it is run.

    #+results[af6f...]:
    #+begin_example
    Please wait (this can take a while)...
    Done!
    #+end_example

#### Avoid re-evaluating code blocks unless some process restarts<a id="orgheadline72"></a>

The following example allows to include the PID of the R process in the results
hash, so that the code would be rerun only if the R process (session) restarts.

    ps -a | grep "$R" | grep -v 'grep' | awk '{print $2}'

    # code to perform side effect
    x <- 'side effect'
    'done' # add something small to get a results block

### See also<a id="orgheadline74"></a>

:eval (See section :eval)   
:exports (See section :exports)

---

## :cmdline<a id="orgheadline79"></a>

### Options<a id="orgheadline76"></a>

-   **nothing:** (default)

-   **`:cmdline <...>`:** Pass some command line arguments.

### Remarks<a id="orgheadline77"></a>

The `:cmdline` header argument is supported by a couple of languages.

### Examples<a id="orgheadline78"></a>

For shell, this allows to make the code inside a Babel code block similar to
a real shell script.

    echo $2

The script can use `$@` for its positional parameters.

    echo "$@"

Also, calling the script using `#+call` is like calling it from another shell
script (supplying the value in the call line).

---

## :colnames<a id="orgheadline92"></a>

Handles **column names in tables**.

### Options<a id="orgheadline80"></a>

-   **nothing:** Removes the header (and the top hline) from the table before processing if
    the input table looks like it has column names (because its second row is
    an hline).   
    (default)

-   **`:colnames no`:** Don&rsquo;t strip the header.  Re-add headers (post-processing).   
    (default for Emacs Lisp code blocks)

-   **`:colnames yes`:** Tells Org Babel that your first row contains column names.  Applies the
    code block to the body of the input table.

-   **`:colnames <LIST>`:** Specifies to use `<LIST>` as column names.

-   **`:colnames '()`:** **Unsets** the header argument if set at a higher level.
    
    Is the same as &ldquo;none&rdquo; WHEN NOT SET AT A HIGHER LEVEL.

### Remarks<a id="orgheadline82"></a>

By default, the first row will be used for column names if followed by a `hline`
XXX???XXX.  Without a `hline`, use `:colnames yes`.

<table id="orgtable1" border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">&#xa0;</th>
<th scope="col" class="org-left">Alpha</th>
<th scope="col" class="org-left">Beta</th>
<th scope="col" class="org-left">Gamma</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">First</td>
<td class="org-left">A1</td>
<td class="org-left">B1</td>
<td class="org-left">C1</td>
</tr>


<tr>
<td class="org-left">Second</td>
<td class="org-left">A2</td>
<td class="org-left">B2</td>
<td class="org-left">C2</td>
</tr>


<tr>
<td class="org-left">Third</td>
<td class="org-left">A3</td>
<td class="org-left">B3</td>
<td class="org-left">C3</td>
</tr>
</tbody>
</table>

    colnames(data)
    rownames(data)

#### Notes<a id="orgheadline81"></a>

&ldquo;Tables&rdquo; occur in at least three contexts in babel source code blocks.  There
are input tables, :var mytable, there are &ldquo;tables&rdquo; within the code block,
represented in R as a dataframe or a matrix, and there are output tables, which
are placed in the Org mode buffer as a result.

I use :colnames to keep the column names of input tables associated with the
&ldquo;tables&rdquo; within the code block, and typically have them represented in the
output, whether that is a &ldquo;table&rdquo; written to file, or output to the Org mode
buffer as an Org mode table, either by default or (more rarely) through use
of :results table.

In my experience :results table is mostly useful for coercing a value that babel
would otherwise interpret as a scalar into a single element table.

### Examples<a id="orgheadline90"></a>

Consider the following input tables, one without column names, one with column
names.

<table id="orgtable2" border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-right" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-right">1</td>
<td class="org-left">one</td>
</tr>


<tr>
<td class="org-right">2</td>
<td class="org-left">two</td>
</tr>


<tr>
<td class="org-right">3</td>
<td class="org-left">three</td>
</tr>
</tbody>
</table>

<table id="orgtable3" border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-right" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-right">num</th>
<th scope="col" class="org-left">word</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-right">1</td>
<td class="org-left">one</td>
</tr>


<tr>
<td class="org-right">2</td>
<td class="org-left">two</td>
</tr>


<tr>
<td class="org-right">3</td>
<td class="org-left">three</td>
</tr>
</tbody>
</table>

#### Using no `:colnames` header argument<a id="orgheadline83"></a>

The following example outputs the table without column names.

#### Using `:colnames no`<a id="orgheadline84"></a>

The following example outputs all the rows of the table, considering there is
no column names.

#### Using `:colnames yes`<a id="orgheadline85"></a>

The following example outputs the table with its column names.

    echo "$data"

    ((1 "one") (2 "two") (3 "three"))

#### Using specified column names<a id="orgheadline86"></a>

<div class="note">
The following will work for R with an (unapplied) patch of Eric, sent on the
Org-mode mailing list on <span class="timestamp-wrapper"><span class="timestamp">[2013-04-08 Mon]</span></span>.

It does already work in most other languages (such as shell blocks).

</div>

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-right" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-right">X0</th>
<th scope="col" class="org-left">foo</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-right">1</td>
<td class="org-left">bar</td>
</tr>


<tr>
<td class="org-right">2</td>
<td class="org-left">baz</td>
</tr>
</tbody>
</table>

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-right" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-right">id</th>
<th scope="col" class="org-left">var1</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-right">1</td>
<td class="org-left">bar</td>
</tr>


<tr>
<td class="org-right">2</td>
<td class="org-left">baz</td>
</tr>
</tbody>
</table>

#### Show the labels of the vertical and the horizontal axes<a id="orgheadline87"></a>

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-right" />

<col  class="org-right" />

<col  class="org-right" />

<col  class="org-right" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">&#xa0;</th>
<th scope="col" class="org-right">&#xa0;</th>
<th scope="col" class="org-right">**happiness**</th>
<th scope="col" class="org-right">&#xa0;</th>
<th scope="col" class="org-right">&#xa0;</th>
</tr>


<tr>
<th scope="col" class="org-left">&#xa0;</th>
<th scope="col" class="org-right">&#xa0;</th>
<th scope="col" class="org-right">0</th>
<th scope="col" class="org-right">1</th>
<th scope="col" class="org-right">2</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">**org.files**</td>
<td class="org-right">0</td>
<td class="org-right">1.00</td>
<td class="org-right">0.00</td>
<td class="org-right">0.00</td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-right">1</td>
<td class="org-right">0.00</td>
<td class="org-right">1.00</td>
<td class="org-right">1.00</td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-right">2</td>
<td class="org-right">0.00</td>
<td class="org-right">1.00</td>
<td class="org-right">3.00</td>
</tr>
</tbody>
</table>

#### Utility function<a id="orgheadline88"></a>

<table id="orgtable4" border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">numbers</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">one</td>
</tr>


<tr>
<td class="org-left">two</td>
</tr>


<tr>
<td class="org-left">three</td>
</tr>


<tr>
<td class="org-left">four</td>
</tr>
</tbody>
</table>

    (mapcar (lambda (row) (mapcar #'length row)) in)

#### `#+call` get the first row of output table lost when using latex export<a id="orgheadline89"></a>

`org-babel-execute:python` does its own formatting.  `#+call:` on the other hand
passes the results to Babel.

The workaround is to use:

    #+call: t1() :colnames yes

### See also<a id="orgheadline91"></a>

:hlines (See section :hlines)   
:rownames (See section :rownames)

---

## :comments<a id="orgheadline95"></a>

Controls the insertion of extra comments into the tangled code files to allow
backward linking from tangled code blocks to the original code blocks (from
which the code was tangled).

### Options<a id="orgheadline93"></a>

-   **`:comments no`:** Does not insert any comments in tangled files (beyond those which may
    already exist in the body of the code block).   
    (default)

-   **`:comments link`:** Inserts &ldquo;tangled from X&rdquo; comments around the code blocks in the tangled
    file (with **links** back to the original Org mode file).

-   **`:comments yes`:** Is the same as `:comments link`.   
    (deprecated)

-   **`:comments noweb`:** Inserts comment wrappers (with **links** back to the original Org mode file)
    around all **embedded noweb** sections.  Is an enhanced version of `:comments
         link`, that can break newlines in `<<block>> + 1`, for example.  XXX

-   **`:comments org`:** Includes **preceding text** from the Org mode file as a comment before the
    code block (but does not wrap the code with links back to the original
    Org file).

-   **`:comments both`:** Turns on both the `link` and `org` comment options.

### See also<a id="orgheadline94"></a>

:tangle (See section :tangle)

The variable `org-babel-tangle-use-relative-file-links` controls whether files
and links in comments in tangled files use relative or absolute path names (it
defaults to relative paths).

---

## :dir<a id="orgheadline100"></a>

Specifies the default (possibly **remote**) directory during code block execution.

### Options<a id="orgheadline96"></a>

-   **nothing:** Uses the directory associated with the **current buffer**.

-   **`:dir <DIR>`:** Specifies to use `DIR` as the **default directory** for code block execution.

### Remarks<a id="orgheadline97"></a>

You can use the `:dir` header argument for **remote execution**.  The directory is
specified using using [Tramp filename syntax](http://www.gnu.org/software/emacs/manual/html_node/tramp/Filename-Syntax.html).

`:results output` seems to be necessary!

### Examples<a id="orgheadline98"></a>

WHY DON&rsquo;T WE HAVE TO SPECIFY :OUTPUT FOR THE SHELL BLOCK?

    hostname

    hostname

    ls -la

    SELECT 2+2 AS 'four', 1+1 AS 'one';

### See also<a id="orgheadline99"></a>

:file (See section :file)

---

## :epilogue<a id="orgheadline103"></a>

Appends text to code block body.

### Options<a id="orgheadline101"></a>

-   **`:epilogue ""`:** (default)

-   **`:epilogue <TEXT>`:** Appends the value of the `:epilogue` header argument to the code block
    body before execution.

### See also<a id="orgheadline102"></a>

:prologue (See section :prologue)

---

## :eval<a id="orgheadline107"></a>

Specifies permissions for *every* execution of code blocks.

### Options<a id="orgheadline104"></a>

-   **nothing:** If the `org-confirm-babel-evaluate` variable is nil, allows evaluation of the
    code block (both interactively and during export) without a confirmation
    from the user.   
    (default)

-   **`:eval <any-value-other-than-the-reserved-ones>`:** If the `org-confirm-babel-evaluate` variable is nil, allows evaluation of the
    code block (both interactively and during export) without a confirmation
    from the user.  It **undoes the effect of `:eval no`** (and other values
    disabling evaluation) set at the file or sub-tree level.

-   **`:eval query`:** Requires confirmation before evaluating the code block (both
    interactively and during export), regardless of the value of the
    `org-confirm-babel-evaluate` variable.

-   **`:eval query-export`:** Allows interactive evaluation of the code block, but requires
    confirmation before evaluating it during export, regardless of the value
    of the `org-confirm-babel-evaluate` variable.

-   **`:eval never`:** **Inhibits** (silently) **evaluation** of the code block (both interactively
    and during export).  This is useful for protecting against the evaluation
    of dangerous code blocks.

-   **`:eval no`:** Is the same as `:eval never`.

-   **`:noeval`:** Is the same as `:eval no`.

-   **`:eval never-export`:** Allows interactive evaluation of the code block, but inhibits its
    evaluation during export.

-   **`:eval no-export`:** Is the same as `:eval never-export`.

### Remarks<a id="orgheadline105"></a>

Sometimes, to **switch off execution** of code blocks **during export**, you can find
it easy to simply manually generate the results of a code block (e.g., through
an interactive evaluation), and set the `:eval` property of the code block to
`never-export`.

Note that, unlike tangling (See section :tangle), evaluation requires the specific language to be
supported for both performing the evaluation and collecting the results (See section :results).

### See also<a id="orgheadline106"></a>

:cache (See section :cache)   
:exports (See section :exports)   
:session (See section :session)

Variable `org-confirm-babel-evaluate`.

---

## :exports<a id="orgheadline111"></a>

Specifies how code and/or results should be handled **during export**.

### Options<a id="orgheadline108"></a>

-   **`:exports none`:** Doesn&rsquo;t include anything in the exported file.

-   **`:exports code`:** Includes (only) the body of the code block into the exported file.   
    (default)   
    (default for Org code blocks)

-   **`:exports results`:** Includes (only) the **results block** in the exported file.   
    (default for *inline* code blocks)   
    (default for LaTeX code blocks)   
    (default for code blocks in graphics-only languages)

-   **`:exports both`:** Includes both the code block and the results (See section :eval) in the exported file.

### Remarks<a id="orgheadline109"></a>

-   When `:exports` is set to `none` or `code`, Org Babel will **not run (See section :eval)** the code block
    **during export**, avoiding to (re-)generate the results on every export.  In
    particular, use that on code blocks which cannot be executed (See section :eval) on
    their own.
    
    This has **no effect on interactive evaluation (See section :eval)**, though.

-   When `:exports` is set to `results` or `both`, if evaluation is allowed during
    export, the code block will be (re-)evaluated (See section :eval) during export.  Otherwise, the
    current (unchanged) results block, when present, will be included **in the
    exported file**.

-   Note that the `:exports` option is only relevant for code blocks, not inline
    code.

-   A code block in a subtree tagged `:noexport:` will still be evaluated, if
    evaluation is allowed during export, because its side-effects may be needed
    for code run elsewhere.  If you don&rsquo;t want that, set `:eval` accordingly.

### See also<a id="orgheadline110"></a>

:cache (See section :cache)   
:eval (See section :eval)   
:results (See section :results)

---

## :file<a id="orgheadline121"></a>

Specifies to **write the results to a file**.

### Options<a id="orgheadline112"></a>

-   **`:file <FILENAME>`:** Writes the **results** from the code block evaluation to `<FILENAME>` and
    inserts (for the **results block**) a **link to the file** into the Org mode
    buffer.

### Remarks<a id="orgheadline113"></a>

Extension can be everything: `.png`, `.pdf`, `.txt`, `.csv`, etc.

When relative, the filename is interpreted relatively to the default
directory (See section :dir).

-   For **graphics-only languages** (e.g. *asymptote*, *ditaa*, *dot*, *gnuplot*,
    *mscgen*, *plantuml*), the &ldquo;results&rdquo; is the **graphics**, and a link to the
    image is placed in the Org buffer.

-   For **general-purpose languages** (e.g. *emacs-lisp*, *python*, *R*, *ruby*, *sh*), the
    &ldquo;results&rdquo; written to file is the **normal Org Babel results** (string, number,
    table).
    
    When generating **graphics**, including the `:results graphics` header argument is
    **required**, in addition to `:file <FILENAME>`, in order for graphical output to be
    sent automatically to file.  If `:file` is supplied, but not `:results graphics`,
    then non-graphical (`value` or `output`) results are written to file.

Links to generated images will be expanded to include the contents of the
images upon export.

If you then turn on inline images with `M-x org-toggle-inline-images`, you can
preview the generated image from within your Org buffer.

Some languages including *R*, *gnuplot*, *dot*, and *ditaa* provide special
handling of the `:file` header argument automatically wrapping the code block
body in the boilerplate code required to save output to the specified file.
This is often useful for saving graphical output of a code block to the
specified file.

*This means that the argument of the `:file` header can be omitted and the
file name can be generated within the source block.*

### Examples<a id="orgheadline119"></a>

#### Saving the textual output from a general-purpose language to a text file<a id="orgheadline114"></a>

Send the text output of `ls -l` directly to a file:

    ls -l

<dirlisting.txt>

Recall that `:results value` is the default.

#### Saving the graphical output from a general-purpose language to an image file<a id="orgheadline115"></a>

XXX Output is not necessary!!

    plot(1:10, (1:10)^2)

![img](images/square.png)

#### Saving the graphical output from a graphics language to an image file<a id="orgheadline118"></a>

-   Dotty

        digraph G {
          a -> b [label="hello", style=dashed];
          a -> c [label="world"];
          b -> c;
          b [shape=Mdiamond, label="this is b"];
          c [shape=polygon, sides=5, peripheries=3];
        }
    
    ![img](images/dot.png)
    
    Recall that `:exports results` is the default for graphics-only languages.

-   R

    Choose PNG extension (and not PDF) to preview the results in the Org buffer
    itself.
    
        plot(1:10, (1:10)^2)

### See also<a id="orgheadline120"></a>

:dir (See section :dir)   
:results (See section :results)   
:sep (See section :sep) (for saving tabular results)

---

## :file-desc<a id="orgheadline122"></a>

Specifies a description for file results.

---

## :hlines<a id="orgheadline127"></a>

Handles **horizontal lines** in input tables.

### Options<a id="orgheadline123"></a>

-   **`:hlines no`:** Strips horizontal lines from the input table.   
    (default)

-   **`:hlines yes`:** Preserves horizontal lines in the input table.   
    (default for Emacs Lisp code blocks)

### Remarks<a id="orgheadline124"></a>

**Don't confound this with the :colnames (See section :colnames) machinery.**

### Examples<a id="orgheadline125"></a>

<table id="orgtable5" border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-right" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-right">Key</th>
<th scope="col" class="org-left">Val</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-right">1</td>
<td class="org-left">one</td>
</tr>


<tr>
<td class="org-right">2</td>
<td class="org-left">two</td>
</tr>


<tr>
<td class="org-right">3</td>
<td class="org-left">three</td>
</tr>
</tbody>
</table>

<table id="orgtable6" border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-right" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-right">Key</th>
<th scope="col" class="org-left">Val</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-right">1</td>
<td class="org-left">one</td>
</tr>


<tr>
<td class="org-right">2</td>
<td class="org-left">two</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-right">3</td>
<td class="org-left">three</td>
</tr>
</tbody>
</table>

    data

    data

    data

    data

The `:hlines yes` header argument must be set on the call line itself.

    ((1 "one") (2 "two") (3 "three"))

    (("Key" "Val") (1 "one") (2 "two") (3 "three"))

### See also<a id="orgheadline126"></a>

:colnames (See section :colnames)

---

## :mkdirp<a id="orgheadline129"></a>

Toggles creation of parent directories of target files during tangling.

### Options<a id="orgheadline128"></a>

-   **`:mkdirp no`:** Don&rsquo;t create the directories if they don&rsquo;t exist.

-   **`:mkdirp yes`:** Create the directories if they don&rsquo;t exist.

---

## :no-expand<a id="orgheadline132"></a>

Turns off the code blocks expansion **during tangling**.

### Remarks<a id="orgheadline130"></a>

`:no-expand` has no effect during execution.

### See also<a id="orgheadline131"></a>

:noweb (See section :noweb)   
:noweb-ref (See section :noweb-ref)   
:noweb-sep (See section :noweb-sep)   

---

## :noweb<a id="orgheadline140"></a>

Specifies when expansion of &ldquo;noweb&rdquo; style references should occur.

### Options<a id="orgheadline133"></a>

-   **`:noweb no`:** (default)

-   **`:noweb yes`:** Expands noweb references in code blocks during both tangling and
    evaluation (interactively and during export).

-   **`:noweb strip-export`:** Expands noweb references in code blocks before the block is tangled or
    evaluated.  However, they will be **stripped on export**.

-   **`:noweb no-export`:** Expands noweb references during tangling and interactive evaluation, but
    not during export.

-   **`:noweb tangle`:** Expands noweb references only during tangling, but not during interactive
    evaluation nor during export.

-   **`:noweb eval`:** Expands noweb references **only** during interactive evaluation.

### Syntax of noweb references<a id="orgheadline134"></a>

-   **`<<code-block-name>>`:** Insert the literal body of **code** block `code-block-name` itself.

-   **`<<code-block-name()>>`:** **Insert the \*results** of evaluating the code block `code-block-name` (as
    obtained with `org-babel-execute-src-block`).  That can be of different
    types: LaTeX, etc.

-   **`<<code-block-name(x=3.3)>>`:** Insert the **results** of evaluating it with the specified argument value.

Note that you can customize `org-babel-noweb-wrap-start` and
`org-babel-noweb-wrap-end` to use something else than angle brackets (for example,
double quotes).

<div class="tip">
The Noweb syntax allows the use of **blanks in names of code blocks** (hence,
sentences for code block names, so that we can enjoy the literate in literate
programming a lot more!) since 2015-02-18.

</div>

### Examples<a id="orgheadline138"></a>

#### Expand block<a id="orgheadline135"></a>

#### Execute block<a id="orgheadline136"></a>

Note the parens in the noweb reference:

    echo "["
    ls *.org | sed 's/$/;/'
    echo "]"

#### Expand variable in tangled code<a id="orgheadline137"></a>

### See also<a id="orgheadline139"></a>

:comments (See section :comments)   
:no-expand (See section :no-expand)   
:noweb-ref (See section :noweb-ref)   
:noweb-sep (See section :noweb-sep)   
:padline (See section :padline)   
:tangle (See section :tangle)

Concept of Noweb references.

---

## :noweb-ref<a id="orgheadline145"></a>

Specifies block&rsquo;s noweb reference resolution target.

### Options<a id="orgheadline141"></a>

### Remarks<a id="orgheadline142"></a>

This allows you to **avoid repeating the reference name** in your Org document: if
you set this header argument at the file or subtree level, the code blocks will
be concatenated into the **propagated** reference name &#x2013; if **property inheritance**
is turned on for the `noweb-ref` property.

>> About `org-babel-use-quick-and-dirty-noweb-expansion`: should it be
>> set to &rsquo;t&rsquo; by default ? I&rsquo;d be tempted to say yes, given the dramatic
>> performance gain
>
> should org-tangle and org-weave enable
> &ldquo;org-babel-use-quick-and-dirty-noweb-expansion&rdquo; before doing their
> jobs ? For now I let the default value to be `nil`, and I was
> wondering if it wouldn&rsquo;t be bette to do the opposite instead, that is,
> enable &ldquo;quick-and-dirty-noweb-expansion&rdquo; by default and provide a
> -noquick option.
> What do you think ?

The quick and dirty approach removes the ability for headings to
inherit the noweb properties of, and override, the properties of its
parent header.

That feature enables the true literate programming to remove it would
be a great loss.

### Examples<a id="orgheadline143"></a>

    *** Some subtree
    
    #+begin_src org
    first
    #+end_src
    
    #+begin_src org
    second
    #+end_src
    
    #+begin_src shell :results output :noweb yes
    echo <<accumulated>>
    #+end_src
    
    #+results:
    #+begin_example
    first
    second
    #+end_example

### See also<a id="orgheadline144"></a>

:noweb (See section :noweb)   
:noweb-sep (See section :noweb-sep)

The variable `org-babel-use-quick-and-dirty-noweb-expansion` controls XXX

---

## :noweb-sep<a id="orgheadline148"></a>

Specifies the string to use to separate accumulated noweb references.

### Options<a id="orgheadline146"></a>

By default a newline is used.

### See also<a id="orgheadline147"></a>

:noweb (See section :noweb)   
:noweb-ref (See section :noweb-ref)

---

## :padline<a id="orgheadline152"></a>

Controls insertion of padding lines in tangled code files.

### Options<a id="orgheadline149"></a>

-   **`:padline yes`:** (default)

-   **`:padline no`:** Gets rid of the **first blank line** preceding tangled output.

### Remarks<a id="orgheadline150"></a>

The padline is not inserted at the top of the file, only between blocks.

### See also<a id="orgheadline151"></a>

:noweb (See section :noweb)

---

## :post<a id="orgheadline157"></a>

**Post-processes** the **results** of a code block.

### Remarks<a id="orgheadline153"></a>

-   Post blocks must return results, eventually by adding a variable to them.  For
    example:
    
        (shell-command "beep")
        data

-   When a post argument is given, the results of the code block will temporarily
    be bound to the `*this*` variable.  This variable may then be included in header
    argument forms such as those used in `var` header argument specifications
    allowing passing of results to other code blocks, or direct execution via
    Emacs Lisp.

-   For example, the `:post` header argument could link to an Emacs Lisp source
    code block which does automatically refresh the inline images of (only)
    certain blocks (and not others).

-   The value of `:post` should be a **Babel call** (in the same format as e.g. a `#+call`
      line), not an Emacs Lisp form.

### Examples<a id="orgheadline154"></a>

We can have a sequence of forward chained blocks with length > 2.

    (* 2 in)

    (+ 1 in)

Putting the previous two together we get.

    4

### Buggy?<a id="orgheadline155"></a>

> whats wrong with this :post header arg:

THE FACT THAT THE VALUE OF :POST SHOULD BE A BABEL CALL!

> #+header: :post (format &ldquo;#+attr\_latex :float :placement {c}{scale=.6}\n%s&rdquo; **this**)
> #+begin\_src emacs-lisp
>  (+ 2 2)
> #+end\_src
>
> When I understand
>
> ,-&#x2014;
> | <http://orgmode.org/manual/post.html>
> \`-&#x2014;
>
> correctly, **this** should hold the block results, but I get
>
> ,-&#x2014;
> | Symbol&rsquo;s value as variable is void: **this**
> \`-&#x2014;

This works:

    (format "Do %sx then quit" x)

    (+ 2 2)

### Additional header arguments<a id="orgheadline156"></a>

Additional header arguments may be passed to the `:post`-function.

The following example illustrates the usage of the `:post` header argument.

The example shows how to use `:post` together with the `:colnames` header argument.

    (mapcar (lambda (row)
              (mapcar (lambda (cell)
                        (if (numberp cell)
                            (format fmt cell)
                          cell))
                      row))
            tbl)

    set.seed(42)
    data.frame(foo=rnorm(1))

---

## :prologue<a id="orgheadline160"></a>

Prepends text to code block body.

### Options<a id="orgheadline158"></a>

-   **`:prologue ""`:** (default)

-   **`:prologue <TEXT>`:** Prepends the value of the `:prologue` header argument to the code block
    body before execution.

### See also<a id="orgheadline159"></a>

:epilogue (See section :epilogue)

---

## :results<a id="orgheadline171"></a>

Specifies the type of results and how they will be collected and handled.

### Options<a id="orgheadline166"></a>

#### How the code block is evaluated<a id="orgheadline161"></a>

Specifies how the results should be **collected** from the code block&rsquo;s
evaluation.

-   **`:results value` (aka functional mode):** Specifies that the results of the block is the **value of the last
    statement** in your code, turned into an **Org mode table** if it is
    a list/vector type object.   
    (default)

-   **`:results output` (aka scripting mode):** Specifies that the results will be **everything printed to stdout** during
    execution of the code block.  The strings collected from `stdout` are not
    treated as values, but rather as **raw strings**.   
    (default for Ledger code blocks)

If you get the message `Source block produced no output`, try adding `:results
output`.

#### How the results are inserted into the Org mode buffer<a id="orgheadline165"></a>

-   Handling params

    Specifies if and how the results should be **inserted in the buffer**.
    
    -   **`:results replace`:** Inserts results after the code block, replacing any previously inserted
        results.   
        (default)
    
    -   **`:results silent`:** Sends the commands, echoes the results in the minibuffer (to see code block
        output), but **does not change the Org mode buffer** (even during export, **no
        results are inserted** into the exported document).   
        (default for Org and Screen code blocks)
    
    -   **`:results none`:** Silents the results, even for the minibuffer.  By definition, such a code
        block is run for its side effects.
    
    -   **`:results append`:** Builds new results onto existing results.
    
    -   **`:results prepend`:** Is the same as `append`, but puts new results before the existing results.

-   Type

    Specifies how the results should be **interpreted**.
    
    -   **`:results verbatim`:** Informs explicitly the code block that you will be returning a **string**, to
        **inhibit its interpretation** as a value &#x2014; the output will neither be
        converted into a table nor into a list.   
        (default for `raw`, `org` and `drawer` results)
    
    -   **`:results scalar`:** Is the same as `:results verbatim`.
    
    -   **`:results table`:** Interprets the results as an Org mode **table**.   
        (default)
    
    -   **`:results vector`:** Is the same as `:results table`.
    
    -   **`:results list`:** Writes an Org mode **list**.
        
            echo "vino blanco"
            echo "vino rosso"
            echo "vino tinto"
    
    -   **`:results file`:** **Interprets the results as a file path**, and insert it into the buffer using
        the Org mode file syntax.   
        (default for code blocks in graphics-only languages)
    
    -   **`:results graphics`:** (extra option for code blocks in maxima, octave and R)
        XXX

-   Format

    Specifies what type of results the code block will return.
    
    -   **`:results raw`:** Means that the input is a string (so hline processing is not performed).   
        (default for Org code blocks)   
        (implies `verbatim` results, unless otherwise stated)
    
    Note that it is allowable for raw results to include newlines (if the code
    block returns a newline): the purpose of raw results is specifically to
    **not** change the result.
    
    -   **`:results html`:** Specifies that the **results** of the code block is **raw HTML code** (which can
        be included correctly in HTML-based export targets).
        
        Is the same as `:wrap HTML`.
        
        Inserts the results inside a `#+begin_html` block.
    
    -   **`:results latex`:** Specifies that the **results** of the code block is **raw LaTeX code** (which can
        be included correctly in LaTeX-based export targets).
        
        Is the same as `:wrap LaTeX`.   
        (default for LaTeX code blocks)
        
        Inserts the results inside a `#+begin_latex` block.
    
    -   **`:results org`:** (implies `verbatim` results, unless otherwise stated)   
        **Wraps raw Org mode results** in a `#+begin_src org` block (dead data,
        **comma-escaped lines**).  This will be **exported as Org code block** (as `verbatim`
        or `listings` to LaTeX).
    
    -   **`:results drawer`:** (replacement of `:results wrap`, since Org 7.9.2)   
        (implies `verbatim` results, unless otherwise stated)   
        **Wraps code block results** in a `RESULTS` drawer.  This has the benefit of
        delimiting your results, and can be useful for really inserting pure **Org
        mode syntax** in the buffer (live data, **not escaped**) &#x2014; which can be
        included correctly **in all export targets**.
        
        You can put anything in your drawer besides a headline and another drawer.
        
        Why are drawers not the default for results?  Because the block may insert
        a headline, or another drawer, within the results drawer, thus breaking
        the document!
        
              cat <<EOF
              | a | b |
              |---+---|
              | 1 | 2 |
            
              [[./plots/file1.pdf]]
            
              [[./plots/file2.pdf]]
            EOF
        
        <table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">
        
        
        <colgroup>
        <col  class="org-right" />
        
        <col  class="org-right" />
        </colgroup>
        <thead>
        <tr>
        <th scope="col" class="org-right">a</th>
        <th scope="col" class="org-right">b</th>
        </tr>
        </thead>
        
        <tbody>
        <tr>
        <td class="org-right">1</td>
        <td class="org-right">2</td>
        </tr>
        </tbody>
        </table>
        
        <./plots/file1.pdf>
        
        <./plots/file2.pdf>
    
    -   **`:results code`:** This will be **exported as <LANG> code block** (as `verbatim` or `listings` to
        LaTeX).
    
    -   **`:results pp`:** Prints data.
        Puts the output in an `EXAMPLE` block? XXX
        
        You might try
        
            (cons (org-entry-get (point) "header-args" t)
                  (nth 2 (org-babel-get-src-block-info)))
    
    <p class="verse">
    &#xa0;&#xa0;&#xa0;&#xa0;&#xa0;>> #+begin\_src R :results pp replace<br  />
    &#xa0;&#xa0;&#xa0;&#xa0;&#xa0;>>  c(&ldquo;**\* New header2", "<./graph1.pdf>", "", "\*** and second header&rdquo;, &ldquo;and some text&rdquo; )<br  />
    &#xa0;&#xa0;&#xa0;&#xa0;&#xa0;>> #+end\_src<br  />
    &#xa0;&#xa0;&#xa0;&#xa0;&#xa0;>><br  />
    &#xa0;&#xa0;&#xa0;&#xa0;&#xa0;>> #+results:<br  />
    &#xa0;&#xa0;&#xa0;&#xa0;&#xa0;>> : \*\* New header2<br  />
    &#xa0;&#xa0;&#xa0;&#xa0;&#xa0;>> : <./graph1.pdf><br  />
    &#xa0;&#xa0;&#xa0;&#xa0;&#xa0;>> :<br  />
    &#xa0;&#xa0;&#xa0;&#xa0;&#xa0;>> : \*\* and second header<br  />
    &#xa0;&#xa0;&#xa0;&#xa0;&#xa0;>> : and some text<br  />
    <br  />
    >>>> I don&rsquo;t quite understand your code below and what it is supposed to do.<br  />
    >>><br  />
    >>> Drawers are probably better in most cases, but this code lets you use<br  />
    >>> &rsquo;:results pp replace&rsquo; while developing, but converts the fixed-width pp<br  />
    >>> results into raw results when desired (e.g. in a buffer copy before some<br  />
    >>> action is taken).<br  />
    >><br  />
    >> OK - I see. But what is the advantage in this approach? Drawer do<br  />
    >> perfectly what I want, i.e. encapsulating an org structure in a for<br  />
    >> replacement when re-calculated, inside the drawer they behave like<br  />
    >> normal org structure (folding, &#x2026;) and upon export they are exported as<br  />
    >> org code would be - or am I missing something?<br  />
    ><br  />
    > There are none (for you) then, but it might be useful anyway in some<br  />
    > situations, at least the results look more like part of the document<br  />
    > when you care about the looks of the org file too (not only the export<br  />
    > results).<br  />
    </p>

### Remarks<a id="orgheadline167"></a>

With `raw`, as there are **no obvious markers to delimit the results** in the Org mode
buffer, there is no way to know where raw results begin or end.  So, `:results
raw replace` will never work as expected: **raw results cannot be removed** (they
become part of the buffer), and you&rsquo;ll get duplicates when re-evaluating the
code block.

The sole purpose of `raw` results is to allow inserting an headline (a real
headline, not comma protected) in the buffer, because headlines cannot be
contained in anything else than headlines.

`raw` behaviour is only &ldquo;useful&rdquo; (i.e., mandatory) when you want to insert
a headline (or a drawer) as a result of a code block evaluation.  But then, you
lose the ability to replace results.

If the generated code doesn&rsquo;t contain an headline, you don&rsquo;t need `raw` results.

In any other case, `drawer+replace` is the superior choice.

The **best and simplest solution** for automatically generating org
headlines for export is&#x2026;

&#x2026;never to execute the source block by hand in org source file.  That way, the
results will appear only in the **temporary** copy of the buffer is parsed for
export and one does need to worry about demarcating the output with a :RESULTS:
drawer&#x2026;

Workaround: a block wrapper makes it possible for the entirety of the results
to be clearly located, regardless of their contents, and replaced (with new
results) by Babel upon code block re-evaluation.

No matter how special the results drawer is, it cannot (and shouldn&rsquo;t)
contain headlines.

<p class="verse">
> Or wrap the results in a drawer when you type C-c C-c, but render them as<br  />
> raw on export (which removes the drawer and replaces with raw results).<br  />
><br  />
> Like so:<br  />
><br  />
> #+header: :results (if (boundp &rsquo;backend) &ldquo;raw&rdquo; &ldquo;drawer&rdquo;)<br  />
> #+begin\_src emacs-lisp :exports both<br  />
><br  />
> (format &ldquo;\* headline\n1\n2\n5\n&rdquo;)<br  />
> #+end\_src<br  />
<br  />
That&rsquo;s a very nice tip - one small weakness is that it&rsquo;ll do the wrong<br  />
thing if you just happen to have a binding for &ldquo;backend&rdquo; outside of the<br  />
export mechanism.<br  />
</p>

A naked &ldquo;`tildes`&rdquo; will be marked up as &ldquo;\verb~tildes~&rdquo; under `:results raw` or
`:results latex raw`, and as &ldquo;`tildes`&rdquo; under `:results latex`.

### Examples<a id="orgheadline169"></a>

#### Interpreting the results as a file path<a id="orgheadline168"></a>

### See also<a id="orgheadline170"></a>

:exports (See section :exports)   
:file (See section :file)   
:wrap (See section :wrap)

---

## :rownames<a id="orgheadline174"></a>

Handles row names in tables.

### Options<a id="orgheadline172"></a>

-   **`:rownames no`:** (default)

-   **`:rownames yes`:** Tells Org that your first column contains row names.

### See also<a id="orgheadline173"></a>

:colnames (See section :colnames)   
:wrap (See section :wrap)

---

## :sep<a id="orgheadline179"></a>

Specifies a delimiter for reading or writing **tabular results**.

### Options<a id="orgheadline175"></a>

-   **nothing:** Uses TAB as default separator.

-   **`:sep <SEPARATOR>`:** Sets separator to `<SEPARATOR>`.

### Examples<a id="orgheadline177"></a>

#### Saving the tabular output to a CSV file<a id="orgheadline176"></a>

Save the output of `ls -l` as a `.csv` file.

    ls -l

Recall that `:results value` is the default.

### See also<a id="orgheadline178"></a>

:file (See section :file)

---

## :session<a id="orgheadline183"></a>

Shares data and persists state between (evaluation of) different code blocks.

### Options<a id="orgheadline180"></a>

-   **`:session none`:** Disables session-based evaluation.   
    (default)

-   **`:session <NAME>`:** Performs evaluation using a persistently running inferior process to which
    the code block is submitted.   
    (default for Screen code blocks: session name set to `default`)

### Remarks<a id="orgheadline181"></a>

-   If a code block has a `:session` header argument, then it is assumed that the
    block should be evaluated regardless of its `:exports` header argument, because
    it could change the state of the session.

-   Even if you don&rsquo;t need to share data between blocks, you can still use
    sessions in order not to wait for the interpreter to start up for each block!

-   This also allows for manual inspection of the results of evaluation.

-   When you work on different files at the same time, you may want to use:
    -   the same session, or
    -   differently named sessions (running their own process).

-   Adding session to a shell source block has the following impact: commands will
    be run from `~` directory (instead of the local one).

### See also<a id="orgheadline182"></a>

:eval (See section :eval)   
:exports (See section :exports)

---

## :shebang<a id="orgheadline187"></a>

Uses preamble for tangled files (and **make** them **executable**).

### Options<a id="orgheadline184"></a>

-   **`:shebang <SHEBANG>`:** Specifies the shebang.

### Remarks<a id="orgheadline185"></a>

The preamble line is only used for tangling, not during evaluation.

Note that whenever a file is tangled which includes a shebang line, Org Babel
will make the file executable, so there is good reason to **only add shebangs
at the source-code level**.

### Examples<a id="orgheadline186"></a>

Set the shebang.

    printf "with a shebang line, I can be run as a script!\n"

---

## :tangle<a id="orgheadline191"></a>

Toggles tangling and specify file name.

### Options<a id="orgheadline188"></a>

-   **`:tangle no`:** (default)

-   **`:tangle yes`:** Tangles to a target file named after the name of the Org mode file
    (`$(basename).<MODE-EXT>`).

-   **`:tangle <FILENAME>`:** Specifies an alternate target file.

### Remarks<a id="orgheadline189"></a>

Tangling works for **any** programming language (even those which have yet to be
created and have no explicit Emacs or Org mode support) because, on tangling,
the code block is simply treated as text.

Blocks to the same target file are **concatenated** during tangling, IN THE ORDER
AT WHICH THEY APPEAR IN THE ORG SOURCE FILE.

Blocks which are under a `COMMENT`&rsquo;ed heading (including parents) are not
tangled.

Propagating changes back from tangled code to Org mode blocks (aka
&ldquo;**detangling**&rdquo;) is possible with the function `org-babel-detangle`.

### See also<a id="orgheadline190"></a>

:noweb (See section :noweb)

---

## :tangle-mode<a id="orgheadline193"></a>

Controls the permissions of tangled files.

### Example<a id="orgheadline192"></a>

    plot(1)

---

## :var<a id="orgheadline203"></a>

**Passes arguments** to code blocks.

### Options<a id="orgheadline194"></a>

-   **`:var <NAME>=<VALUE>`:** Assigns a **default** value (literal or reference to a literal, a table, a
    list or a code block) to the argument.

### Remarks<a id="orgheadline195"></a>

Multiple `var` specifications behind a single `:var` are allowed.  The multiple
var arguments must be comma-separated:

    #+PROPERTY: var foo=1, bar=2
    
    #+begin_src emacs-lisp
    (+ foo bar)
    #+end_src
    
    #+results:
    : 3

and

    #+begin_src emacs-lisp :var foo="hello", bar="world"
    (concat foo " " bar)
    #+end_src
    
    #+results:
    : hello world

<div class="inlinetask">
<b><span class="todo TODO">TODO</span> Question about prefix char</b><br  />
So, the prefix char of such lines should be `|`, `:` or `-`?
</div>

The value passed to an argument can be:

-   a **literal** value:
    -   string (value wrapped in double quotes)
    -   number

-   a **reference** to **named data**, such as:
    -   literal **example block**
    -   (part of) a **table** (when naming a table, `#+name:` should replace `#+tblname:`,
        which still exists for backwards compatibility)
    -   (level-1) **list**

-   a **reference** to the results of a **named code block**

-   **Emacs Lisp code**
    
    Look in the manual at the description of what causes header arguments to be
    interpreted as Emacs Lisp.
    
    **Note --** Eric means about &rsquo;(&#x2026;) or (&#x2026;)
    
    Arbitrary Emacs Lisp can be placed inside of header arguments, and the `org-sbe`
    take the name of a code block and returns its results.

To specify a **subset** of a table, give a `[row,column]` index.

-   Indices are numerical and begin at 0.
-   Column names, hlines and row names do count in the indexes.
-   Negative numbers imply counting back from the end.
-   Ranges (consecutive rows or columns) are specified using the colon notation
    `first:last`.
-   If row or column is empty, then all rows or all columns are taken.

To reference a variable in an external file, use the syntax `:var
data=<FILE>:<VAR>`.  Note that, in order to work, the files must be in the same
directory.

`:var` introduces code block dependencies.
include anything in the Org mode file that takes a `#+name:`, `#+begin_example`
blocks, other code blocks, and the results of other code blocks.  When
a reference is made to another code block, the referenced block will be
evaluated whenever needed, in order to supply its value to the referencing
block.  If the referenced block is cached (see @ref{cache}), its value will be
reused if possible, instead of being re-calculated.  If the referring code block
is cached, its hash value will depend on the value of all the code blocks it
references.

### Examples<a id="orgheadline202"></a>

#### Literal string<a id="orgheadline196"></a>

    value

#### Literal number<a id="orgheadline197"></a>

    42

#### Reference to a literal example block<a id="orgheadline198"></a>

    Les sanglots longs
    des violons de l'automne

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">Les sanglots longs</td>
</tr>


<tr>
<td class="org-left">des violons de l'automne</td>
</tr>
</tbody>
</table>

#### Reference to part of a table<a id="orgheadline199"></a>

<table id="orgtable7" border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-right" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-right">0</td>
<td class="org-left">foo1</td>
<td class="org-left">bar1</td>
</tr>


<tr>
<td class="org-right">1</td>
<td class="org-left">foo2</td>
<td class="org-left">bar2</td>
</tr>


<tr>
<td class="org-right">2</td>
<td class="org-left">foo3</td>
<td class="org-left">bar3</td>
</tr>
</tbody>
</table>

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-right" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-right">1</td>
<td class="org-left">foo2</td>
<td class="org-left">bar2</td>
</tr>


<tr>
<td class="org-right">2</td>
<td class="org-left">foo3</td>
<td class="org-left">bar3</td>
</tr>
</tbody>
</table>

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-right" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-right">1</td>
<td class="org-left">foo2</td>
<td class="org-left">bar2</td>
</tr>


<tr>
<td class="org-right">2</td>
<td class="org-left">foo3</td>
<td class="org-left">bar3</td>
</tr>
</tbody>
</table>

<table id="orgtable8" border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-right" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-right">id</th>
<th scope="col" class="org-left">var1</th>
<th scope="col" class="org-left">var2</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-right">0</td>
<td class="org-left">foo1</td>
<td class="org-left">bar1</td>
</tr>


<tr>
<td class="org-right">1</td>
<td class="org-left">foo2</td>
<td class="org-left">bar2</td>
</tr>


<tr>
<td class="org-right">2</td>
<td class="org-left">foo3</td>
<td class="org-left">bar3</td>
</tr>
</tbody>
</table>

    hline

#### Reference to a list<a id="orgheadline200"></a>

-   foo
-   bar
-   baz

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">foo</td>
<td class="org-left">bar</td>
<td class="org-left">baz</td>
</tr>
</tbody>
</table>

#### Reference to a code block<a id="orgheadline201"></a>

It is possible to **chain code blocks** (possibly in different languages) in `:var`
lines, as shown:

    rowSums(x)

---

## :wrap<a id="orgheadline211"></a>

Delimit the results (of source block evaluation).

### Options<a id="orgheadline204"></a>

-   **nothing:** Tells Org Babel **not** to **wrap** the results.   
    (default)

-   **`:wrap`:** Specifies to wrap the results in a `#+begin/end_results` block.

-   **`:wrap <MARKUP>`:** Specifies the name of the block (`#+begin/end_<markup>`) with which to wrap
    the results.

**XXX How to unset it (when set in a higher level)?**

### Remarks<a id="orgheadline205"></a>

The `:wrap` header argument gives you control over the formatting of results
from code blocks.

### Examples<a id="orgheadline209"></a>

#### Using `:wrap` with no value<a id="orgheadline206"></a>

The following example wraps the results in `#+begin_results` &#x2026; `#+end_results`
block.

<div class="results">
<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-right" />

<col  class="org-right" />

<col  class="org-right" />

<col  class="org-right" />
</colgroup>
<tbody>
<tr>
<td class="org-right">1</td>
<td class="org-right">4</td>
<td class="org-right">7</td>
<td class="org-right">10</td>
</tr>


<tr>
<td class="org-right">2</td>
<td class="org-right">5</td>
<td class="org-right">8</td>
<td class="org-right">11</td>
</tr>


<tr>
<td class="org-right">3</td>
<td class="org-right">6</td>
<td class="org-right">9</td>
<td class="org-right">12</td>
</tr>
</tbody>
</table>

</div>

#### Using `:wrap` with a string value<a id="orgheadline207"></a>

The following examples puts the **output in an `example` block**.

    Some results wrapped in an example block.

#### Using `:wrap` to produce a source code block in a named language<a id="orgheadline208"></a>

The following examples puts the results into a `SRC` code block (associated to
the `c++` mode).

On LaTeX export, the `SRC` block will be exported as `c++` code under `listings`
instead of being wrapped in a `\begin{verbatim}..\end{verbatim}` environment.

Alternatively, you can use the :post header argument to wrap the results in a
source block.

### See also<a id="orgheadline210"></a>

:results (See section :results)

---

# Extra language-specific header arguments<a id="orgheadline215"></a>

The following table lists the header arguments that some languages use.

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">:border</td>
<td class="org-left">:java</td>
</tr>


<tr>
<td class="org-left">:buffer</td>
<td class="org-left">:main</td>
</tr>


<tr>
<td class="org-left">:classname</td>
<td class="org-left">:nullvalue</td>
</tr>


<tr>
<td class="org-left">:cmd</td>
<td class="org-left">:out-file</td>
</tr>


<tr>
<td class="org-left">:cmd-line</td>
<td class="org-left">:package</td>
</tr>


<tr>
<td class="org-left">:cmpflag</td>
<td class="org-left">:packages</td>
</tr>


<tr>
<td class="org-left">:colname-names</td>
<td class="org-left">:pdf</td>
</tr>


<tr>
<td class="org-left">:database</td>
<td class="org-left">:pdfheight</td>
</tr>


<tr>
<td class="org-left">:db</td>
<td class="org-left">:pdfpng</td>
</tr>


<tr>
<td class="org-left">:dbhost</td>
<td class="org-left">:pdfwidth</td>
</tr>


<tr>
<td class="org-left">:dbpassword</td>
<td class="org-left">:preamble (See section :preamble)</td>
</tr>


<tr>
<td class="org-left">:dbuser</td>
<td class="org-left">:result-params</td>
</tr>


<tr>
<td class="org-left">:defines</td>
<td class="org-left">:result-type</td>
</tr>


<tr>
<td class="org-left">:engine</td>
<td class="org-left">:results\_switches</td>
</tr>


<tr>
<td class="org-left">:eps</td>
<td class="org-left">:return</td>
</tr>


<tr>
<td class="org-left">:filetype</td>
<td class="org-left">:rowname-names</td>
</tr>


<tr>
<td class="org-left">:fit</td>
<td class="org-left">:scheme</td>
</tr>


<tr>
<td class="org-left">:flags</td>
<td class="org-left">:separator (See section :separator)</td>
</tr>


<tr>
<td class="org-left">:headers</td>
<td class="org-left">**:stdin**</td>
</tr>


<tr>
<td class="org-left">:imagemagick</td>
<td class="org-left">:tangle</td>
</tr>


<tr>
<td class="org-left">:iminoptions</td>
<td class="org-left">:term</td>
</tr>


<tr>
<td class="org-left">:imoutoptions</td>
<td class="org-left">:terminal</td>
</tr>


<tr>
<td class="org-left">:in-file</td>
<td class="org-left">:xmp-option</td>
</tr>


<tr>
<td class="org-left">**:includes**</td>
<td class="org-left">&#xa0;</td>
</tr>
</tbody>
</table>

XXX Title disappears!?

## :preamble<a id="orgheadline213"></a>

Specifies code block prefix for code evaluation.

## :separator<a id="orgheadline214"></a>

Specifies a different results separator.

---

# Data types<a id="orgheadline216"></a>

---

# Hooks<a id="orgheadline217"></a>

-   **`org-babel-post-tangle-hook`:** When the hook is executed, the current buffer (as identified by the
    `current-buffer` function) will be a buffer visiting the **file** of the **tangled
    code**.
    
    Example applications could include post-processing, compilation or
    evaluation of tangled code files.

---

# Subtleties<a id="orgheadline219"></a>

## Shell mode<a id="orgheadline218"></a>

<p class="verse">
> Why is the \`diff&rsquo; command alone not exported while the piped \`diff&rsquo; via<br  />
> \`cat&rsquo; works?<br  />
<br  />
The exit code is not zero since diff found differences, so Babel assumes the<br  />
script produced an error.  Try this instead when you don&rsquo;t know what the return<br  />
code of the last command will be or if you know that it isn&rsquo;t zero even when no<br  />
error occured:<br  />
<br  />
\\#+begin\_src shell :exports results :results output<br  />
diff testdiff.txt.orig testdiff.txt<br  />
:<br  />
\\#+end\_src<br  />
<br  />
> Is this this a bug?<br  />
<br  />
I don&rsquo;t think so, although Babel could give a more enlightening message about<br  />
why it didn&rsquo;t evaluate STDOUT.  It gives this on your original example:<br  />
<br  />
Babel evaluation exited with code 1<br  />
Code block produced no output.<br  />
</p>

---

# Colophon<a id="orgheadline227"></a>

## References<a id="orgheadline220"></a>

The primary sources for this documentation are:

-   [The Org Manual](http://orgmode.org/org.pdf)

-   [Worg](http://orgmode.org/worg/) site

-   Mailing list [emacs-orgmode@gnu.org](emacs-orgmode@gnu.org)

## Contributing<a id="orgheadline224"></a>

### Issues<a id="orgheadline221"></a>

Report issues and suggest features and improvements on the [GitHub issue tracker](https://github.com/fniessen/refcard-org-babel /issues/new).

### Patches<a id="orgheadline222"></a>

I love contributions!  Patches under any form are always welcome!

### Donations<a id="orgheadline223"></a>

If you like the refcard-org-babel project, you can show your appreciation and
help support future development by making a [donation](https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=VCVAS6KPDQ4JC&lc=BE&item_number=refcard%2dorg%2dbabel&currency_code=EUR&bn=PP%2dDonationsBF%3abtn_donate_LG%2egif%3aNonHosted) through PayPal.

Regardless of the donations, refcard-org-babel will always be free both as in
beer and as in speech.

## License<a id="orgheadline225"></a>

Copyright (C) 2014-2015 Free Software Foundation, Inc.

Author: Fabrice Niessen   
Keywords: org-mode org-babel reference card

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program.  If not, see <http://www.gnu.org/licenses/>.

## Change Log<a id="orgheadline226"></a>

    a5e6401 (HEAD, origin/master, master) Document deprecated syntax (one header arg per PROPERTY)
    e956d85 Add "Edit on GitHub" button
    8392d06 Update README.org
    48c7c02 Fix typos, add separators and Change Log
    add1feb Remove useless headlines in "See also" sections

<a href="http://opensource.org/licenses/GPL-3.0">
  <img src="http://img.shields.io/:license-gpl-blue.svg" alt=":license-gpl-blue.svg" />
</a>

<a href="https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=VCVAS6KPDQ4JC&lc=BE&item_number=refcard%2dorg%2dbabel&currency_code=EUR&bn=PP%2dDonationsBF%3abtn_donate_LG%2egif%3aNonHosted">
  <img src="https://www.paypalobjects.com/en_US/i/btn/btn_donate_LG.gif" alt="btn_donate_LG.gif" />
</a>
