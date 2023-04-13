# Inheritance

Header arguments can be set at different levels of a hierarchy:

1.  **Default** header arguments (See section 1.1) shipped with Org mode
2.  Default **languages-specific** header arguments (See section 1.2) shipped with Org mode
3.  **Buffer** (or file) level header arguments (See section 1.3)
4.  **Subtree** header arguments (See section 1.4)
5.  **Code block** header arguments (See section 1.5)
6.  **Call line** header arguments (See section 1.6)

At the top of the hierarchy, default header arguments shipped with Org mode
are the most general of all: they define behavior common to all code blocks,
unless set otherwise, inherited by all lower levels.

Header arguments near the bottom of the hierarchy provide behavior more
specific to a (group of) code block(s).

## Default header arguments shipped with Org mode

-   Variable `org-babel-default-header-args` for source blocks
-   Variable `org-babel-default-inline-header-args` for inline code blocks
-   Variable `org-babel-default-lob-header-args` for `#+call` lines

These default header arguments can be set by the user:

    ;; add default arguments to use when evaluating a source block
    (add-to-list 'org-babel-default-header-args
                 '(:noweb . "yes"))

This can also be done file-wide through the use of *file local variables*.

## Default languages-specific header arguments shipped with Org mode

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

-   property lines (See section 1.3) or
-   *file local variables*.

## Buffer (or file) level header arguments

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

That can be over-ridden on a per-subtree (See section 1.4) or per-block (See section 1.5) basis.

<div class="warning">
Any property specification, unless it is postfixed with a `+`, will *reset* the
value of that property to its current value.

</div>

In the case of two `#+PROPERTY:` lines for the same property, the property will
have the later value.

But there is a general mechanism for the **concatenation of property** strings
(**accumulated values**):

    #+PROPERTY: header-args:R :exports results
    #+PROPERTY: header-args:R+ :width 800

## Subtree header arguments

In contrast to property lines, a `:PROPERTIES:` block is only valid **for the given
tree (and subtrees)**:

    * Outline heading
     :PROPERTIES:
     :header-args: :results output :cache yes
     :END:

## Code block header arguments

    #+header: :exports code :var data=2
    #+begin_src emacs-lisp
    ... some code ...
    #+end_src

## Call line header arguments

# Standard header arguments

The following table lists the standard header arguments that Org Babel uses.

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">:cache (See section 3)</td>
<td class="org-left">:noweb-ref (See section 17)</td>
</tr>


<tr>
<td class="org-left">:cmdline (See section 4)</td>
<td class="org-left">:noweb-sep (See section 18)</td>
</tr>


<tr>
<td class="org-left">:colnames (See section 5)</td>
<td class="org-left">:padline (See section 19)</td>
</tr>


<tr>
<td class="org-left">:comments (See section 6)</td>
<td class="org-left">:post (See section 20)</td>
</tr>


<tr>
<td class="org-left">:dir (See section 7)</td>
<td class="org-left">:prologue (See section 21)</td>
</tr>


<tr>
<td class="org-left">:epilogue (See section 8)</td>
<td class="org-left">:results (See section 22)</td>
</tr>


<tr>
<td class="org-left">:eval (See section 9)</td>
<td class="org-left">:rownames (See section 23)</td>
</tr>


<tr>
<td class="org-left">:exports (See section 10)</td>
<td class="org-left">:sep (See section 24)</td>
</tr>


<tr>
<td class="org-left">:file (See section 11)</td>
<td class="org-left">:session (See section 25)</td>
</tr>


<tr>
<td class="org-left">:file-desc (See section 12)</td>
<td class="org-left">:shebang (See section 26)</td>
</tr>


<tr>
<td class="org-left">:hlines (See section 13)</td>
<td class="org-left">:tangle (See section 27)</td>
</tr>


<tr>
<td class="org-left">:mkdirp (See section 14)</td>
<td class="org-left">:tangle-mode (See section 28)</td>
</tr>


<tr>
<td class="org-left">:no-expand (See section 15)</td>
<td class="org-left">:var (See section 29)</td>
</tr>


<tr>
<td class="org-left">:noweb (See section 16)</td>
<td class="org-left">:wrap (See section 30)</td>
</tr>
</tbody>
</table>

<div class="note">
The argument of any header option can be replaced by an ELisp form &#x2013; which
should return a string (or a list of strings, depending on the case).

</div>

---

# :cache

Avoids re-evaluating unchanged code blocks.

## Options

-   **`:cache no`:** (default)

-   **`:cache yes`:** Avoids re-evaluating unchanged code blocks by associating a **hash** of the
    *expanded* code block (= code block and parameters) with the **results**.  It
    allows the results to be returned without having to re-run the code
    block &#x2014; unless the code or the input parameters have changed.

## Remarks

In order for caching to work (i.e., **no evaluation** when triggered either
**interactively** or **during export**), the results of the code block must be present
in the Org mode file: you must first evaluate it manually, leaving the results
(with the hash tag) saved within the Org mode file.

Note that same input does **not** always **guarantee the same output**, e.g.,

    date

Though, this code block shouldn&rsquo;t be marked `:cache` unless the desired (and
odd) behavior is to have a datestamp that is only updated when the user
forcibly re-evaluates the block (with `C-u C-c C-v C-e`).

## Examples

### Avoid re-evaluating unchanged code blocks

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

### Avoid re-evaluating code blocks unless some process restarts

The following example allows to include the PID of the R process in the results
hash, so that the code would be rerun only if the R process (session) restarts.

    ps -a | grep "$R" | grep -v 'grep' | awk '{print $2}'

    # code to perform side effect
    x <- 'side effect'
    'done' # add something small to get a results block

## See also

:eval (See section 9)   
:exports (See section 10)

---

# :cmdline

## Options

-   **nothing:** (default)

-   **`:cmdline <...>`:** Pass some command line arguments.

## Remarks

The `:cmdline` header argument is supported by a couple of languages.

## Examples

For shell, this allows to make the code inside a Babel code block similar to
a real shell script.

    echo $2

The script can use `$@` for its positional parameters.

    echo "$@"

Also, calling the script using `#+call` is like calling it from another shell
script (supplying the value in the call line).

---

# :colnames

Handles **column names in tables**.

## Options

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

## Remarks

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

### Notes

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

## Examples

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

### Using no `:colnames` header argument

The following example outputs the table without column names.

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


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

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


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

### Using `:colnames no`

The following example outputs all the rows of the table, considering there is
no column names.

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


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

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-right" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-right">num</td>
<td class="org-left">word</td>
</tr>


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

### Using `:colnames yes`

The following example outputs the table with its column names.

    echo "$data"

    ((1 "one") (2 "two") (3 "three"))

### Show the labels of the vertical and the horizontal axes

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

### Utility function

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

### `#+call` get the first row of output table lost when using latex export

`org-babel-execute:python` does its own formatting.  `#+call:` on the other hand
passes the results to Babel.

The workaround is to use:

    #+call: t1() :colnames yes

## See also

:hlines (See section 13)   
:rownames (See section 23)

---

# :comments

Controls the insertion of extra comments into the tangled code files to allow
backward linking from tangled code blocks to the original code blocks (from
which the code was tangled).

## Options

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

## See also

:tangle (See section 27)

The variable `org-babel-tangle-use-relative-file-links` controls whether files
and links in comments in tangled files use relative or absolute path names (it
defaults to relative paths).

---

# :dir

Specifies the default (possibly **remote**) directory during code block execution.

## Options

-   **nothing:** Uses the directory associated with the **current buffer**.

-   **`:dir <DIR>`:** Specifies to use `DIR` as the **default directory** for code block execution.

## Remarks

You can use the `:dir` header argument for **remote execution**.  The directory is
specified using using [Tramp filename syntax](http://www.gnu.org/software/emacs/manual/html_node/tramp/Filename-Syntax.html).

`:results output` seems to be necessary!

## Examples

WHY DON&rsquo;T WE HAVE TO SPECIFY :OUTPUT FOR THE SHELL BLOCK?

    hostname

    hostname

    ls -la

    SELECT 2+2 AS 'four', 1+1 AS 'one';

## See also

:file (See section 11)

---

# :epilogue

Appends text to code block body.

## Options

-   **`:epilogue ""`:** (default)

-   **`:epilogue <TEXT>`:** Appends the value of the `:epilogue` header argument to the code block
    body before execution.

## See also

:prologue (See section 21)

---

# :eval

Specifies permissions for *every* execution of code blocks.

## Options

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

## Remarks

Sometimes, to **switch off execution** of code blocks **during export**, you can find
it easy to simply manually generate the results of a code block (e.g., through
an interactive evaluation), and set the `:eval` property of the code block to
`never-export`.

Note that, unlike tangling (See section 27), evaluation requires the specific language to be
supported for both performing the evaluation and collecting the results (See section 22).

## See also

:cache (See section 3)   
:exports (See section 10)   
:session (See section 25)

Variable `org-confirm-babel-evaluate`.

---

# :exports

Specifies how code and/or results should be handled **during export**.

## Options

-   **`:exports none`:** Doesn&rsquo;t include anything in the exported file.

-   **`:exports code`:** Includes (only) the body of the code block into the exported file.   
    (default)   
    (default for Org code blocks)

-   **`:exports results`:** Includes (only) the **results block** in the exported file.   
    (default for *inline* code blocks)   
    (default for LaTeX code blocks)   
    (default for code blocks in graphics-only languages)

-   **`:exports both`:** Includes both the code block and the results (See section 9) in the exported file.

## Remarks

-   When `:exports` is set to `none` or `code`, Org Babel will **not run (See section 9)** the code block
    **during export**, avoiding to (re-)generate the results on every export.  In
    particular, use that on code blocks which cannot be executed (See section 9) on
    their own.
    
    This has **no effect on interactive evaluation (See section 9)**, though.

-   When `:exports` is set to `results` or `both`, if evaluation is allowed during
    export, the code block will be (re-)evaluated (See section 9) during export.  Otherwise, the
    current (unchanged) results block, when present, will be included **in the
    exported file**.

-   Note that the `:exports` option is only relevant for code blocks, not inline
    code.

-   A code block in a subtree tagged `:noexport:` will still be evaluated, if
    evaluation is allowed during export, because its side-effects may be needed
    for code run elsewhere.  If you don&rsquo;t want that, set `:eval` accordingly.

## See also

:cache (See section 3)   
:eval (See section 9)   
:results (See section 22)

---

# :file

Specifies to **write the results to a file**.

## Options

-   **`:file <FILENAME>`:** Writes the **results** from the code block evaluation to `<FILENAME>` and
    inserts (for the **results block**) a **link to the file** into the Org mode
    buffer.

## Remarks

Extension can be everything: `.png`, `.pdf`, `.txt`, `.csv`, etc.

When relative, the filename is interpreted relatively to the default
directory (See section 7).

-   For **graphics-only languages** (e.g. *asymptote*, *ditaa*, *dot*, *gnuplot*,
    *mscgen*, *plantuml*), the &ldquo;results&rdquo; is the **graphics**, and a link to the
    image is placed in the Org buffer.

-   For **general-purpose languages** (e.g. *emacs-lisp*, *python*, *R*, *ruby*, *sh*), the
    &ldquo;results&rdquo; written to file is the **normal Org Babel results** (string, number,
    table).
    
    When generating **graphics**, including the `:results output graphics file` header argument is
    **required**, in addition to `:file <FILENAME>`, in order for graphical output to be
    sent automatically to file.  If `:file` is supplied, but not `:results output graphics file`,
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

## Examples

### Saving the textual output from a general-purpose language to a text file

Send the text output of `ls -l` directly to a file:

    ls -l

<dirlisting.txt>

Recall that `:results value` is the default.

### Saving the graphical output from a general-purpose language to an image file

    plot(1:10, (1:10)^2)

![img](images/square.png)

### Saving the graphical output from a graphics language to an image file

1.  Dotty

        digraph G {
          a -> b [label="hello", style=dashed];
          a -> c [label="world"];
          b -> c;
          b [shape=Mdiamond, label="this is b"];
          c [shape=polygon, sides=5, peripheries=3];
        }
    
    ![img](images/dot.png)
    
    Recall that `:exports results` is the default for graphics-only languages.

2.  R

    Choose PNG extension (and not PDF) to preview the results in the Org buffer
    itself.
    
        plot(1:10, (1:10)^2)

## See also

:dir (See section 7)   
:results (See section 22)   
:sep (See section 24) (for saving tabular results)

---

# :file-desc

Specifies a description for file results.

---

# :hlines

Handles **horizontal lines** in input tables.

## Options

-   **`:hlines no`:** Strips horizontal lines from the input table.   
    (default)

-   **`:hlines yes`:** Preserves horizontal lines in the input table.   
    (default for Emacs Lisp code blocks)

## Remarks

**Don't confound this with the :colnames (See section 5) machinery.**

## Examples

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

## See also

:colnames (See section 5)

---

# :mkdirp

Toggles creation of parent directories of target files during tangling.

## Options

-   **`:mkdirp no`:** Don&rsquo;t create the directories if they don&rsquo;t exist.

-   **`:mkdirp yes`:** Create the directories if they don&rsquo;t exist.

---

# :no-expand

Turns off the code blocks expansion **during tangling**.

## Remarks

`:no-expand` has no effect during execution.

## See also

:noweb (See section 16)   
:noweb-ref (See section 17)   
:noweb-sep (See section 18)   

---

# :noweb

Specifies when expansion of &ldquo;noweb&rdquo; style references should occur.

## Options

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

## Syntax of noweb references

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

## Examples

### Expand block

### Execute block

Note the parens in the noweb reference:

    echo "["
    ls *.org | sed 's/$/;/'
    echo "]"

### Expand variable in tangled code

## See also

:comments (See section 6)   
:no-expand (See section 15)   
:noweb-ref (See section 17)   
:noweb-sep (See section 18)   
:padline (See section 19)   
:tangle (See section 27)

Concept of Noweb references.

---

# :noweb-ref

Specifies block&rsquo;s noweb reference resolution target.

## Options

## Remarks

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

## Examples

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

## See also

:noweb (See section 16)   
:noweb-sep (See section 18)

The variable `org-babel-use-quick-and-dirty-noweb-expansion` controls XXX

---

# :noweb-sep

Specifies the string to use to separate accumulated noweb references.

## Options

By default a newline is used.

## See also

:noweb (See section 16)   
:noweb-ref (See section 17)

---

# :padline

Controls insertion of padding lines in tangled code files.

## Options

-   **`:padline yes`:** (default)

-   **`:padline no`:** Gets rid of the **first blank line** preceding tangled output.

## Remarks

The padline is not inserted at the top of the file, only between blocks.

## See also

:noweb (See section 16)

---

# :post

**Post-processes** the **results** of a code block.

## Remarks

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

## Examples

We can have a sequence of forward chained blocks with length > 2.

    (* 2 in)

    (+ 1 in)

Putting the previous two together we get.

    4

## Buggy?

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

## Additional header arguments

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

# :prologue

Prepends text to code block body.

## Options

-   **`:prologue ""`:** (default)

-   **`:prologue <TEXT>`:** Prepends the value of the `:prologue` header argument to the code block
    body before execution.

## See also

:epilogue (See section 8)

---

# :results

Specifies the type of results and how they will be collected and handled.

## Options

### How the code block is evaluated

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

### How the results are inserted into the Org mode buffer

1.  Handling params

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

2.  Type

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
    
    -   **`:results output graphics file`:** (extra option for code blocks in maxima, octave and R)
        XXX

3.  Format

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

## Remarks

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

## Examples

### Interpreting the results as a file path

## See also

:exports (See section 10)   
:file (See section 11)   
:wrap (See section 30)

---

# :rownames

Handles row names in tables.

## Options

-   **`:rownames no`:** (default)

-   **`:rownames yes`:** Tells Org that your first column contains row names.

## See also

:colnames (See section 5)   
:wrap (See section 30)

---

# :sep

Specifies a delimiter for reading or writing **tabular results**.

## Options

-   **nothing:** Uses TAB as default separator.

-   **`:sep <SEPARATOR>`:** Sets separator to `<SEPARATOR>`.

## Examples

### Saving the tabular output to a CSV file

Save the output of `ls -l` as a `.csv` file.

    ls -l

Recall that `:results value` is the default.

## See also

:file (See section 11)

---

# :session

Shares data and persists state between (evaluation of) different code blocks.

## Options

-   **`:session none`:** Disables session-based evaluation.   
    (default)

-   **`:session <NAME>`:** Performs evaluation using a persistently running inferior process to which
    the code block is submitted.   
    (default for Screen code blocks: session name set to `default`)

## Remarks

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

## See also

:eval (See section 9)   
:exports (See section 10)

---

# :shebang

Uses preamble for tangled files (and **make** them **executable**).

## Options

-   **`:shebang <SHEBANG>`:** Specifies the shebang.

## Remarks

The preamble line is only used for tangling, not during evaluation.

Note that whenever a file is tangled which includes a shebang line, Org Babel
will make the file executable, so there is good reason to **only add shebangs
at the source-code level**.

## Examples

Set the shebang.

    printf "with a shebang line, I can be run as a script!\n"

---

# :tangle

Toggles tangling and specify file name.

## Options

-   **`:tangle no`:** (default)

-   **`:tangle yes`:** Tangles to a target file named after the name of the Org mode file
    (`$(basename).<MODE-EXT>`).

-   **`:tangle <FILENAME>`:** Specifies an alternate target file.

## Remarks

Tangling works for **any** programming language (even those which have yet to be
created and have no explicit Emacs or Org mode support) because, on tangling,
the code block is simply treated as text.

Blocks to the same target file are **concatenated** during tangling, IN THE ORDER
AT WHICH THEY APPEAR IN THE ORG SOURCE FILE.

Blocks which are under a `COMMENT`&rsquo;ed heading (including parents) are not
tangled.

Propagating changes back from tangled code to Org mode blocks (aka
&ldquo;**detangling**&rdquo;) is possible with the function `org-babel-detangle`.

## See also

:noweb (See section 16)

---

# :tangle-mode

Controls the permissions of tangled files.

## Example

    plot(1)

---

# :var

**Passes arguments** to code blocks.

## Options

-   **`:var <NAME>=<VALUE>`:** Assigns a **default** value (literal or reference to a literal, a table, a
    list or a code block) to the argument.

## Remarks

Multiple `var` specifications behind a single `:var` are allowed.  The multiple
var arguments must be space-separated:

    
    #+PROPERTY: header-args :var foo=1 bar=2
    #+begin_src emacs-lisp
    (+ foo bar)
    #+end_src
    
    #+results:
    : 3

and

    #+begin_src emacs-lisp :var foo="hello" bar="world"
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

## Examples

### Literal string

    value

### Literal number

    42

### Reference to a literal example block

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

### Reference to part of a table

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

### Reference to a list

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

### Reference to a code block

It is possible to **chain code blocks** (possibly in different languages) in `:var`
lines, as shown:

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

    rowSums(x)

---

# :wrap

Delimit the results (of source block evaluation).

## Options

-   **nothing:** Tells Org Babel **not** to **wrap** the results.   
    (default)

-   **`:wrap`:** Specifies to wrap the results in a `#+begin/end_results` block.

-   **`:wrap <MARKUP>`:** Specifies the name of the block (`#+begin/end_<markup>`) with which to wrap
    the results.

**XXX How to unset it (when set in a higher level)?**

## Remarks

The `:wrap` header argument gives you control over the formatting of results
from code blocks.

## Examples

### Using `:wrap` with no value

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

### Using `:wrap` with a string value

The following examples puts the **output in an `example` block**.

    Some results wrapped in an example block.

### Using `:wrap` to produce a source code block in a named language

The following examples puts the results into a `SRC` code block (associated to
the `c++` mode).

On LaTeX export, the `SRC` block will be exported as `c++` code under `listings`
instead of being wrapped in a `\begin{verbatim}..\end{verbatim}` environment.

Alternatively, you can use the :post header argument to wrap the results in a
source block.

## See also

:results (See section 22)
