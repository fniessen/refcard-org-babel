Org-babel is making use of the `C-c C-v` key binding.  All Org-babel keybindings
are located behind this prefix.

<div class="tip">
Babel commands can be used as **speed commands** when the point is at the beginning
of a code block (specifically, at the beginning of the `#+begin_src` line, in
column `0`).

</div>

# Help

-   **`C-c C-v h`:** **Describe** all **key bindings** behind Org Babel key prefix.

# Edit the code

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

# SHA1 hash

-   **`C-c C-v a` (or `C-c C-v C-a`):** **View SHA1 hash** of the current code block.

You could use the hash returned by that function to manually replace the
original one in the block, and avoid reevaluating the block in case of purely
cosmetic changes.

# Navigate

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

# Execute (or &ldquo;evaluate&rdquo;)

-   **`C-c C-v b` (or `C-c C-v C-b`):** Evaluate all the code blocks **in the current buffer**.

-   **`C-c C-v s` (or `C-c C-v C-s`):** Evaluate all the code blocks **in the current subtree**.

-   **`C-c C-v e` (or `C-c C-v C-e`):** Evaluate the code block **at point**.

With `C-u C-c C-v b/s/e`, **forcibly re-evaluate the block(s)** (even when marked
&ldquo;cached&rdquo;).

# Tangle

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

# View the results

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

# Library of Babel

-   **`C-c C-v i` (or `C-c C-v C-i`):** **Ingest all named code blocks** from supplied file into the Library of Babel
    (in memory).

-   **`C-c C-v l` (or `C-c C-v C-l` or `<M-up>`):** **Load the current code** block into the Library of Babel and enter the
    **session**.
