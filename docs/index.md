<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#orgheadline3">1. Compendium principles</a>
<ul>
<li><a href="#orgheadline1">1.1. Literate Programming</a></li>
<li><a href="#orgheadline2">1.2. Reproducible Research</a></li>
</ul>
</li>
</ul>
</div>
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
