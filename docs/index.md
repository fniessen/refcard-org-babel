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

In a nutshell, Org Babel is like Sweave (for reproducible research (See section 2)) but it takes
a **large number of possible languages** (C, Java, Python, Ruby, R, &#x2026;) and Org
mode can produce **HTML** as well as **PDF**.

<div class="warning">
Prolonged use may cause addiction!

</div>

# Literate Programming

**Literate programming** (LP) offers 2 classical operations:

-   **Tangle:** Extract the source code blocks and **generate real working code** files for
    further compilation or execution, eventually outside of Emacs.

-   **Weave:** **Export** the whole Org file as literate, **human-readable documentation**
         (generally in HTML or LaTeX).

# Reproducible Research

Above those, Org Babel adds *in situ* code evaluation:

-   during **interactive** use (in the Org buffer itself),
-   during **tangle**, and/or
-   during **weave** (code blocks with `:exports` set to `results` or `both`)

This allows you to insert in your Org document:

-   all **data** (that can reasonably be included),
-   all **code** you used, and
-   the full set of **outputs** you got,

following the principles of **reproducible research** (RR).

# Overview of contents

-   [Code blocks](eval.md)
-   [Header arguments](header-args.md)
-   [Other Header arguments](extra-header-args.md)
-   [FAQ](faq.md)
-   [Key bindings](key-bindings.md)
-   [Colophon](colophon.md)
