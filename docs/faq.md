# How can I post-process the generated file?

-   **`org-babel-post-tangle-hook`:** When the hook is executed, the current buffer (as identified by the
    `current-buffer` function) will be a buffer visiting the **file** of the **tangled
    code**.
    
    Example applications could include post-processing, compilation or
    evaluation of tangled code files.

# Why does my Shell code chunk produce no output?

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
