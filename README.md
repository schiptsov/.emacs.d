

# The universal principles

It is delightful to see all the *fundamental principles* at work in one place. A half of a non-bullshit Computer Science, so to speak.

*Abstraction by parameterization* (lambdas and &ldquo;commands&rdquo;) *Data Abstraction (ADTs), standardized interfaces*, as well as *leaking abstractions* (`plist`, `alist`) and what the actual consequences are.

GNU Emacs is a *very* large and *very* complex system, so *modularity*
plays the central role here. Tens of packages (modules) are being used, so the obvious benefits and the obvious problems (breaking abstraction boundaries, relying on implementations, etc.) can be clearly seen.

At a higher level, the &ldquo;meta&rdquo; modularity of *DSLs* embedded in Emacs Lisp, which form distinct *layers* of related abstractions, is the result of another universal principle bein applied.

Last but not least, the principles, insights and proper generalizations from the oldtimer&rsquo;s classic frameworks - *the early LISPs, /UNIX, Smalltalk, Genera, Plan9*, are beening used.

A lot has been generalized from daily practices of running mundane tasks on a *terminal* (in the 70s and 80s), where `ls`, `find` and `grep` were the main tools (utilities).

Notice that in the old times there were no way to easily copy and paste struff around, so they used *pipes* and */temporary filles*, which later became *editor buffers*. This is actually a *big deal (the birth of proper UIs)*.

The most fundamental principles are still of *pipelining* and of *htransforming* a strucrured text (from *UNIX*), along with having small procedures which do just one thing, but do it well (*just right*). Piping texts into (and from) editor&rsquo;s buffers is the Plan9 way to do thing.

Other principles and eventually discovered &ldquo;big ideas&rdquo; or even *evolved* standard interfaces came from Smalltalk and the Lisp Machine OS - Open Genera.

The *dynamic* nature of the language (*dynamicly typed, late binding*) &ldquo;imply&rdquo; the dynamic nature of the resulting *environment*, where anything can be created and *rebound* at runtime. Just like Smalltalk. Well, almost.

We could even generate some Lisp procedures at runtime according to specific conditions (using macros) and evaluate and call them *in-place*.

Just as with *Common Lisp*, which is an *imperative language* with little or no emphasis on pure functions and immutable data (unlike in *classic MIT Scheme*), one has to pay careful attention to what modifies and rewrites what (which procedures performing &ldquo;destructive&rdquo; updates and when).

This is the great illustration of the fundamental principle of functional programming - in a pure functional language one could *actually* rely on a function&rsquo;s *specification*, without have to look at the implementation, leave alone to understand it.

With the imperative *Emacs Lisp*, however, one has to read and understand the code (implementation) in order to use a procedure correctly. Not just that, but one has to *mentally simulate* (in one&rsquo;s mind) the actual order of execution and trac the mutable state.

Like any other evolved complex (biological) system, GNU Emacs has several distinct *subsystems* with correspondig sets of modules (packages) or even &ldquo;micro frameworks&rdquo; (`ivy`, `company`, `yasnippet`, etc).

These subsystems, at least in theory, have to be properly &ldquo;isolated&rdquo;, &ldquo;orthogonal&rdquo; to each other and &ldquo;layered&rdquo; (thus &ldquo;reusable&rdquo;). The parallels (correspondence) to the universal &ldquo;biological design patterns&rdquo; are not ancidental.

Notice that the universal patterns are at both *micro* (Lists, *homoiconicity* - a hidden *Monoid*) and at the *macro* (layers of &ldquo;functional&rdquo; *embedded DSLs*, which form distinct subsytems) levels. This is the proper (evolved) *architecture of complexity*.

Anyway, coming (at least partially) from the *MIT CSAIL* it has been based on the right priciples and incorporates (realizes and manifests) some &ldquo;big ideas&rdquo; and universal notions.

The most important are:

-   *self-documenting* (people of the classic golden age knew and applied the right principles)
-   everything is *discoverable* (every binding and every command can be discovered and cross-referenced)
-   truly *extensible* (dynamicly typed LISP2 with embedded Common Lisp and macros)
-   *keyboard macros* (it is a *software Lisp Machine*, after all)
-   *Lisp Macros* (the way to define your own *special forms and DSLs*)
-   *declarative embedded DSLs* (what makes Emacs what it is)

So, when one &ldquo;visits&rdquo; GNU emacs one has to experience some near &ldquo;religios ave&rdquo;, as when visiting an ancient &ldquo;spiritual&rdquo; *world heritage site*.

More about this on <https://lngnmn2.github.io/articles/emacs/>

Enough of an &ldquo;abstract theory&rdquo; (it isn&rsquo;t). Here comes the &ldquo;concrete&rdquo; code in which everything &ldquo;universal&rdquo; *manifests itself*.

