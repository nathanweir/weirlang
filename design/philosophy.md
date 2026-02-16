# Design Philosophy

Core principles that guide all design decisions for Weir.

## The name

A **weir** is a low dam built across a river to control water flow. It doesn't block the river — it guides it. Water still flows freely, but the weir shapes where it goes, prevents flooding, and creates predictable behavior downstream.

This is the language's design philosophy in physical form:

- **Controls flow without stopping it** — guard rails that prevent bugs without preventing expressiveness
- **An engineering structure, not a wall** — practical, purposeful constraints that make the system more useful, not more restrictive
- **Predictable downstream behavior** — static types, exhaustive matching, and clear error handling mean you know what to expect
- **The water still flows** — Lisp's expressive power, live reloading, and fast iteration are preserved

File extension: `.weir`

## Guard rails are a feature

The language should actively prevent the developer from inadvertently causing bugs through trivial misuse. C++-style swiss-army-chainsaw footguns are to be avoided. If the design evolves toward being more hand-holdy than typical, that's acceptable — within reason.

This principle informs decisions across the language:

- **Hygienic macros** over unhygienic — prevent accidental variable capture
- **Static types** over dynamic — catch errors before runtime
- **Immutable by default** — prevent accidental mutation and aliasing bugs
- **Exhaustive pattern matching** — compiler errors on unhandled cases
- **Arena escape prevention at compile time** — no dangling references to freed memory
- **Safe defaults with explicit opt-out** over unsafe defaults with opt-in
- **Live reloading communicates clearly** rather than silently doing the wrong thing — the system never runs stale data without telling the developer

## Optimize for the developer's inner loop

The primary use case is game development with live iteration. Design decisions should optimize for the experience of: write code → see it running → adjust → see the adjustment. Compilation speed in dev mode, clear error messages, and immediate feedback matter more than squeezing out the last 5% of release-build performance.

## A more expressive type system reduces pressure on the macro system

Invest in type system expressiveness (generics, typeclasses, associated types) to reduce the need for complex macros. A simpler macro system that's purely syntactic becomes more viable when the type system can carry more weight. This is preferable to a powerful but complex macro system that's hard to reason about.
