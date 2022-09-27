# Memory Usage Profiling With `eventlog2html` and `ghc-debug`

## Abstract

Understanding and analysing the memory usage of Haskell programs is a
notoriously difficult yet important problem. Recent improvements to GHC's
profiling capabilities, along with better tooling, has made it much easier to
deeply and precisely analyze the memory usage characteristics of even large
Haskell programs.

This workshop aims to present two such tools that allow high and low level
memory usage analysis of Haskell programs: `eventlog2html` and `ghc-debug`. We
will learn how to set up and use `eventlog2html` to generate high-level visuals
and statistics of our program's execution. We will also learn how to set up and
use `ghc-debug` to precisely and programmatically explore our program's
low-level memory usage profile.

We will examine these tools by using them on several pre-prepared Haskell
programs. The workshop aims to be beneficial to Haskell programmers of all
levels. Beginner Haskell programmers can expect to gain a deeper understanding
of lazy evaluation and the impacts it can have on program performance.
Experienced Haskell programmers can expect to gain an understanding of exactly
what these tools have to offer and the skills necessary to use these tools on
their own Haskell programs.

## Workshop Outline

### Goal

The primary goal of this workshop is for participants to gain experience and
familiarity with the `eventlog2html` and `ghc-debug` memory profiling tools.

### Prerequisites: Lazy Evaluation and Normal Forms

A crucial step in the pursuit of understanding the memory usage of Haskell
programs is understanding Haskell's semantics as a lazy programming language.
While thorough coverage of such semantics is outside the scope of this workshop,
I do hope that much of what we cover will be approachable and enlightening to
Haskell beginners and experts alike.

There are plenty of fantastic resources on these topics out there, one of them
being this excellent interactive
[blog post by Well-Typed](https://well-typed.com/blog/2017/09/visualize-cbn/).

### A First Look at `ghc-debug`

The `ghc-debug` style of debugging is, like Haskell, somewhat unique. In this
style, we have a *debuggee* and a *debugger*. The debuggee is the application
whose heap profile we would like to analyze. The debugger is the application
which will actually execute the analysis.

Communication between the debuggee and debugger happens over a socket, where the
debuggee simply responds to requests sent by the debugger. Crucially, it is
incredibly simple to turn a Haskell application into a debuggee for analysis
using `ghc-debug` debuggers, as we will see later.

With the above in mind, we can introduce `ghc-debug` as a set of libraries and
tools:

- [`ghc-debug-stub`](https://hackage.haskell.org/package/ghc-debug-stub): A
  library containing the functions you should include in your program to
  perform analysis with `ghc-debug` debuggers.
- [`ghc-debug-client](https://hackage.haskell.org/package/ghc-debug-client): A
  library containing useful functions for writing your own heap analysis
  scripts.
- [`ghc-debug-brick`](https://hackage.haskell.org/package/ghc-debug-brick): An
  executable terminal user interface application that can connect to any
  debuggee.

These aren't all of the libraries involved, but they are the big three that we
care about as users of `ghc-debug`.

To get started in the workshop, we will be examining the example `heap-shapes`
application as a debuggee using `ghc-debug-brick` as our debugger. This will
serve as an introduction to the `ghc-debug` style of debugging, and it will
cover some examples of evaluation scenarios that will be important later in the
workshop.

### The Haskell Is Obviously Better at Everything (HIOBE) Index

The HIOBE Index server (in `hiobe-index/server`) is the application
we would like to profile with the `eventlog2html` and `ghc-debug` tools. It is a
simple [scotty](https://hackage.haskell.org/package/scotty) web server
application that serves data from a sqlite database on various endpoints. We
will generate fake traffic for the application which will cause interesting
objects to build up on the heap.

For a full description of the HIOBE Index, see
[its README](./hiobe-index/README.md).

We will spend the rest of the workshop analysing, understanding, and tuning the
memory profile of the HIOBE Index server.
