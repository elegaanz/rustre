#import "template.typ": *

#show: project.with(
  title: "Project report: Rustre",
  authors: (
    "Ana Gelez",
    "Mathis Grange",
    "Lucas Mathieu",
    "Edgar Onghena",
  ),
  date: [March 28#super("th"), 2023],
)

= Introduction
Lustre is a synchronous and declarative programming language. A Lustre program is a set of equations linking each variable of the program to an expression. These equations are not "executed" in a particular order as in an imperative programming language. A Lustre program simulator only ensures that these equations are verified at any time during execution.

Lustre has given birth to a suite of tools used in industry. But the language is also used in educational contexts, to simulate electronic circuits.
This is notably the case at Polytech Grenoble in the Computer Science department.

There is an official implementation of the Lustre compiler as well as complementary tools allowing to execute and test programs. This is the implementation that is used at Polytech Grenoble. However, these tools create a lot of friction with the user and make working with Lustre programs complex.

The goal of the Rustre project is to implement a set of development tools for Lustre offering a better experience than the official implementation. The first step is to create a library that allows the analysis and manipulation of a Lustre program. This library can then be used to create various tools: compiler, code formatter, _language server_, simulator, etc.

= Critique of the official implementation

In order to understand the purpose of the project, we think it is interesting to explain what we criticize about the official implementation (called "LV6", for "Lustre Version 6" in the following).

== Stop at the first error

The LV6 compiler has the defect of stopping at the first error encountered. It seems to us preferable to report a maximum of errors before stopping for three reasons.

Firstly, if we display several errors, we can correct several at once. Otherwise, we have to go back and forth between the compiler and the code editor, which slows down the user.

Secondly, in most cases, an error in one place does not prevent the analysis of the rest of the program from continuing. Let's imagine a program where a constant is incorrectly typed, but not used by the node we want to compile. It is worth reporting the error, but the compilation can still succeed.

Finally, in the context of building a _language server_, the program should not quit at the first error in the source code, since while we are modifying the code we will inevitably go through states with errors (if only when we haven't finished writing the name of a variable in an expression for example). Moreover, having an overview of all the problems to be solved directly in your code editor is a very useful tool.

== Unclear error messages

In addition to reporting only one error at a time, the message displayed by LV6 is generally not very explicit (see @err).

#figure(
  image("lv6-err.png", width: 80%),
  caption: [An LV6 error, indicating that a semicolon is missing at the end of an equation]
) <err>

Here, we understand that there is a syntax error on line 4, but it's hard to know what exactly to correct. So we waste time trying to figure out what the concern is.

== Multitude of different tools

LV6 offers a collection of different tools, each with a very specific use. However, the relationship between these tools (which tool can provide an input for which other) is not always very clear. It is also difficult to list all the available tools to know if there is one for a particular use case, because they have names that do not follow a particular logic (no common prefix that would allow autocompletion in the terminal, for example).

== Little documentation

Finally, the Lustre language and the tools proposed by LV6 are poorly documented. In addition to a reference manual and a tutorial that we could expect, we also notice a lack of contextual documentation. It would have been possible, for example, to explain directly in the compiler output why an error occurs, possibly redirecting to a tutorial or a more detailed documentation page.

= Specifications for a better implementation

Starting from this observation, we have established a specification for Rustre, our reimplementation. Our first goal was to build a classical compiler taking as input Lustre source code and generating a binary simulating the described system (or even more simply, directly simulating the syntax tree built after analysis phases, without binary output).

== A partial syntax tree

In order to support several errors and to be able to continue the analysis of the source code even if part of the syntax is invalid, we needed a syntax tree that could contain "error" nodes.

For example, the following code:

```lustre
function id(x : int) returns (y : int);
let
  x = -- oops
such;
```

Gives a tree similar to this one:

```haskell
FunctionNode "function id ... tel;"
  Ident "id"
  Parameters "(x : int)"
    VariableDecl "x : int"
      Ident "x"
      Type "int"
  Returns "returns (y : int)"
    VariableDecl "y : int"
      Ident "y"
      Type "int"
  Body "let ... tel;"
    Error "x = "
    Comment "-- oops"
```

We can see that we have an "Error" node that contains only the tokens that we were unable to parse, but that the rest of the tree is correctly constructed.

With such a tree, we can still define some analyses, which is very useful in the case of a language server. For example, one could calculate the signature of the `id` function even if the body of the function contains an error.

The main difficulty is to know where to resume parsing after having encountered an error. In the example above we considered for example that we could resume at the moment we encountered a comment.

== Keeping the trivia in the syntax tree

In addition to containing potential errors, the AST must be without loss of information. It must be possible to reconstruct the source file identically just from the tree. Therefore, comments, but also spaces, tabs and line breaks must be kept.

We made this choice with the perspective of building a code formatter, but this information can also be used in the case of a language server (to refactor code while preserving its "formatting") or if we want to exploit the comments to generate code documentation pages.

== Incremental analysis for a language server

Finally, the architecture of our project must allow us to redo the same analysis as often as possible. This is an advantage in a classical compiler, but even more so in a language server, where you don't want to redo all the analyses every time you press a key. We also wanted it to be possible to enrich AST nodes with the result of an analysis easily (e.g., we didn't want to have to redefine a `TypedAST` type similar to the `AST` type but with extra typing information).

To do this, we decided to use an incremental computation framework, i.e. one that stores the results of certain functions and reuses them as much as possible, while keeping track of the interdependencies between computations in order to correctly invalidate the cache.

Our first choice was `salsa`, but we encountered bugs and some choices that had been made did not suit us. So we decided to create our own _crate_ to fill this need: `yeter`. This library has been published on _crates.io_ and can be used by other projects.

Memoization allows us to avoid unnecessary recomputation of the same results, but it also allows us to enrich the AST dynamically. Rather than redefining an AST with a similar structure but with an extra `x` field for each `n` node, we can define a memoized function `x(n)`.

= Project architecture

Based on these constraints, we divided the project into different modules (independent _crates_).

== rustre-parser: a fault tolerant parser

The purpose of this crate is to generate the AST corresponding to a file. It consists of a lexer based on the `logos` crate, and a parser based on `rowan` and `nom`. The use of these crates saves us a lot of time.

In particular, Rowan makes it easy to build a syntax tree with the specifities we are interested in (it was developed for rust-analyzer). Nom is a parser combinator library. With the help of a `rowan-nom` adapter crate between the two, we were able to build the tree with a clear and concise syntax.

The parser always produces a tree. If it encounters an error, an `Error` node will be inserted, but the rest of the tree will still be here.

The generated AST can be displayed with the `rustre ast` command.

== yéter : an incremental computation framework

Once we had a parser, we had to build the various analysis that needs to be run before the final compilation. To do it in the best way possible, we decided to build an incremental computation framework, called Yéter.

Using Yéter to cache computations is almost transparent. All you have to do is:

+ at the start of your program, create a `yeter::Database` that will act as a giant cache.
+ decorate all _queries_ (functions that can be memoized) with the `#[yeter::query]` attribute, and add a `&yeter::Database` as their first parameter.

Yéter then takes care of computing the result the first time and then putting it in the cache for future use. It also tracks dependencies between queries. This way, if a query changes its output (for instance if you edit your code to change the type of a variable), all dependent queries will be recomputed.

Yéter is published on #link("https://crates.io/crates/yeter")[crates.io], to make it available to other projects too.

== rustre-core : Lustre code analysis library

This crate provides the main analysis functions. Each one is defined as a Yéter query.

The most interesting ones are the following.

=== Name resolution

This query takes an identifier (and optionally a node in which we currently are) and returns the node, constant or type with this name, if any. It can also be used to resolve local bindings such as node variables, inputs or outputs.

=== Constant expression interpreter

We have implemented queries to evaluate an expression node of the AST in a given scope. These functions can therefore evaluate the values of global constants but also local to a given node.

Being able to evaluate constant expression is crucial as the standard array type (`T^N`) can allow relatively complex expressions for its size (the right-hand-side parameter, `N`), should this expression not depend on anything that cannot be determined at compile-time.

=== Type checking

This query checks that all the types in the program are correct. Lustre type system is relatively simple, so the algorithm basically takes all equations and checks that the left and right terms are of the same type. To do that it recursively computes the type of both expressions.

The main difficulty here is to report useful errors, which requires a little bit of special casing. An example of such "smart" error reporting can be found in @tyck.

#figure(
  image("rustre-tyck.png", width: 80%),
  caption: [Errors and warnings reported by our type checker]
) <tyck>

== rustre-cli: a command line interface

The CLI is the user-facing entry-point to Rustre. While the rest of the crates making Rustre are intentionally designed to be usable as independent librairies that could potentially be embedded in foreign software, this crate is an executable program.

The argument interface is specified using the popular Rust library #link("https://crates.io/crates/clap")[`clap`], which can almost automatically generate a #strong[c]ommand-#strong[l]ine #strong[a]rgument #strong[p]arser from a Rust structure with the correct attributes. In addition to the parser, we get a nice help message for free, and automatic errors on syntax errors.

#figure(
  image("rustre-help.png", width: 80%),
  caption: [The default Rustre help message, when no argument is given to the program]
)

As of today, the most interesting subcommand is `rustre check <FILE>`, which applies various checks to a Lustre program given an entry point and reports any diagnostics to the console. A non-zero status code is returned if any error is printed, ignoring warnings, except if `--deny-warnings` is specified.

= Non-technical aspects of the project

Working in team on a project like this one for weeks requires more than coding. We would like to end our report with a note about everything in our project not directly related to its code.

== Documentation

During all the project, and especially at the beginning when we were figuring out the global architecture, we did our best to document our technical choices and the design of our compiler.

The documentation is available #link("https://projets-info4.gricad-pages.univ-grenoble-alpes.fr/22-23/26/docs/html/")[online].

We also generate #link("https://projets-info4.gricad-pages.univ-grenoble-alpes.fr/22-23/26/rustre/rustre_core/")[documentation from our source code].

== Project management

We were using Gitlab to build Rustre. We tried to use it to its full potential, by managing our roadmap with issues and by collaborating with merge requests.

With also decided to setup continuous integration very early in the project to ensure our code quality always met our standards.

Because Edgar and Ana were more experienced with Rust and compiler design, they naturally became the ones who decided how to organize the project. After learning Rust during the first days, Mathis and Lucas were able to contribute as well, and wrote the type checker and constant evaluator, which are the two most important analysis that we built. 

== What's next?

Unfortunately, building a compiler is not an easy task, and it takes time to have a fully working product. But in only three month, we were able to build solid foundations for a complex compiler, which can scale to other development tools as well.

The next task would probably be to build an interpreter (or simulator) that takes a syntax tree and a state, and computes the next step of a given node.

After that, generating what is called "extended code" could be interesting because it would allow us to interface our compiler with many LV6 tools, and test it against the official implementation.

Rustre also lacks support for some more advanced features, like static parameters or clocks. A final product should implement them, and more generally, be fully compliant with the official implementation.

We laid down the base for this work to be as easy as possible, and we started to document what would be necessary for these features to be implemented. Someone familiar with the code base should be able to implement them fairly easily, if given a little bit of time.