\section{Future Work}

\strachey{} is an ongoing research project. Here, we briefly summarize the
current state, and some of the challenges that still remain.

While we can implement the examples from this paper in the current prototype
implementation of \strachey{}, the language still lacks a complete
specification.  As such, the immediate next steps are to develop specifications
of the type system and operational semantics. This requires us to address
several open research questions, such as giving a semantics to the flavor of
higher-order effects used by \strachey{}, and applying row types to type
\strachey{}' extensible data types and functions. While the \textsc{Rose}
language supports extensible variants, this support is limited to non-recursive
types. For \strachey{}, we would need to adapt their type sytem to support
recursive extensible data types as well.  Designing a small core calculus into
which \strachey{} can be translated could be potential way to explore these
questions, making a formalization of the language in a proof assistant more
attainable, by formalizing the core language. Further down the line, we also
intend to explore a denotational model for effect handlers in \strachey{},
giving the language a more solid formal foundation, similar to existing
programming languages based on algebraic effects and handlers.

In the future, we also hope to enforce stronger properties about specifications
defined in \strachey{} through the language's type system. The prime example are
\emph{intrinsically-typed definitional
  interpreters}~\cite{augustsson1999exercise}, which specify a language's
operational semantics such that it is \emph{type sound by construction}.

\section{Conclusion}

Reusable programming language components have the potential to significantly
reduce the amount of time, effort, and expertise needed for developing
programming languages. In this paper, we presented \strachey{}, a functional
meta-language for defining reusable programming language components. \strachey{}
enables the defintion of reusable language components using algebraic data types
and pattern matching functions, by supporting \emph{extensible data types and
  functions}, which are defined on a case-by-case basis. Additionally,
\strachey{} features built-in support for \emph{effects and handlers} for
defining the side effects of a language. The flavor of effects and handlers
implemented by \strachey{} supports \emph{higher-order} operations, and can be
used to define features that affect a program's control flow, such as function
abstraction, as a reusable effect. We illustrated how these features can be used
for developing reusable programming language components by defining a component
for function abstraction, which can be composed with other language components
and evaluated using both a call-by-value and call-by-name strategy.
