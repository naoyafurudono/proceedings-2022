\section{Related Work}

\paragraph{Effect Semantics.}

\emph{Monads}, originally introduced by Moggi~\cite{DBLP:journals/iandc/Moggi91}
have long been the dominant approach to modelling programs with side
effects. They are, however, famously hard to compose, leading to the development
of \emph{monad transformers}~\cite{DBLP:conf/popl/LiangHJ95} as a technique for
building monads from individual definitions of effects. \emph{Algebraic
  effects}~\cite{DBLP:journals/acs/PlotkinP03} provide a more structured
approach towards this goal, where an effect is specified in terms of the
\emph{operations} that we can use to interact with it. The behaviour of these
operations is governed by a set of equations that specify its
well-behavedness. Later, Plotkin and Pretnar~\cite{DBLP:conf/esop/PlotkinP09}
extended the approach with \emph{handlers}, which define interpretations of
effectful operations by defining a homomorphism from a \emph{free model} that
trivially inhabits the equational theory (i.e., syntax) to a programmer-defined
domain, making the approach attractive for implementing effects as well. Perhaps
the most well-known implementation of algebraic effects and handlers is the
\emph{free monad}~\cite{DBLP:conf/icfp/KammarLO13}, and this implementation is
often taken as the semantic foundation of languages with support for effect
handlers. Schrijvers et al.~\cite{DBLP:conf/haskell/SchrijversPWJ19} showed that
algebraic effects implemented using the free monad correspond to a sub-class of
monad-transformers. The algebraic effects and handlers approach provides a solid
formal framework for understanding effectful programs in which we intend to
ground \strachey{}' semantics of effects and handlers.

A crucial difference between \strachey{}' effects and handlers, and the original
formulation by Plotkin and Pretnar~\cite{DBLP:conf/esop/PlotkinP09}, is the
support for \emph{higher-order operations}. Although it is possible to implement
such operations in algebraic effects by defining them as handlers, this breaches
the separation between the syntax and semantics of effects that underpins
\strachey{}' design.  \emph{Scoped Effects}~\cite{DBLP:conf/haskell/WuSH14} were
proposed as an alternative flavor of algebraic effects that supports
higher-order syntax, recovering a separation between the syntax semantics of
effects for higher-order operations. In subsequent work, Pir\'og et
al. \cite{DBLP:conf/lics/PirogSWJ18} adapted the categorical formulation of
algebraic effects to give Scoped Effects a similar formal
underpinning. Unfortunately, Scoped Effects is not suitable out-of-the-box as a
model for effects and handlers in \strachey{}, because it cannot readily capture
operations that arbitrarily postpone the execution of their higher-order
arguments, such as |lam|. \emph{Latent effects} were developed by Van den Berg
et al. \cite{DBLP:conf/aplas/BergSPW21} as a refinement of scoped effects that
solves this issue. Key to their approach is a \emph{latent effect functor},
which explicitly tracks semantic residue of previously-installed handlers,
allowing for a more fine-grained specification of the types of the computational
arguments of operations. With Latent Effects, it is possible to capture function
abstraction as a higher-order operation. It remains future work to formulate a
precise model of effectful computation for \strachey{}, and to establish if and
how \strachey{}' effect handlers correspond to Latent Effects.

\paragraph{Implementations of Algebraic Effects and Handlers.}

There are many languages with support for algebraic effects and
handlers. Perhaps the most mature is Koka~\cite{DBLP:conf/popl/Leijen17}, which
features a Hindley/Milner-style row-polymorphic type system. While we borrow
from Frank~\cite{DBLP:journals/jfp/ConventLMM20} a
CBPV-inspired~\cite{DBLP:books/sp/Levy2004} distinction between computations and
values, Koka is purely call-by-value, and only functions can be
effectful. Frank~\cite{DBLP:journals/jfp/ConventLMM20}, on the other hand, does
maintain this distinction between values and computations. Its type system
relies on an \emph{ambient ability} and implicit row polymorphism to approximate
effects. Handlers are not first-class constructs in Frank. Instead, functions
may adjust the ambient ability of their arguments by specifying the behaviour of
operations. This provides some additional flexibility over built-in handers, for
example by permitting \emph{multihandlers} that handle multiple effects at
once. Both Koka and Frank lack native support for higher order effects, thus
higher-order operations must be encoded in terms of handlers. This means that it
is not possible to define higher order operations while maintaining the
aforementioned distinction between the syntax and semantics of effects.

Eff~\cite{DBLP:journals/jlp/BauerP15} is a functional language with support for
algebraic effects and handlers, with the possibility to dynamically generate new
operations and effects. In later work, Bauer and
Pretnar~\cite{DBLP:journals/corr/BauerP13} developed a type-and-effect system
for Eff, together with an inference
algorithm~\cite{DBLP:journals/corr/Pretnar13}. The language
Links~\cite{DBLP:conf/tldi/LindleyC12} employs row-typed algebraic effects in
the context of database programming. Their system is based on System F extended
with effect rows and row polymorphism, and limits effectful computations to
functions similar to Koka. Importantly, their system tracks effects using
R\'emy-style rows~\cite{DBLP:conf/popl/Remy89}, maintaining so-called
\emph{presence types} that can additionally express an effect's absence from a
computation. Brachth{\"{a}}user et
al.~\cite{DBLP:journals/pacmpl/BrachthauserSO20} presented \emph{Effekt} as a
more practical implementation of effects and handlers, using \emph{capability
  based} type system where effect types express a set of capabilities that a
computation requires from its context.

\paragraph{Semantics of Composable Data Types and Functions.}

We give a semantics to extensible data types and functions in \strachey{} using
the initial algebra semantics~\cite{DBLP:conf/tlca/JohannG07} of an underlying
signature functor. \emph{Data Types \`a la Carte}
(DTC)~\cite{DBLP:journals/jfp/Swierstra08} solves the expression problem in
Haskell by embedding this semantics into the host language. In later work,
Bahr~\cite{DBLP:conf/icfp/Bahr14} and Bahr and
Hvitved~\cite{DBLP:conf/icfp/BahrH11,DBLP:journals/corr/abs-1202-2917} extended
the approach to improve its expressiveness and flexibility.

DTC, like any approach that relies on initial algebra semantics, limits the
modular definition of functions to functions that correspond to a \emph{fold}
over the input data. While this may seem restrictive, in practice more
complicated traversals can often be encoded as a fold, such as
paramorphisms~\cite{DBLP:conf/fpca/MeijerFP91} or some classes of attribute
grammars~\cite{DBLP:conf/fpca/Johnsson87}. While \strachey{} currently only has
syntax for plain algebras and folds, we plan to extend the syntax for working
with extensible data types and functions to accomodate a wider range of
traversals in the future.

Carette et al. \cite{DBLP:journals/jfp/CaretteKS09} showed how to define
interpreters by encoding object language expressions as a term that represents
their traversal. These traversals correspond to a fold, but abstract over the
algebra that defines the computation, meaning that alternative semantics can be
assigned to terms by picking a suitable algebra. Semantics are defined as type
class instances in the host language, thus one can build object language terms
in a modular way by defining multiple different type classes that correspond to
different syntactical constructs.

\paragraph{Row Types.}

While a concrete design for \strachey{}' type system is still emerging, we
anticipate that it will make heavy use of \emph{row types}, both for tracking
effects and typing extensible types and functions. While to the best of our
knowledge no type system exists with this combination of features, all the
ingredients are there in the literature. Originally, row types were incepted as
a means to model inheritance in object-oriented
languages~\cite{DBLP:conf/lics/Wand89,DBLP:conf/popl/Remy89}, and later
extensible records~\cite{DBLP:conf/icfp/BlumeAC06,gaster1996polymorphic}. More
recently, they also gained popularity in the form of row-based effect systems
with the development of languages such as Koka~\cite{DBLP:conf/popl/Leijen17}
and Links~\cite{DBLP:conf/tldi/LindleyC12}. Their use for typing extensible
algebraic data types and pattern matching functions is less well-studied. For
the most part, row types in this context exist implicitly as part of encoding
techniques such as DTC~\cite{DBLP:journals/jfp/Swierstra08}, where we can view
the use of signature functors and functor co-products as an embedding of
row-typed extensible variants in the host language's type system. Various
refinements of
DTC~\cite{DBLP:conf/haskell/Morris15,DBLP:conf/haskell/OliveiraMY15,DBLP:conf/icfp/Bahr14}
make this connection more explicit by using type-level lists to track the
composition of extensible data. A notable exception is the
\textsc{Rose}~\cite{DBLP:journals/pacmpl/MorrisM19} language, which has a
row-based type system with built-in support for extensible variants and pattern
matching function.
