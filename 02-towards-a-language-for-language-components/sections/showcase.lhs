\section{\strachey{} by Example}
\label{sec:showcase}

In this section, we give an example-driven introduction to \strachey{}'s
features. 

\subsection{Data Types and Functions}

\strachey{} is a functional programming language, and comes equipped with
algebraic data types and pattern matching functions. We declare a new inductive
data type for natural numbers as follows:

\begin{code}
data Nat = Zero | Suc Nat 
\end{code}

\noindent
We can write functions over inductive data types by pattern matching, using a
``pipe'' ($\mid$) symbol to separate clauses: 

\begin{code}
fun double : Nat -> Nat where
| Zero     ↦  Zero
| (Suc n)  ↦  Suc (Suc (double n))  
\end{code}

\noindent
Not all types are user-declared: \strachey{} also offers built-in types and
syntax for integers, lists, tuples, and strings.

\begin{code}
fun length : List a -> Int where
|  []         ↦ 0
|  (_ :: xs)  ↦ 1 + length xs

fun zip : List a -> List b -> List (a * b) where
|  []         _          ↦ []
|  (x :: xs)  (y :: ys)  ↦ (x,y) :: zip xs ys
\end{code}

\noindent
Both |length| and |zip| are polymorphic in the type of elements stored in the
input list(s). Functions implicitly generalize over any free variables in their
type signature.

\subsection{Effects and Handlers}
\label{sec:effects-and-handlers}

\strachey{} supports effectful programs by means of effects and handlers in the
spirit of Plotkin and Pretnar~\cite{DBLP:conf/esop/PlotkinP09}, adapted to
support higher-order operations. The key idea of the effects-and-handlers
approach is to declare the syntax of effectful \emph{operations}, and assign a
semantics to these operations in a separate \emph{handler}. Programs compute
values and have side-effects, and operations act as the interface through which
these side effects are triggered.

We declare a new effect |Fail| with a single operation |fail| in \strachey{} as
follows: 

\begin{code}
effect Fail where
  fail : { [Fail] a }
\end{code}

\noindent
Effects in \strachey{} are declared with the |effect| keyword, and we declare
its operations by giving a list of GADT-style signatures. In this case, the
|fail| operation is declared to have type |{ [Fail] a }|. We enclose the type of
|fail| in braces ($\{-\}$) to indicate that the name |fail| refers to a
\emph{suspended computation} (\cref{sec:evaluation-order}). Suspended
computations are annotated with an \emph{effect row}, enclosed in square
brackets ($[-]$), denoting the side effects of running the computation. Invoking
the |fail| operation has |Fail| as a side effect.

We can use the |Fail| effect to implement a safe division function that invokes
|fail| if the divisor is zero.

\begin{code}
fun safeDiv : Int -> Int -> [Fail] Int where
|  x 0 ↦ fail!!!
|  x y ↦ ... 
\end{code}

\noindent
The postfix exclamation mark to |fail| is necessary to force the suspended
computation. Here, we want to refer to the ``action'' of failing, rather than
the computation itself, following Frank's~\cite{DBLP:journals/jfp/ConventLMM20}
separation between ``being and doing''. We elaborate on this distinction in
\cref{sec:evaluation-order}.

A function's type signature must explicitly indicate its side-effects. In this
case, we annotate the return type of |safeDiv| with the |Fail| effect to
indicate that its implementation uses operations of this effect. Removing the
annotation would make the above invocation of |fail| ill-typed. For functions
that have no side-effects, we may omit its row annotation: |a -> b| is
synonymous to |a -> [] b|

Handlers discharge an effect from annotations by assigning a semantics to its
operations. For the |Fail| effect, we can do this by encoding exceptions in the
|Maybe| type.

\begin{code}
data Maybe a = Just a | Nothing 

handler hFail : { [Fail|||e] a } -> { [e] (Maybe a) } where
|  fail    k  ↦ { Nothing }
|  return  x  ↦ { Just x } 
\end{code}

\noindent
The handler |hFail| takes a value annotated with the |Fail| effect, and produces
a |Maybe| value annotated with the remaining effects |e|. All free effect row
variables in a signature, like |e|, are implicitly generalized over. When
defining a handler we must provide a clause for each operation of the handled
effect. Additionally, we must write a |return| clause that lifts pure values
into the domain that encodes the effect's semantics. Operation clauses have a
\emph{continuation parameter}, |k|, with type |b -> [e] (Maybe a)|, which captures
the remainder of the program starting from the current operation. Handlers may
use the continuation parameter to decide how execution should resume after the
current operation is handled. For example, when handing the |fail| operation we
terminate execution of the program by ignoring this continuation.

We use the continuation parameter in a different way when defining a handler for
a |State| effect, where |s : Set| is a parameter of the module in which we
define the effect. 

\begin{code}
effect State where
|  get  :       [State] s
|  put  : s ->  [State] ()

handler hState : { [State|||e] a } -> s -> { [e] (a * s) } where 
| get        st  k  ↦ k st  st
| (put st')  st  k  ↦ k ()  st'
| return  x  st     ↦ { (x , st) }
\end{code}

\noindent
For both the |get| and |put| operations, we use the continuation parameter |k|
to implement the corresponding branch in |hState|. The continuation expects a
value whose type corresponds to the return type of the current operation, and
produces a computation with the same type as the return type of the handler. For
the |put| operation, for example, this means that |k| is of type |() -> s -> {
  [e] (a * s) }|. The implementation of |hState| for |get| and |put| then simply
invokes |k|, using the current state as both the value and input state (|get|),
or giving a unit value and using the given state |st'| as the input state
(|put|). Effectively, this means that after handling |get| or |put|, execution
of the program resumes, respectively with the same state or an updated state
|st'|.

Handlers in \strachey{} are so-called \emph{deep handlers}, meaning that they
are automatically distributed over continuations. For the example above, this
means that that the |State| effect is already handled in the computation
returned by |k|. The alternative is \emph{shallow handlers}, in which case |k|
would return a computation of type |{ [State|$\mid$|e] a }|. When using shallow
handlers, the programmer is responsible for recursively applying the handler to
the result of continuations. While shallow handlers are more expressive, unlike
deep handlers they are not guaranteed to correspond to a fold over the effect
tree, meaning that they are potentially harder to reason about.

\subsection{Order of Evaluation, Suspension, and Enactment}
\label{sec:evaluation-order}

Inspired by Frank~\cite{DBLP:journals/jfp/ConventLMM20}, \strachey{} allows
effectful computations to be used as if they were pure values, without having to
sequence them. Sub-expressions in \strachey{} are evaluated from left to right,
and the side-effects of computational sub-expressions are evaluated eagerly in
that order. For example, consider the following program:

\begin{code}
fun f : Int -> [Fail] Int where
|  n ↦ fail!!! + n 
\end{code}

\noindent
Here, we use the expression |fail!!!| (whose type is instantiated to |[Fail]
Int|) as the first argument to |+|, where a value of type |Int| is
expected. This is fine, because side-effects that occur during evaluation of
sub-terms are discharged to the surrounding context. That is, the side-effects
of evaluating computational sub-terms in the definition of |f| become
side-effects of |f| itself.

In practice, this means that function application in \strachey{} is not unlike
programming in an \emph{applicative style} in Haskell. For instance, when using
the previously-defined handler |hFail|, which maps the |Fail| effect to a
|Maybe|, we can  informally understand the semantics of the \strachey{} program above as
equivalent to the following Haskell program: 

\begin{code}
f :: Int -> Maybe Int
f n = (+) <$> Nothing <*> pure n 
\end{code}

\noindent
Equivalently, we could write the following \emph{monadic} program in Haskell,
which makes the evaluation order explicit.

\begin{code}
f :: Int -> Maybe Int
f n = do  x <- Nothing
          y <- pure n
          return (x + y) 
\end{code}

\strachey{}'s eager treatment of side-effects means that effectful computations
are not first-class values, in the sense that we cannot refer to an effectful
computation without triggering its side effects. To treat computations as
first-class, we must explicitly \emph{suspend} their effects using
\emph{braces}:

\begin{code}
fun f' : Int -> { [Fail] Int } where
|  n ↦ { f n } 
\end{code}

\noindent
The function |f'| is no longer a function on |Int| that may fail, but instead a
function from |Int| to a computation that returns and |Int|, but that could also
fail. We indicate a suspended computation in types using braces (\{/\}), and
construct a suspension at the term level using the same notation. 

To \emph{enact} the side-effects of a suspended computation, we postfix it with
an exclamation mark ($!$). For example, the expression |(f' 0)!!!| has type
|[Fail] Int|, whereas the expression |(f' 0)| has type |{ [Fail] Int }|. We see
the same distinction with operations declared using the |effect| keyword. When
we write |fail|, we refer to the operation in a descriptive sense, and we can
treat it like any other value without having to worry about its
side-effects. When writing |fail!!!|, on the other hand, we are really
performing the action of abruptly terminating: |fail| \emph{is} and |fail!!!|
\emph{does}.


\subsection{Modules and Imports}

\strachey{} programs are organized using modules. Modules are delimited
using the |module| and |end| keywords, and their definitions can be brought into
scope elsewhere using the |import| keyword. All declarations---i.e., data types,
functions, effects, and handlers---must occur inside a module.

\begin{code}
module A where
  fun f : Int -> Int where
  | n ↦ n + n
end

module B where
  import A
  fun g : Int -> Int where
  | n ↦ f n 
end 
\end{code}

\noindent
In addition to being an organizational tool, modules play a key role in defining
and composing modular data types and functions.


\subsection{Composable Data Types and Functions}
\label{sec:composable-types-and-functions}

In addition to plain algebraic data types and pattern matching functions,
declared using the |data| and |fun| keywords, \strachey{} also supports
case-by-case definitions of \emph{extensible} data types and functions. In
effect, \strachey{} provides a convenient surface syntax for working with
DTC-style~\cite{DBLP:journals/jfp/Swierstra08} definitions, which relies on an
embedding of the initial algebra semantics~\cite{DBLP:conf/tlca/JohannG07} of
data types to give a semantics to extensible and composable algebraic data types
and functions, meaning that extensible functions have to correspond to a
\emph{fold}~\cite{DBLP:conf/fpca/MeijerFP91}. In \strachey{}, one can program
with extensible data types and functions in the same familiar way as with their
plain, non-extensible counterparts.

The module system plays an essential role in the definition of composable data
types and functions. That is, modules can inhabit a \emph{signature} that
declares the extensible types and functions for which that module can give a
partial definition. In a signature declaration, we use the keyword |sort| to
declare an extensible data type, and the |alg| keyword to declare an
extensible function, or \emph{algebra}. By requiring extensible functions to be
defined as algebras over the functor semantics of extensible data types, we
enforce \emph{by construction} that they correspond to a fold. 

As an example, consider the following signature that declares an extensible data
type |Expr|, which can be evaluated to an integer using |eval|.

\begin{code}
signature Eval where
  sort  Expr : Set
  alg   eval : Expr -> Int 
end 
\end{code}

\noindent
To give cases for |Expr| and |eval|, we define modules that inhabit the |Eval|
signature. 

\begin{code}
module Lit : Eval where
  cons  Lit : Int -> Expr
  case  eval (Lit x) ↦ x 
end

module Add : Eval where
  cons  Add : Expr -> Expr -> Expr
  case  eval (Add x y) ↦ x + y 
end 
\end{code}

\noindent
The |cons| keyword declares a new constructor for an extensible data type, where
we declare any arguments by giving a GADT-style type signature. We declare
clauses for functions that match on an extensible type using the |case|
keyword. For every newly declared constructor of an extensible data type, we
have an obligation to supply exactly one corresponding clause \emph{for every
  extensible function that matches on that type}. \strachey{} has a coverage
checker that checks whether modules indeed contain all necessary definitions, in
order to rule out partiality resulting from missing patterns. For example,
omitting the |eval| case from either the module |Lit| or |Add| above will result
in a static error.  Coverage is checked locally in modules, and preserved when
composing signature instances.

In the definition of |eval| in the module |Add|, we see the implications of
defining function clauses as algebras. We do not have direct control over
recursive calls to |eval|. Instead, in |case| declarations, any recursive
arguments to the matched constructor are replaced with the result of recursively
invoking |eval| on them. In this case, this implies that |x| and |y| do not
refer to expressions. Rather, if we invoke |eval| on the expression |Add e1 e2|,
in the corresponding |case| declaration, |x| and |y| are bound to |eval e1| and
|eval e2| respectively. We could encode the same example in Haskell as follows,
but to use |eval| on concrete expressions additionally requires explicit
definitions of a type level fixpoint and fold operation. In \strachey{}, this
encoding layer is hidden by the language.

\begin{code}
data Add e = Add e e
eval :: Add Int -> Int
eval (Add x y) = x + y 
\end{code}

To compose signature instances we merely have to import them from the same
location.

\begin{code}
module Program where
  import Lit , Add

  -- Evaluates to 3 
  fun test : Int = eval (Add (Lit 1) (Lit 2))
end
\end{code}

\noindent
By importing both the |Lit| and |Add| modules, the names |Expr| and |eval| will
refer to the composition of the constructors/clauses defined in the imported
signature instances. Here, this means that we can construct and evaluate
expressions that consist of both literals and addition. Furthermore, to add a
new constructor into the mix, we can simply define a new module that
instantiates the |Eval| signature, and add it to the import list.

To define an alternative interpretation for |Expr|, we declare a new
signature. In order to reference the |sort| declaration for |Expr|, we must
import the |Eval| signature.

\begin{code}
signature Pretty where
  import Eval -- brings 'Expr' into scope

  alg pretty : Expr -> String
end
\end{code}

\noindent
We declare cases for |pretty| by instantiating the newly defined signature,
adding |import| declarations to bring relevant |cons| declaration into scope.

\begin{code}
module PrettyAdd : Pretty where
  import Add -- brings 'Add' into scope

  case pretty (Add s1 s2) = s1 ++ " + " ++ s2 
end
\end{code}

\noindent
It is possible for two modules to be \emph{conflicting}, in the sense that they
both define an algebra case for the same constructor. This would happen, for
example, if we were to define another module |PrettyAdd2| that also implements
|pretty| for the constructor |Add|. Importing two conflicting modules should
result in a type error, since the semantics of their composition is unclear. 