
\section{Introduction}
\label{sec:introduction}

Developing programming languages is a difficult and time consuming task, which
requires a lot of expertise from the language designer. One way to reduce the
cost of language development is to build languages from \emph{reusable
  programming components}, allowing language designers to grab off-the-shelf
components for common language features. This approach has the potential to make
language development both cheaper and more accesbile, while producing
specifications that allow us to understand the semantics of language features
independent from the language they are a part of. Modern functional programming
languages, however, lack support for reuse of definitions, and as a result,
language components built from algebraic data types and pattern matching
functions cannot be reused without modifying or copying existing code.  To
illustrate the kind of language components we would like to define modularly,
and where current functional programming languages fall short for this purpose,
we consider the implementation of a tiny expression language and its interpreter
in Haskell:

\newpage

\begin{code}
data Expr = Lit Int | Div Expr Expr 

eval :: MonadFail m => Expr -> m Int
eval (Lit x)      =  return x
eval (Div e1 e2)  =  do
  v1 <- eval e1
  v2 <- eval e2
  if v2 /= 0 then return (v1 `div` v2 else fail 
\end{code}

\noindent
The |Expr| data type declares an abstract syntax type with constructors for
literals and division, and the function |eval| implements an interpreter for
|Expr|. Importantly, |eval| needs to account for possible divisions by zero:
evaluating |Div (Lit 10) (Lit 0)|, for example, should safely evaluate to a
result that indicates a failure, without crashing. For this reason |eval| does
not produce an |Int| directly, but rather wraps its result in an abstract monad
|m| that encapsulates the side effects of interpretation. In this case, we only
assume that |m| is a member of the |MonadFail| typeclass. The |MonadFail| class
hase one function, |mfail|:

\begin{code}
class MonadFail m where
  mfail :: m a 
\end{code}

\noindent
We refer to functions, such as |mfail|, that allow us to interact with an
abstract monad as \emph{operations}. We choose to factor language definitions
this way, because it allows us to both define a completely new interpretation
such as pretty printing or compilation for |Expr| by writing new functions
|pretty :: Expr -> String| or |compile :: Expr -> m [Instr]|, while also having
the option to change the implementation of existing semantics, by supplying
alternative implementations for the |fail| operation. We can summarize this
approach to defining language components with the following pipeline:

\begin{center}
$\text{Syntax}\ \xrightarrow{\,\mathrm{denotation}\,} \text{Operations} \xrightarrow{\, \mathrm{implementation}\,} \text{Result}$
\end{center}

\noindent
That is, a \emph{denotation} maps \emph{syntax} to an appropriate domain. In the
definition of this domain, we distinguish between the type of the resulting
value, and the side effects of computing this result, which are encapsulated in
an abstract monad. We interact with this abstract monad using \emph{operations},
and thus to extract a result we must supply a monad that \emph{implements} all
required operations.

%% The reason for factoring definitions in this way is to make defining
%% new interpretations of existing abstract syntax as convenient as possible. In
%% some cases it is easiest to define a whole new denotation (e.g., pretty printing
%% or compilation), while in other scenarios it is more convenient to change the
%% implementation of an existing semantics (e.g., to switch between a declarative
%% and efficient implementation of an interpreter). \todo{This ignores the aspect
%%   of adding new syntax/components to existing languages}
%% 
What if we want to extend this language?  To add new constructors to the
abstract syntax tree, we must extend the definition of |Expr|, and modify
\emph{all} functions that match on |Expr| accordingly. Furthermore, the new
clauses for these constructors may impose additional requirements on |m| for
which we would need to add more typeclass constraints, and any existing
instantiations of |m| would need to be updated to ensure that they are still a
member of all required typeclasses.

Clearly, for these reasons |Expr| and |eval| in their current form do not work
very well as a reusable language component. We introduce \strachey{}, a
functional meta-language for defining reusable programming language
components. The goal of \strachey{} is to provide a language in which one can
define language components by defining data types and pattern matching
functions, like |Expr| and |eval|, in such a way that we compose the syntax,
interpretations, and effects of a language component without affecting existing
defintion. Importantly, we should also retain the possibility to add completely
new interpretations for existing syntax by writing a new pattern matching
function. In other words, \strachey{} should solve the \emph{expression
  problem}~\cite{wadler1998expression}. 

We can summarize this with following concrete design goals. In \strachey{}, one
should be able to

\begin{itemize}

\item extend existing abstract syntax types with new constructors without having
  to modify existing definitions, 

\item extend existing denotations with clauses for new constructors, and define
  new semantics for existing syntax by defining new denotations, 

\item define abstract effect operations, and use these operations to implement
  denotation clauses without having to worry about the operations needed by
  other clauses, and 

\item define implementations for effect operations that are independent from the
  implementations of other operations.  
  
\end{itemize}


\noindent
There exist abstractions, such as \emph{Data Types \`{a} la
  Carte}~\cite{DBLP:journals/jfp/Swierstra08} and \emph{Algebraic Effects and
  Handlers}~\cite{DBLP:conf/esop/PlotkinP09}, that achieve the same
goals. These provide the well-understood formalism on wich \strachey{} is
built. \strachey{} then provides a convenient surface syntax for working with
these abstractions that avoids the overhead that occurs when encoding them in a
host language like Haskell. 

\strachey{} is work in progress. There is a prototype implementation of an
interpreter and interactive programming environment which we can use to define
and run the examples from this abstract. We are, however, still in the process
of developing and implementing a type system. In particular, we should
statically detect errors resulting from missing implementations of function
clauses.

The name \strachey{} is an abbreviation of ``CompositionalSemantics''.  It is
also the initials of Christopher Strachey, whose pioneering
work~\cite{strachey1966towards} initiated the development of denotational
semantics. In \emph{Fundamental Concepts in Programming
  Languages}~\cite{DBLP:journals/lisp/Strachey00}, Strachey wrote that “the
urgent task in programming languages is to explore the field of semantic
possibilities”, and that we need to “recognize and isolate the central concepts”
of programming languages. Today, five decades later, the words still ring
true. The \strachey{} language aims to address this urgent task in programming
languages, by supporting the definition of reusable (central) programming
language concepts, via compositional denotation functions that map the syntax of
programming languages to their meaning.


