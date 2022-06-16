\documentclass[runningheads]{llncs}
%include polycode.fmt
%include style.fmt

\usepackage{microtype}

%options ghci

%if False
\begin{code}
{-# LANGUAGE ExistentialQuantification #-}

-- Only required for GADTs
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Dynamic

main :: IO ()
main = putStrLn (print_4 (x0 :: Expr_4 PrintDict_4))
    >> putStrLn (print_4 $ opt_4 (x0 :: Expr_4 OptPrintDict_4))
    >> putStrLn (print_4 (x1 :: Expr_4 PrintDict_4))
    >> putStrLn (print_4 $ opt_4 (x1 :: Expr_4 OptPrintDict_4))
    >> putStrLn (print_4 (x2 :: Expr_4 PrintDict_4))
    >> putStrLn (print_4 $ opt_4 (x2 :: Expr_4 OptPrintDict_4))
    >> putStrLn (print_4 (x3 :: Expr_4 PrintDict_4))
    >> putStrLn (print_4 $ opt_4 (x3 :: Expr_4 OptPrintDict_4))

x0 :: Expr_4 d
x0 = Lit_4 42

x1 :: Expr_4 d
x1 = x0 `Add_4` Lit_4 38

x2 :: (Typeable d, GDict (d (Sub_4 d))) => Expr_4 d
x2 = x1 `sub_4` Lit_4 0

x3 :: (Typeable d, GDict (d (Neg_4 d)), GDict (d (Sub_4 d))) => Expr_4 d
x3 = neg_4 x2
\end{code}
%endif

% url style
\usepackage{url}
\urlstyle{same}

\begin{document}

\title{Deep Embedding with Class}
\author{Mart~Lubbers\orcidID{0000-0002-4015-4878}}
\institute{%
	Institute for Computing and Information Sciences,\\
	Radboud University Nijmegen, Nijmegen, The Netherlands\\
	\email{mart@@cs.ru.nl}
}

\maketitle
	
\begin{abstract}
	The two flavours of DSL embedding are shallow and deep embedding.
	In functional languages, shallow embedding models the language constructs as functions in which the semantics are embedded.
	Adding semantics is therefore cumbersome while adding constructs is a breeze.
	Upgrading the functions to type classes lifts this limitation to a certain extent.

	Deeply embedded languages represent their language constructs as data and the semantics are functions on it.
	As a result, the language constructs are embedded in the semantics, hence adding new language constructs is laborious where adding semantics is trouble free.

	This paper shows that by abstracting the semantics functions in deep embedding to type classes, it is possible to easily add language constructs as well.
	So-called classy deep embedding results in DSLs that are extensible both in language constructs and in semantics while maintaining a concrete abstract syntax tree.
	Additionally, little type-level trickery or complicated boilerplate code is required to achieve this.
	\keywords{functional programming\and Haskell \and embedded domain-specific languages}
\end{abstract}

\section{Introduction}%
\label{sec:intro}

The two flavours of DSL embedding are deep and shallow embedding~\cite{boulton_experience_1992}.
In functional programming languages, shallow embedding models language constructs as functions in the host language.
As a result, adding new language constructs---extra functions---is easy.
However, the semantics of the language is embedded in these functions, making it troublesome to add semantics since it requires updating all existing language constructs.

On the other hand, deep embedding models language constructs as data in the host language.
The semantics of the language are represented by functions over the data.
Consequently, adding new semantics, i.e.\ novel functions, is straightforward.
It can be stated that the language constructs are embedded in the functions that form a semantics.
If one wants to add a language construct, all semantics functions must be revisited and revised to avoid ending up with partial functions.

This juxtaposition has been known for many years~\cite{reynolds_user-defined_1978} and discussed by many others~\cite{krishnamurthi_synthesizing_1998} but most famously dubbed the \emph{expression problem} by Wadler~\cite{wadler_expression_1998}:

\begin{quote}
	The \emph{expression problem} is a new name for an old problem.
	The goal is to define a data type by cases, where one can add new cases to the data type and new functions over the data type, without recompiling existing code, and while retaining static type safety (e.g., no casts).
\end{quote}

In shallow embedding, abstracting the functions to type classes disentangles the language constructs from the semantics, allowing extension both ways.
This technique is dubbed tagless-final embedding~\cite{carette_finally_2009}, nonetheless it is no silver bullet.
Some semantics that require an intensional analysis of the syntax tree, such as transformation and optimisations, are difficult to implement in shallow embedding due to the lack of an explicit data structure representing the abstract syntax tree.
The semantics of the DSL have to be combined and must hold some kind of state or context, so that structural information is not lost~\cite{kiselyov_typed_2012}.

\subsection{Research contribution}
This paper shows how to apply the technique observed in tagless-final embedding to deep embedding.
The presented basic technique, christened \emph{classy deep embedding}, does not require advanced type system extensions to be used.
However, it is suitable for type system extensions such as generalised algebraic data types.
While this paper is written as a literate
Haskell~\cite{peyton_jones_haskell_2003} program using some minor extensions provided by GHC~\cite{ghc_team_ghc_2021}, the idea is applicable to other languages as well\footnotemark{}.
\footnotetext{Lubbers, M. (2022): Literate Haskell/lhs2\TeX{} source code of the paper ``Deep Embedding
with Class'': TFP 2022.\ Zenodo.\ \url{https://doi.org/10.5281/zenodo.6650880}.}

\section{Deep embedding}%
\label{sec:deep}

%Pick a DSL, any DSL, pick the language of literal integers and addition.
Consider the simple language of integer literals and addition.
In deep embedding, terms in the language are represented by data in the host language.
Hence, defining the constructs is as simple as creating the following algebraic data type\footnote{All data types and functions are subscripted to indicate the evolution.}.

\begin{code}
data Expr_0  =  Lit_0  Int
             |  Add_0  Expr_0 Expr_0
\end{code}

Semantics are defined as functions on the |Expr_0| data type.
For example, a function transforming the term to an integer---an evaluator---is implemented as follows.

\begin{code}
eval_0 :: Expr_0 -> Int
eval_0 (Lit_0 e)      =  e
eval_0 (Add_0 e1 e2)  =  eval_0 e1 + eval_0 e2
\end{code}

Adding semantics---e.g.\ a printer---just means adding another function while the existing functions remain untouched.
I.e.\ the key property of deep embedding.
The following function, transforming the |Expr_0| data type to a string, defines a simple printer for our language.

\begin{code}
print_0 :: Expr_0 -> String
print_0 (Lit_0  v)      =  show v
print_0 (Add_0  e1 e2)  =  "(" ++ print_0 e1 ++ "-" ++ print_0 e2 ++ ")"
\end{code}

While the language is concise and elegant, it is not very expressive.
Traditionally, extending the language is achieved by adding a case to the |Expr_0| data type.
So, adding subtraction to the language results in the following revised data type.

\begin{spec}
data Expr_0  =  Lit_0  Int
             |  Add_0  Expr_0 Expr_0
             |  Sub_0  Expr_0 Expr_0
\end{spec}

Extending the DSL with language constructs exposes the Achilles' heel of deep embedding.
Adding a case to the data type means that all semantics functions have become partial and need to be updated to be able to handle this new case.
This does not seem like an insurmountable problem, but it does pose a problem if either the functions or the data type itself are written by others or are contained in a closed library.

\section{Shallow embedding}%
\label{sec:shallow}

Conversely, let us see how this would be done in shallow embedding.
First, the data type is represented by functions in the host language with embedded semantics.
Therefore, the evaluators for literals and addition both become a function in the host language as follows.

\begin{code}
type Sem_s = Int

lit_s :: Int -> Sem_s
lit_s i = i

add_s :: Sem_s -> Sem_s -> Sem_s
add_s e1 e2 = e1 + e2
\end{code}

Adding constructions to the language is done by adding functions.
Hence, the following function adds subtraction to our language.

\begin{code}
sub_s :: Sem_s -> Sem_s -> Sem_s
sub_s e1 e2 = e1 - e2
\end{code}

Adding semantics on the other hand---e.g.\ a printer---is not that simple because the semantics are part of the functions representing the language constructs.
One way to add semantics is to change all functions to execute both semantics at the same time.
In our case this means changing the type of |Sem_s| to be |(Int, String)| so that all functions operate on a tuple containing the result of the evaluator and the printed representation at the same time. %chktex 36
Alternatively, a single semantics can be defined that represents a fold over the language constructs~\cite{gibbons_folding_2014}, delaying the selection of semantics to the moment the fold is applied.

\subsection{Tagless-final embedding}\label{sec:tagless-final}
Tagless-final embedding overcomes the limitations of standard shallow embedding.
To upgrade to this embedding technique, the language constructs are changed from functions to type classes.
For our language this results in the following type class definition.

\begin{code}
class Expr_t s where
    lit_t  ::  Int -> s
    add_t  ::  s -> s -> s
\end{code}

Semantics become data types\footnotemark{} implementing these type classes, resulting in the following instance for the evaluator.
\footnotetext{%
	In this case |newtype|s are used instead of regular |data| declarations.
	A |newtype| is a special data type with a single constructor containing a single value only to which it is isomorphic.
	It allows the programmer to define separate class instances that the instances of the isomorphic type without any overhead.
	During compilation the constructor is completely removed~\cite[Sec.~4.2.3]{peyton_jones_haskell_2003}.
}

\begin{code}
newtype Eval_t = E_t Int

instance Expr_t Eval_t where
    lit_t  v                  =  E_t v
    add_t  (E_t e1) (E_t e2)  =  E_t (e1 + e2)
\end{code}

Adding constructs---e.g.\ subtraction---just results in an extra type class and corresponding instances.

\begin{code}
class Sub_t s where
    sub_t :: s -> s -> s

instance Sub_t Eval_t where
    sub_t (E_t e1) (E_t e2) = E_t (e1 - e2)
\end{code}

Finally, adding semantics such as a printer over the language is achieved by providing a data type representing the semantics accompanied by instances for the language constructs.

\begin{code}
newtype Printer_t = P_t String

instance Expr_t Printer_t where
    lit_t  i                  =  P_t (show i)
    add_t  (P_t e1) (P_t e2)  =  P_t ("(" ++ e1 ++ "+" ++ e2 ++ ")")

instance Sub_t Printer_t where
    sub_t  (P_t e1) (P_t e2)  =  P_t ("(" ++ e1 ++ "-" ++ e2 ++ ")")
\end{code}

\section{Lifting the backends}%
\label{sec:lift}

Let us rethink the deeply embedded DSL design.
Remember that in shallow embedding, the semantics are embedded in the language construct functions.
Obtaining extensibility both in constructs and semantics was accomplished by abstracting the semantics functions to type classes, making the constructs overloaded in the semantics.
In deep embedding, the constructs are embedded in the semantics functions instead.
So, let us apply the same technique, i.e.\ make the semantics overloaded in the language constructs by abstracting the semantics functions to type classes.
The same effect may be achieved when using similar techniques such as explicit dictionary passing or ML style modules.
In our language this results in the following type class.

\begin{code}
class Eval_1 v where
    eval_1 :: v -> Int

data Expr_1  =  Lit_1  Int
             |  Add_1  Expr_1 Expr_1
\end{code}

Implementing the semantics type class instances for the |Expr_1| data type is an elementary exercise.
By a copy-paste and some modifications, we come to the following implementation.

\begin{code}
instance Eval_1 Expr_1 where
    eval_1 (Lit_1  v)      =  v
    eval_1 (Add_1  e1 e2)  =  eval_1 e1 + eval_1 e2
\end{code}

Subtraction can now be defined in a separate data type, leaving the original data type intact.
Instances for the additional semantics can now be implemented separately as instances of the type classes.

\begin{code}
data Sub_1 = Sub_1 Expr_1 Expr_1

instance Eval_1 Sub_1 where
    eval_1 (Sub_1 e1 e2) = eval_1 e1 - eval_1 e2
\end{code}

\section{Existential data types}%
\label{sec:existential}

The astute reader might have noticed that we have dissociated ourselves from the original data type.
It is only possible to create an expression with a subtraction on the top level.
The recursive knot is left untied and as a result, |Sub_1| can never be reached from an |Expr_1|.

Luckily, we can reconnect them by adding a special constructor to the |Expr_1| data type for housing extensions.
It contains an existentially quantified~\cite{mitchell_abstract_1988} type with type class constraints~\cite{laufer_combining_1994,laufer_type_1996} for all semantics type classes~\cite[Chp.~6.4.6]{ghc_team_ghc_2021} to allow it to house not just subtraction but any future extension.

\begin{code}
data Expr_2  =                         Lit_2  Int
             |                         Add_2  Expr_2 Expr_2
             |  forall x. Eval_2 x =>  Ext_2  x
\end{code}

%if False
\begin{code}
data Sub_2 = Sub_2 Expr_2 Expr_2

class Eval_2 v where
    eval_2 :: v -> Int
\end{code}
%endif

The implementation of the extension case in the semantics type classes is in most cases just a matter of calling the function for the argument as can be seen in the semantics instances shown below.

\begin{code}
instance Eval_2 Expr_2 where
    eval_2 (Lit_2  v)      = v
    eval_2 (Add_2  e1 e2)  = eval_2 e1 + eval_2 e2
    eval_2 (Ext_2  x)      = eval_2 x
\end{code}

%if False
\begin{code}
instance Eval_2 Sub_2 where
    eval_2 (Sub_2 e1 e2) = eval_2 e1 - eval_2 e2
\end{code}
%endif

Adding language construct extensions in different data types does mean that an extra |Ext_2| tag is introduced when using the extension.
This burden can be relieved by creating a smart constructor for it that automatically wraps the extension with the |Ext_2| constructor so that it is of the type of the main data type.

\begin{code}
sub_2 :: Expr_2 -> Expr_2 -> Expr_2
sub_2 e1 e2 = Ext_2 (Sub_2 e1 e2)
\end{code}

In our example this means that the programmer can write\footnotemark{}:
\footnotetext{%
	Backticks are used to use functions or constructors in an infix fashion~\cite[Sec.~4.3.3]{peyton_jones_haskell_2003}.
}
\begin{code}
e2  ::  Expr_2
e2  =   Lit_2 42 `sub_2` Lit_2 1
\end{code}
instead of having to write
\begin{code}
e2p  ::  Expr_2
e2p  =   Ext_2 (Lit_2 42 `Sub_2` Lit_2 1)
\end{code}

\subsection{Unbraiding the semantics from the data}
This approach does reveal a minor problem.
Namely, that all semantics type classes are braided into our datatypes via the |Ext_2| constructor.
Say if we add the printer again, the |Ext_2| constructor has to be modified to contain the printer type class constraint as well\footnote{Resulting in the following constructor: |forall x.(Eval_2 x, Print_2 x) => Ext_2 x|.}. %chktex 36
Thus, if we add semantics, the main data type's type class constraints in the |Ext_2| constructor need to be updated.
To avoid this, the type classes can be bundled in a type class alias or type class collection as follows.

\begin{spec}
class (Eval_2 x, Print_2 x) => Semantics_2 x

data Expr_2  =                              Lit_2  Int
             |                              Add_2  Expr_2 Expr_2
             |  forall x. Semantics_2 x =>  Ext_2  x
\end{spec}

The class alias removes the need for the programmer to visit the main data type when adding additional semantics.
Unfortunately, the compiler does need to visit the main data type again.
Some may argue that adding semantics happens less frequently than adding language constructs but in reality it means that we have to concede that the language is not as easily extensible in semantics as in language constructs.
More exotic type system extensions such as constraint kinds~\cite{bolingbroke_constraint_2011,yorgey_giving_2012} can untangle the semantics from the data types by making the data types parametrised by the particular semantics.
However, by adding some boilerplate, even without this extension, the language constructs can be parametrised by the semantics by putting the semantics functions in a data type.
First the data types for the language constructs are parametrised by the type variable |d| as follows.

\begin{code}
data Expr_3 d     =             Lit_3  Int
                  |             Add_3  (Expr_3 d) (Expr_3 d)
                  |  forall x.  Ext_3  (d x) x
\end{code}

\begin{code}
data Sub_3 d  =             Sub_3  (Expr_3 d) (Expr_3 d)
\end{code}

The |d| type variable is inhabited by an explicit dictionary for the semantics, i.e.\ a witness to the class instance.
Therefore, for all semantics type classes, a data type is made that contains the semantics function for the given semantics.
This means that for |Eval_3|, a dictionary with the function |EvalDict_3| is defined, a type class |HasEval_3| for retrieving the function from the dictionary and an instance for |HasEval_3| for |EvalDict_3|.

\begin{code}
newtype EvalDict_3 v = EvalDict_3 (v -> Int)

class HasEval_3 d where
    getEval_3 :: d v -> v -> Int

instance HasEval_3 EvalDict_3 where
    getEval_3 (EvalDict_3 e) = e
\end{code}

%if False
\begin{code}
class Eval_3 v where
    eval_3 :: v -> Int
\end{code}
%endif

The instances for the type classes change as well according to the change in the datatype.
Given that there is a |HasEval_3| instance for the witness type |d|, we can provide an implementation of |Eval_3| for |Expr_3 d|.

\begin{code}
instance HasEval_3 d => Eval_3 (Expr_3 d) where
    eval_3 (Lit_3  v)      = v
    eval_3 (Add_3  e1 e2)  = eval_3 e1 + eval_3 e2
    eval_3 (Ext_3  d x)    = getEval_3 d x
\end{code}

\begin{code}
instance HasEval_3 d => Eval_3 (Sub_3 d) where
    eval_3 (Sub_3 e1 e2) = eval_3 e1 - eval_3 e2
\end{code}

Because the |Ext_3| constructor from |Expr_3| now contains a value of type |d|, the smart constructor for |Sub_3| must somehow come up with this value.
To achieve this, a type class is introduced that allows the generation of such a dictionary.

\begin{code}
class GDict a where
    gdict :: a
\end{code}

This type class has individual instances for all semantics dictionaries, linking the class instance to the witness value.
I.e.\ if there is a type class instance known, a witness value can be conjured using the |gdict| function.

\begin{code}
instance Eval_3 v => GDict (EvalDict_3 v) where
    gdict = EvalDict_3 eval_3
\end{code}

With these instances, the semantics function can be retrieved from the |Ext_3| constructor and in the smart constructors they can be generated as follows:

\begin{code}
sub_3 :: GDict (d (Sub_3 d)) => Expr_3 d -> Expr_3 d -> Expr_3 d
sub_3 e1 e2 = Ext_3 gdict (Sub_3 e1 e2)
\end{code}

Finally, we reached the end goal, orthogonal extension of both language constructs as shown by adding subtraction to the language and in language semantics.
Adding the printer can now be done without touching the original code as follows.
First the printer type class, dictionaries and instances for |GDict| are defined.

\begin{code}
class Print_3 v where
    print_3 :: v -> String

newtype PrintDict_3 v = PrintDict_3 (v -> String)

class HasPrint_3 d where
    getPrint_3 :: d v -> v -> String

instance HasPrint_3 PrintDict_3 where
    getPrint_3 (PrintDict_3 e) = e

instance Print_3 v => GDict (PrintDict_3 v) where
    gdict = PrintDict_3 print_3
\end{code}

Then the instances for |Print_3| of all the language constructs can be defined.

\begin{code}
instance HasPrint_3 d => Print_3 (Expr_3 d) where
    print_3 (Lit_3  v)      =  show v
    print_3 (Add_3  e1 e2)  =  "(" ++ print_3 e1 ++ "+" ++ print_3 e2 ++ ")"
    print_3 (Ext_3  d x)    =  getPrint_3 d x
\end{code}
\begin{code}
instance HasPrint_3 d => Print_3 (Sub_3 d) where
    print_3 (Sub_3 e1 e2)  = "(" ++ print_3 e1 ++ "-" ++ print_3 e2 ++ ")"
\end{code}

\section{Transformation semantics}%
\label{sec:transformation}

Most semantics convert a term to some final representation and can be expressed just by functions on the cases.
However, the implementation of semantics such as transformation or optimisation may benefit from a so-called intentional analysis of the abstract syntax tree.
In shallow embedding, the implementation for these types of semantics is difficult because there is no tangible abstract syntax tree.
In off-the-shelf deep embedding this is effortless since the function can pattern match on the constructor or structures of constructors.

To demonstrate intensional analyses in classy deep embedding we write an optimizer that removes addition and subtraction by zero.
In classy deep embedding, adding new semantics means first adding a new type class housing the function including the machinery for the extension constructor.

\begin{code}
class Opt_3 v where
    opt_3 :: v -> v

newtype OptDict_3 v = OptDict_3 (v -> v)

class HasOpt_3 d where
    getOpt_3 :: d v -> v -> v

instance HasOpt_3 OptDict_3 where
    getOpt_3 (OptDict_3 e) = e

instance Opt_3 v => GDict (OptDict_3 v) where
    gdict = OptDict_3 opt_3
\end{code}

The implementation of the optimizer for the |Expr_3| data type is no complicated task.
The only interesting bit occurs in the |Add_3| constructor, where we pattern match on the optimised children to determine whether an addition with zero is performed.
If this is the case, the addition is removed.

\begin{code}
instance HasOpt_3 d => Opt_3 (Expr_3 d) where
    opt_3 (Lit_3  v)      = Lit_3 v
    opt_3 (Add_3  e1 e2)  = case (opt_3 e1, opt_3 e2) of
        (Lit_3 0,  e2p      ) -> e2p
        (e1p,      Lit_3 0  ) -> e1p
        (e1p,      e2p      ) -> Add_3 e1p e2p
    opt_3 (Ext_3  d x)    = Ext_3 d (getOpt_3 d x)
\end{code}

Replicating this for the |Opt_3| instance of |Sub_3| seems a clear-cut task at first glance.

\begin{spec}
instance HasOpt_3 d => Opt_3 (Sub_3 d) where
    opt_3 (Sub_3 e1 e2) = case (opt_3 e1, opt_3 e2) of
        (e1p, Lit_3 0  ) -> e1p
        (e1p, e2p      ) -> Sub_3 e1p e2p
\end{spec}

Unsurprisingly, this code is rejected by the compiler.
When a literal zero is matched as the right-hand side of a subtraction, the left-hand side of type |Expr_3| is returned.
However, the type signature of the function dictates that it should be of type |Sub_3|.
To overcome this problem we add a convolution constructor.

\subsection{Convolution}%
\label{ssec:convolution}

Adding a loopback case or convolution constructor to |Sub_3| allows the removal of the |Sub_3| constructor while remaining the |Sub_3| type.
It should be noted that a loopback case is \emph{only} required if the transformation actually removes tags.
This changes the |Sub_3| data type as follows.

\begin{code}
data Sub_4 d  =  Sub_4      (Expr_4 d) (Expr_4 d)
              |  SubLoop_4  (Expr_4 d)

instance HasEval_4 d => Eval_4 (Sub_4 d) where
    eval_4 (Sub_4      e1 e2)  = eval_4 e1 - eval_4 e2
    eval_4 (SubLoop_4  e1)     = eval_4 e1
\end{code}

With this loopback case in the toolbox, the following |Sub| instance optimises away subtraction with zero literals.

\begin{code}
instance HasOpt_4 d => Opt_4 (Sub_4 d) where
    opt_4 (Sub_4  e1 e2)  = case (opt_4 e1, opt_4 e2) of
        (e1p, Lit_4 0  ) -> SubLoop_4 e1p
        (e1p, e2p      ) -> Sub_4 e1p e2p
    opt_4 (SubLoop_4 e)   =  SubLoop_4 (opt_4 e)
\end{code}

\subsection{Pattern matching}%
\label{ssec:pattern}

Pattern matching within datatypes and from an extension to the main data type works out of the box.
Cross-extensional pattern matching on the other hand---matching on a particular extension---is something that requires a bit of extra care.
Take for example negation propagation and double negation elimination.
Pattern matching on values with an existential type is not possible without leveraging dynamic typing~\cite{abadi_dynamic_1991,baars_typing_2002}.
To enable dynamic typing support, the |Typeable| type class as provided by |Data.Dynamic|~\cite{ghc_team_datadynamic_2021} is added to the list of constraints in all places where we need to pattern match across extensions.
As a result, the |Typeable| type class constraints are added to the quantified type variable |x| of the |Ext_4| constructor and to |d|s in the smart constructors.

\begin{code}
data Expr_4 d  =                           Lit_4  Int
               |                           Add_4  (Expr_4 d) (Expr_4 d)
               |  forall x. Typeable x =>  Ext_4  (d x) x
\end{code}

%if False
\begin{code}
sub_4 :: (Typeable d, GDict (d (Sub_4 d))) => Expr_4 d -> Expr_4 d -> Expr_4 d
sub_4 e1 e2 = Ext_4 gdict (Sub_4 e1 e2)

newtype EvalDict_4 v = EvalDict_4 (v -> Int)

class HasEval_4 d where
    getEval_4 :: d v -> v -> Int

instance HasEval_4 EvalDict_4 where
    getEval_4 (EvalDict_4 e) = e

class Eval_4 v where
    eval_4 :: v -> Int

instance HasEval_4 d => Eval_4 (Expr_4 d) where
    eval_4 (Lit_4  v)      = v
    eval_4 (Add_4  e1 e2)  = eval_4 e1 + eval_4 e2
    eval_4 (Ext_4  d x)    = getEval_4 d x

class Print_4 v where
    print_4 :: v -> String

newtype PrintDict_4 v = PrintDict_4 (v -> String)

class HasPrint_4 d where
    getPrint_4 :: d v -> v -> String
instance HasPrint_4 PrintDict_4 where
    getPrint_4 (PrintDict_4 e) = e

instance Print_4 v => GDict (PrintDict_4 v) where
    gdict = PrintDict_4 print_4
instance HasPrint_4 d => Print_4 (Expr_4 d) where
    print_4 (Lit_4  v)      =  show v
    print_4 (Add_4  e1 e2)  =  "(" ++ print_4 e1 ++ "+" ++ print_4 e2 ++ ")"
    print_4 (Ext_4  d x)    =  getPrint_4 d x
instance HasPrint_4 d => Print_4 (Sub_4 d) where
    print_4 (Sub_4 e1 e2)  = "(" ++ print_4 e1 ++ "-" ++ print_4 e2 ++ ")"
    print_4 (SubLoop_4 e)  = print_4 e


class Opt_4 v where
    opt_4 :: v -> v

newtype OptDict_4 v = OptDict_4 (v -> v)

class HasOpt_4 d where
    getOpt_4 :: d v -> v -> v

instance HasOpt_4 OptDict_4 where
    getOpt_4 (OptDict_4 e) = e

instance Opt_4 v => GDict (OptDict_4 v) where
    gdict = OptDict_4 opt_4

instance HasOpt_4 d => Opt_4 (Expr_4 d) where
    opt_4 (Lit_4  v)      = Lit_4 v
    opt_4 (Add_4  e1 e2)  = case (opt_4 e1, opt_4 e2) of
        (Lit_4 0, e2p)    -> e2p
        (e1p, Lit_4 0)    -> e1p
        (e1p, e2p)        -> Add_4 e1p e2p
    opt_4 (Ext_4  d x)    = Ext_4 d (getOpt_4 d x)
\end{code}
%endif

First let us add negation to the language by defining a datatype representing it.
Negation elimination requires the removal of negation constructors, so a convolution constructor is defined as well.

\begin{code}
data Neg_4 d  =  Neg_4      (Expr_4 d)
              |  NegLoop_4  (Expr_4 d)

neg_4 :: (Typeable d, GDict (d (Neg_4 d))) => Expr_4 d -> Expr_4 d
neg_4 e = Ext_4 gdict (Neg_4 e)
\end{code}

The evaluation and printer instances for the |Neg_4| datatype are defined as follows.

\begin{code}
instance HasEval_4 d => Eval_4 (Neg_4 d) where
    eval_4  (Neg_4      e)  = negate (eval_4 e)
    eval_4  (NegLoop_4  e)  = eval_4 e

instance HasPrint_4 d => Print_4 (Neg_4 d) where
    print_4  (Neg_4      e)  = "(~" ++ print_4 e ++ ")"
    print_4  (NegLoop_4  e)  = print_4 e
\end{code}

The |Opt_4| instance contains the interesting bit.
If the sub expression of a negation is an addition, negation is propagated downwards.
If the sub expression is again a negation, something that can only be found out by a dynamic pattern match, it is replaced by a |NegLoop_4| constructor.

\begin{code}
instance (Typeable d, GDict (d (Neg_4 d)), HasOpt_4 d) =>
        Opt_4 (Neg_4 d) where
    opt_4  (Neg_4  (Add_4 e1 e2))
        = NegLoop_4 (Add_4 (opt_4 (neg_4 e1)) (opt_4 (neg_4 e2)))
    opt_4  (Neg_4  (Ext_4 d x))
        = case fromDynamic (toDyn (getOpt_4 d x)) of
            Just (Neg_4 e)  -> NegLoop_4 e
            _               -> Neg_4 (Ext_4 d (getOpt_4 d x))
    opt_4  (Neg_4      e)  = Neg_4 (opt_4 e)
    opt_4  (NegLoop_4  e)  = NegLoop_4 (opt_4 e)
\end{code}

Loopback cases do make cross-extensional pattern matching less modular in general.
For example, |Ext_4 d (SubLoop_4 (Lit_4 0))| is equivalent to |Lit_4 0| in the optimisation semantics and would require an extra pattern match.
Fortunately, this problem can be mitigated---if required---by just introducing an additional optimisation semantics that removes loopback cases.
Luckily, one does not need to resort to these arguably blunt matters often.
Dependent language functionality often does not need to span extensions, i.e.\ it is possible to group them in the same data type.

\subsection{Chaining semantics}
Now that the data types are parametrised by the semantics a final problem needs to be overcome.
The data type is parametrised by the semantics, thus, using multiple semantics, such as evaluation after optimising is not straightforwardly possible.
Luckily, a solution is readily at hand: introduce an ad-hoc combination semantics.

\begin{code}
data OptPrintDict_4 v = OPD_4 (OptDict_4 v) (PrintDict_4 v)

instance HasOpt_4    OptPrintDict_4 where
    getOpt_4    (OPD_4  v  _)  = getOpt_4 v
instance HasPrint_4  OptPrintDict_4 where
    getPrint_4  (OPD_4  _  v)  = getPrint_4 v

instance (Opt_4 v, Print_4 v) => GDict (OptPrintDict_4 v) where
    gdict = OPD_4 gdict gdict
\end{code}

And this allows us to write |print_4 (opt_4 e1)| resulting in \eval{print_4 (opt_4 e1)} when |e1| represents $(\sim(42+38))-0$ and is thus defined as follows.

\begin{code}
e1 :: Expr_4 OptPrintDict_4
e1 = neg_4 (Lit_4 42 `Add_4` Lit_4 38) `sub_4` Lit_4 0
\end{code}

When using classy deep embedding to the fullest, the ability of the compiler to infer very general types expires.
As a consequence, defining reusable expressions that are overloaded in their semantics requires quite some type class constraints that cannot be inferred by the compiler (yet) if they use many extensions.
Solving this remains future work.
For example, the expression $\sim(42-38)+1$ has to be defined as:

\begin{code}
e3 ::  ( Typeable d
       , GDict (d (Neg_4 d))
       , GDict (d (Sub_4 d))) => Expr_4 d
e3 = neg_4 (Lit_4 42 `sub_4` Lit_4 38) `Add_4` Lit_4 1
\end{code}

\section{Generalised algebraic data types}%
\label{sec:generalisation}
Generalised algebraic data types (GADTs) are enriched data types that allow the type instantiation of the constructor to be explicitly defined~\cite{cheney_first-class_2003,hinze_fun_2003}.
Leveraging GADTs, deeply embedded DSLs can be made statically type safe even when different value types are supported.
Even when GADTs are not supported natively in the language, they can be simulated using embedding-projection pairs or equivalence types~\cite[Sec.~2.2]{cheney_lightweight_2002}.
Where some solutions to the expression problem do not easily generalise to GADTs (see Section~\ref{sec:related}), classy deep embedding does.
Generalising the data structure of our DSL is fairly straightforward and to spice things up a bit, we add an equality and boolean not language construct.
To make the existing DSL constructs more general, we relax the types of those constructors.
For example, operations on integers now work on all numerals instead.
Moreover, the |Lit_g| constructor can be used to lift values of any type to the DSL domain as long as they have a |Show| instance, required for the printer.
Since some optimisations on |Not_g| remove constructors and therefore use cross-extensional pattern matches, |Typeable| constraints are added to |a|.
Furthermore, because the optimisations for |Add_g| and |Sub_g| are now more general, they do not only work for |Int|s but for any type with a |Num| instance, the |Eq| constraint is added to these constructors as well.
Finally, not to repeat ourselves too much, we only show the parts that substantially changed.
The omitted definitions and implementation can be found in Appendix~\ref{sec:appendix}.

\begin{code}
data Expr_g d a   where
    Lit_g      ::  Show a         =>  a -> Expr_g d a
    Add_g      ::  (Eq a, Num a)  =>  Expr_g d a -> Expr_g d a -> Expr_g d a
    Ext_g      ::  Typeable x     =>  d x -> x a -> Expr_g d a
data Neg_g d a    where
    Neg_g      ::  (Typeable a, Num a) =>  Expr_g d a -> Neg_g d a
    NegLoop_g  ::                          Expr_g d a -> Neg_g d a
data Not_g d a where
    Not_g      ::  Expr_g d Bool -> Not_g d Bool
    NotLoop_g  ::  Expr_g d a -> Not_g d a
\end{code}

The smart constructors for the language extensions inherit the class constraints of their data types and include a |Typeable| constraint on the |d| type variable for it to be usable in the |Ext_g| constructor as can be seen in the smart constructor for |Neg_g|:

\begin{code}
neg_g  :: (Typeable d, GDict (d (Neg_g d)), Typeable a, Num a) =>
    Expr_g d a -> Expr_g d a
neg_g e = Ext_g gdict (Neg_g e)

not_g  :: (Typeable d,  GDict (d (Not_g d))) =>
    Expr_g d Bool -> Expr_g d Bool
not_g e = Ext_g gdict (Not_g e)
\end{code}

Upgrading the semantics type classes to support GADTs is done by an easy textual search and replace.
All occurrences of |v| are now parametrised by type variable |a|:

\begin{code}
class Eval_g   v where
    eval_g   :: v a -> a
class Print_g  v where
    print_g  :: v a -> String
class Opt_g    v where
    opt_g    :: v a -> v a
\end{code}

Now that the shape of the type classes has changed, the dictionary data types and the type classes need to be adapted as well.
The introduced type variable |a| is not an argument to the type class, so it should not be an argument to the dictionary data type.
To represent this type class function, a rank-2 polymorphic function is needed~\cite[Chp.~6.4.15]{ghc_team_ghc_2021}\cite{odersky_putting_1996}.
Concretely, for the evaluatior this results in the following definitions:

\begin{code}
newtype EvalDict_g v = EvalDict_g (forall a. v a -> a)
class HasEval_g d where
    getEval_g :: d v -> v a -> a
instance HasEval_g EvalDict_g where
    getEval_g (EvalDict_g e) = e
\end{code}

The |GDict| type class is general enough, so the instances can remain the same.
The |Eval_g| instance of |GDict| looks as follows:

\begin{code}
instance Eval_g v => GDict (EvalDict_g v) where
    gdict = EvalDict_g eval_g
\end{code}

Finally, the implementations for the instances can be ported without complication show using the optimisation instance of |Not_g|:

\begin{code}
instance (Typeable d, GDict (d (Not_g d)), HasOpt_g d) =>
        Opt_g (Not_g d) where
    opt_g (Not_g (Ext_g d x))
        = case fromDynamic (toDyn (getOpt_g d x)) :: Maybe (Not_g d Bool) of
            Just (Not_g e)  ->  NotLoop_g e
            _               ->  Not_g (Ext_g d (getOpt_g d x))
    opt_g (Not_g e)       =  Not_g (opt_g e)
    opt_g (NotLoop_g  e)  =  NotLoop_g (opt_g e)
\end{code}

\section{Conclusion}%
\label{sec:conclusion}

Classy deep embedding is a novel organically grown embedding technique that alleviates deep embedding from the extensibility problem in most cases.

By abstracting the semantics functions to type classes they become overloaded in the language constructs.
Thus, making it possible to add new language constructs in a separate type.
These extensions are brought together in a special extension constructor residing in the main data type.
This extension case is overloaded by the language construct using a data type containing the class dictionary.
As a result, orthogonal extension is possible for language constructs and semantics using only little syntactic overhead or type annotations.
The basic technique only requires---well established through history and relatively standard---existential data types.
However, if needed, the technique generalises to GADTs as well, adding rank-2 types to the list of type system requirements as well.
Finally, the abstract syntax tree remains observable which makes it suitable for intensional analyses, albeit using occasional dynamic typing for truly cross-extensional transformations.

Defining reusable expressions overloaded in semantics or using multiple semantics on a single expression requires some boilerplate still, getting around this remains future work.

\section{Related work}%
\label{sec:related}

Embedded DSL techniques in functional languages have been a topic of research for many years, thus we do not claim a complete overview of related work.

Clearly, classy deep embedding bears most similarity to the \emph{Datatypes \`a la Carte}~\cite{swierstra_data_2008}.
In Swierstra's approach, semantics are lifted to type classes similarly to classy deep embedding.
Each language construct is their own datatype parametrised by a type parameter.
This parameter contains some type level representation of language constructs that are in use.
In classy deep embedding, extensions do not have to be enumerated at the type level but are captured in the extension case.
Because all the constructs are expressed in the type system, nifty type system tricks need to be employed to convince the compiler that everything is type safe and the class constraints can be solved.
Furthermore, it requires some boilerplate code such as functor instances for the data types.
In return, pattern matching is easier and does not require dynamic typing.
Classy deep embedding only strains the programmer with writing the extension case for the main data type and the occasional loopback constructor.

L\"oh and Hinze proposed a language extension that allows open data types and open functions, i.e.\ functions and data types that can be extended with more cases later on~\cite{loh_open_2006}.
They hinted at the possibility of using type classes for open functions but had serious concerns that pattern matching would be crippled because constructors are becoming types, thus ultimately becoming impossible to type.
In contrast, this paper shows that pattern matching is easily attainable---albeit using dynamic types---and that the terms can be typed without complicated type system extensions.

A technique similar to classy deep embedding was proposed by Najd and Peyton~Jones to tackle a slightly different problem, namely that of reusing a data type for multiple purposes in a slightly different form~\cite{najd_trees_2017}.
For example to decorate the abstract syntax tree of a compiler differently for each phase of the compiler.
They propose to add an extension descriptor as a type variable to a data type and a type family that can be used to decorate constructors with extra information and add additional constructors to the data type using an extension constructor.
Classy deep embedding works similarly but uses existentially quantified type variables to describe possible extensions instead of type variables and type families.
In classy deep embedding, the extensions do not need to be encoded in the type system and less boilerplate is required.
Furthermore, pattern matching on extensions becomes a bit more complicated but in return it allows for multiple extensions to be added orthogonally and avoids the necessity for type system extensions.

Tagless-final embedding is the shallowly embedded counterpart of classy deep embedding and was invented for the same purpose; overcoming the issues with standard shallow embedding~\cite{carette_finally_2009}.
Classy deep embedding was organically grown from observing the evolution of tagless-final embedding.
The main difference between tagless-final embedding and classy deep embedding---and in general between shallow and deep embedding---is that intensional analyses of the abstract syntax tree is more difficult because there is no tangible abstract syntax tree data structure.
In classy deep embedding, it is possible to define transformations even across extensions.

Hybrid approaches between deep and shallow embedding exist as well.
For example, Svenningson et al.\ show that by expressing the deeply embedded language in a shallowly embedded core language, extensions can be made orthogonally as well~\cite{svenningsson_combining_2013}.
This paper differs from those approaches in the sense that it does not require a core language in which all extensions need to be expressible.

\section*{Acknowledgements}\label{sec:acknowledgements}
This research is partly funded by the Royal Netherlands Navy.
Furthermore, I would like to thank Pieter and Rinus for the fruitful discussions, Ralf for inspiring me to write a functional pearl, and the anonymous reviewers for their valuable and honest comments.

\bibliographystyle{splncs04}
\bibliography{refs}

\appendix
\section{Appendix}%
\label{sec:appendix}
\subsection{Data type definitions}
\begin{code}
data Sub_g d a    where
    Sub_g      ::  (Eq a, Num a)       =>  Expr_g d a -> Expr_g d a -> Sub_g d a
    SubLoop_g  ::                          Expr_g d a -> Sub_g d a

data Eq_g d a     where
    Eq_g      ::   (Typeable a, Eq a)  =>  Expr_g d a -> Expr_g d a -> Eq_g d Bool
    EqLoop_g  ::                           Expr_g d a -> Eq_g d a
\end{code}

\subsection{Smart constructors}
\begin{code}
sub_g  :: (Typeable d,  GDict (d (Sub_g d)), Eq a, Num a) =>
    Expr_g d a -> Expr_g d a -> Expr_g d a
sub_g e1 e2 = Ext_g gdict (Sub_g e1 e2)

eq_g   :: (Typeable d,  GDict (d (Eq_g d)), Eq a, Typeable a) =>
    Expr_g d a -> Expr_g d a -> Expr_g d Bool
eq_g e1 e2 = Ext_g gdict (Eq_g e1 e2)
\end{code}

\subsection{Semantics classes and data types}
\begin{code}
newtype PrintDict_g v = PrintDict_g (forall a.v a -> String)

class HasPrint_g d where
    getPrint_g :: d v -> v a -> String

instance HasPrint_g PrintDict_g where
    getPrint_g (PrintDict_g e) = e
\end{code}

\begin{code}
newtype OptDict_g v = OptDict_g (forall a.v a -> v a)

class HasOpt_g d where
    getOpt_g :: d v -> v a -> v a

instance HasOpt_g OptDict_g where
    getOpt_g (OptDict_g e) = e
\end{code}

\subsection{|GDict| instances}
\begin{code}
instance Print_g v  =>  GDict (PrintDict_g v)  where
    gdict = PrintDict_g print_g
instance Opt_g v    =>  GDict (OptDict_g v)    where
    gdict = OptDict_g opt_g
\end{code}

\subsection{Evaluator instances}
\begin{code}
instance HasEval_g d => Eval_g (Expr_g d) where
    eval_g (Lit_g  v)      =  v
    eval_g (Add_g  e1 e2)  =  eval_g e1 + eval_g e2
    eval_g (Ext_g  d x)    =  getEval_g d x
\end{code}

\begin{code}
instance HasEval_g d => Eval_g (Sub_g d) where
    eval_g (Sub_g      e1 e2)  =  eval_g e1 - eval_g e2
    eval_g (SubLoop_g  e)      =  eval_g e
\end{code}

\begin{code}
instance HasEval_g d => Eval_g (Neg_g d) where
    eval_g (Neg_g      e)  =  negate (eval_g e)
    eval_g (NegLoop_g  e)  =  eval_g e
\end{code}

\begin{code}
instance HasEval_g d => Eval_g (Eq_g d) where
    eval_g (Eq_g  e1 e2)   =  eval_g e1 == eval_g e2
    eval_g (EqLoop_g  e)   =  eval_g e
\end{code}

\begin{code}
instance HasEval_g d => Eval_g (Not_g d) where
    eval_g (Not_g  e)      =  not (eval_g e)
    eval_g (NotLoop_g  e)  =  eval_g e
\end{code}

\subsection{Printer instances}
\begin{code}
instance HasPrint_g d => Print_g (Expr_g d) where
    print_g (Lit_g  v)      =  show v
    print_g (Add_g  e1 e2)  =  "(" ++ print_g e1 ++ "+" ++ print_g e2 ++ ")"
    print_g (Ext_g  d x)    =  getPrint_g d x
\end{code}

\begin{code}
instance HasPrint_g d => Print_g (Sub_g d) where
    print_g (Sub_g e1 e2)  =  "(" ++ print_g e1 ++ "-" ++ print_g e2 ++ ")"
    print_g (SubLoop_g e)  =  print_g e
\end{code}

\begin{code}
instance HasPrint_g d => Print_g (Neg_g d) where
    print_g (Neg_g e)       =  "(negate " ++ print_g e ++ ")"
    print_g (NegLoop_g  e)  =  print_g e
\end{code}

\begin{code}
instance HasPrint_g d => Print_g (Eq_g d) where
    print_g (Eq_g e1 e2)  =  "(" ++ print_g e1 ++ "==" ++ print_g e2 ++ ")"
    print_g (EqLoop_g e)  =  print_g e
\end{code}

\begin{code}
instance HasPrint_g d => Print_g (Not_g d) where
    print_g (Not_g e)      =  "(not " ++ print_g e ++ ")"
    print_g (NotLoop_g e)  =  print_g e
\end{code}

\subsection{Optimisation instances}
\begin{code}
instance HasOpt_g d => Opt_g (Expr_g d) where
    opt_g (Lit_g  v)      =  Lit_g v
    opt_g (Add_g  e1 e2)  =  case (opt_g e1, opt_g e2) of
        (Lit_g 0,  e2p      ) -> e2p
        (e1p,      Lit_g 0  ) -> e1p
        (e1p,      e2p      ) -> Add_g e1p e2p
    opt_g (Ext_g  d x)    =  Ext_g d (getOpt_g d x)
\end{code}

\begin{code}
instance HasOpt_g d => Opt_g (Sub_g d) where
    opt_g (Sub_g      e1 e2)  =  case (opt_g e1, opt_g e2) of
        (e1p,  Lit_g 0  ) -> SubLoop_g e1p
        (e1p,  e2p      ) -> Sub_g e1p e2p
    opt_g (SubLoop_g  e)      =  SubLoop_g (opt_g e)
\end{code}

\begin{code}
instance (Typeable d, GDict (d (Neg_g d)), HasOpt_g d) =>
        Opt_g (Neg_g d) where
    opt_g  (Neg_g  (Add_g e1 e2))
        = NegLoop_g (Add_g (opt_g (neg_g e1)) (opt_g (neg_g e2)))
    opt_g  (Neg_g  (Ext_g d x))
        = case fromDynamic (toDyn (getOpt_g d x)) of
            Just (Neg_g e)  -> NegLoop_g e
            _               -> Neg_g (Ext_g d (getOpt_g d x))
    opt_g  (Neg_g      e)  = Neg_g (opt_g e)
    opt_g  (NegLoop_g  e)  = NegLoop_g (opt_g e)
\end{code}

\begin{code}
instance HasOpt_g d => Opt_g (Eq_g d) where
    opt_g (Eq_g      e1 e2)  =  Eq_g (opt_g e1) (opt_g e2)
    opt_g (EqLoop_g  e)      =  EqLoop_g (opt_g e)
\end{code}

\end{document}
