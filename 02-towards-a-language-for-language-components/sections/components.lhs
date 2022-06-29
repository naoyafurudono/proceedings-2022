\section{Defining Reusable Language Components in \strachey{}}
\label{sec:components}

In this section, we demonstrate how to use the features of \strachey{}
introduced in the previous section to define reusable language components. We
work towards defining a reusable component for function abstraction, which can
be composed with other constructs, and for which we can define alternative
implementations. As an example, we will show that we can use the same component
defining functions with both a call-by-value and call-by-name strategy.

\subsection{A Signature for Reusable Components}

The first step is to define an appropriate module signature. We follow the same
setup as for the |Eval| signature in
\cref{sec:composable-types-and-functions}. That is, we declare an extensible
sort |Expr|, together with an algebra |eval| that consumes values of type
|Expr|. The result of evaluation is a |Value|, with potential side effects
|e|. The side effects are still abstract in the signature definition. The effect
variable |e| is universally quantified, but instantiations of the signature
|Eval| can impose additional constraints depending on which effects they need to
implement |eval|. As a result, when invoking |eval|, |e| can be instantiated
with any effect row that satisfies all the constraints imposed by the instances
of |Eval| that are in scope.

\begin{code}
signature Eval where
  sort  Expr  : Set
  alg   eval  : Expr -> { [e] Value } 
end 
\end{code}

\noindent
We will consider the precise definition of |Value| later in
\cref{sec:functions-as-effect}. For now, it is enough to know that it has a
constructor |Num : Int -> Value| that constructs a value from an integer
literal.

\subsection{A Language Component for Arithmetic Expressions}

Let us start by defining instances of the |Eval| signature for the expression
language from the introduction. First, we define a module for integer literals. 

\begin{code}
module Lit : Eval where
  cons  Lit : Int -> Expr
  case  eval (Lit n) = { Num n } 
end
\end{code}

\noindent
The corresponding clause for |eval| simply returns the value |n| stored inside
the constructor. Because the interpreter expects that we return a suspended
computation, we must wrap |n| in a suspension, even though it is a pure
value. Enacting this suspension, however, does not trigger any side effects, and
as such importing |Lit| imposes no constraints on the effect row |e|.

Next, we define a module |Div| that implements integer division. 

\begin{code}
module Div : Eval where
  cons  Div : Expr -> Expr -> Expr
  case  eval (Div m1 m2) = { safeDiv m1!!! m2!!! }      
\end{code}

\noindent
Looking at the implementation of |eval| in the module |Div| we notice two
things. First, the recursive arguments to |Div| have been replaced by the result
of calling |eval| on them, meaning that |m1| and |m2| are now
\emph{computations} with type |{ [e] Int }|, and hence we must use enactment
before we can pass the result to |safeDiv|. Enacting these computations may
trigger side effects, so the order in which sub-expressions are evaluated
determines in which order these side effects occur in the case that expressions
contain more than one enactment. Sub-expressions in \strachey{} are evaluated
from left to right. Second, the implementation uses the function |safeDiv|,
defined in \cref{sec:effects-and-handlers}, which guards against errors
resulting from division by zero.

The function |safeDiv| is annotated with the |Fail| effect, which supplies the
|fail| operation. By invoking |safeDiv| in the defintion of |eval|, which from
the definition of |Eval| has type |Expr -> { [e] Int }|, we are imposing a
constraint on the effect row |e| that it contains at least the |Fail| effect. In
other words, whenever we import the module |Div| we have to make sure that we
instantiate |e| with a row that has |Fail| in it. Consequently, before we can
extract a value from any instantiation of |Eval| that includes |Div|, we must
apply a handler for the |Fail| effect.

Since the interpreter now returns a |Value| instead of an |Int|, we must modify
|safeDiv| accordingly. In practice this means that we must check if its
arguments are constructed using the |Num| constructor before further processing
the input. Since |safeDiv| already has |Fail| as a side effect, we can invoke
the |fail| operation in case an argument was constructed using a different
constructor than |Num|.

\subsection{Implementing Functions as a Reusable Effect}
\label{sec:functions-as-effect}

\strachey{}'s effect system can describe much more sophisticated effects than
|Fail|. The effect system permits fine-grained control over the semantics of
operations that affect a program's control flow, even in the presence of other
effects. To illustrate its expressiveness, we will now consider how to define
function abstraction as a reusable effect, and implement two different handlers
for this effect corresponding to a call-by-value and call-by-name
semantics. Implementing function abstraction as an effect is especially
challenging since execution of the function body is deferred until the function
is applied. From a handler's perspective, this means that the function body and
its side effect have to be postponed until a point beyond its own control, a
pattern that is very difficult to capture using traditional algebraic effects.

We will see shortly how \strachey{} addresses this challenge. A key part of the
solution is the ability to define \emph{higher-order operations}: operations
with arguments that are themselves effectful computations, leaving it up to the
operation's handler to enact the side effects of higher-order arguments. The
|Fun| effect, which implements function abstraction, has several higher-order
operations.

\noindent
\begin{code}
effect Fun where
|  lam    : String  -> { [Fun] Value }  -> { [Fun] Value }
|  app    : Value   -> Value            -> { [Fun] Value }
|  var    : String                      -> { [Fun] Value }
|  thunk  : { [Fun] Value }             -> { [Fun] Value }
\end{code}

\noindent
The |Fun| effect defines four operations, three of which correspond to the usual
constructs of the $\lambda$-calculus. The |thunk| operation has no counterpart
in the $\lambda$-calculus, and postpones evaluation of a computation. It is
necessary for evaluation to support both a call-by-value and call-by-name
evaluation strategy

When looking at the |lam| and |thunk| operations, we find that they both have
parameters annotated with the |Fun| effect. This annotation indicates that they
are higher-order parameters. By allowing higher-order parameters to
operations, effects in \strachey{} do not correspond directly to algebraic
effects. Instead, to give as semantics to effects in \strachey{}, we must use a
flavor of effects that permits higher-order syntax, such as \emph{Latent
  Effects}~\cite{DBLP:conf/aplas/BergSPW21}.


As a result, any effects of the computations
stored in a closure or thunk are postponed, leaving it up to the handler to
decide when these take place.

\paragraph{Using the |Fun| effect}

To build a language with function abstractions that uses the |Fun| effect, we
give an instance of the |Eval| signature that defines the constructors |Abs|,
|App|, and |Var| for |Expr|. We extend |eval| for these constructors by mapping
onto the corresponding operation.

\noindent
\begin{code}
module Lambda : Eval where
  cons  Abs : String  -> Expr  -> Expr
  |     App : Expr    -> Expr  -> Expr
  |     Var : String           -> Expr
  
  case  eval (Abs x m)    = lam x m
  |     eval (App m1 m2)  = app m1!!! (thunk m2)!!!
  |     eval (Var x)      = var x 
end 
\end{code}

\noindent
Crucially, in the case for |Abs| we pass the effect-annotated body |m|, with
type |{ [e] Value }|, to the |lam| operation directly without extracting a pure
value first. This prevents any effects in the body of a lambda from being
enacted at the definition site, and instead leaves the decision of when these
effects should take place to the used handler for the |Fun| effect. Similarly,
in the case for |App|, we pass the function argument |m2| to the |thunk|
operation directly, postponing any side effects until we force the constructed
thunk. The precise moment at which we will force the thunk constructed for
function arguments will depend on whether we employ a call-by-value or
call-by-name strategy. We must, however, enact the side effects of evaluating
the function itself (i.e., |m1|), because the |app| operation expects its
arguments to be a pure value.

We implement call-by-value and call-by-name handlers for |Fun| in a new module,
which also defines the type of values, |Value|, for our language. To keep the
exposition simple, |Value| is not an extensible |sort|, but it is
possible to do so in \strachey{}.

Values in this language are either numbers (|Num|), function closures (|Clo|),
or thunked computations (|Thunk|). We define the type of values together in the
same module as the handler(s) for the |Fun| effect. This module is parameterized
over an effect row |e|, that denotes the \emph{remaining effects} that are left
after handling the |Fun| effect. In this case, |e| is a module parameter to
express that the remaining effects in the handlers that we will define coincide
with the effect annotations of the computations stored in the |Clo| and |Thunk|
constructors, allowing us to run these computations in the handler.

\begin{code}
module HLambda (e : Effects) where

  import Fun

  type Env    =  List (String * Value)
  data Value  =  Num Int
              |  Clo String (Env -> { [Fail|||e] Value }) Env        
              |  Thunk ({ [Fail|||e] Value })

  -- ... (handlers for the Fun effect) ... 
end 
\end{code}
 
\paragraph{Call-by-value} 

\begin{figure}[t!]
\begin{code}
handler hCBV  :   { [Fun|||e] Value }
              ->  Env -> { [Fail|||e] Value } where
|  (lam x f)                        nv  k  ↦  k (Clo x f nv) nv
|  (app (Clo x f nv') (mthunk))     nv  k  ↦  k (f ((x, texc) :: nv'))!!! nv
|  (app _ _)                        _   _  ↦  { fail!!! }
|  (var x)                          nv  k  ↦  k (lookup nv x)!!! nv
|  (thunk f)                        nv  k  ↦  k (Thunk {f nv}) nv 
|  return v                         nv     ↦  { v }
\end{code}
\hrule
\caption{A Handler for the |Fun| effect, implementing a call-by-value
  semantics for function arguments. The gray highlights indicate where thunks
  constructed for function arguments are forced.}
\label{fig:handler-cbv}
\end{figure}

We are now ready to define a handler for the |Fun| effect that implements
a call-by-value evaluation strategy. \cref{fig:handler-cbv} shows its
implementation.

The |return| case is unremarkable: we simply ignore the environment |nv| and
return the value |v|. The cases for |lam| and |thunk| are similar, as in both
cases we do not enact the side effects associated with the stored computation
|f|, but instead wrap this computation in a |Closure| or |Thunk| which is passed
to the continuation |k|. For variables, we resolve the identifier |x| in the
environment and pass the result to the continuation.

A call-by-value semantics arises from the implementation of the |app| case. The
highlights (e.g., |texc|) indicate where the thunk we constructed for the
function argument in |eval| is forced. In this case, we force this argument
thunk immediately when encountering a function application, meaning that any
side effects of the argument take place \emph{before} we evaluate the function
body.

\paragraph{Call-by-name} 

\begin{figure}[t]
\begin{code}
handler hCBN  :   { [Fun|||e] Value }
              ->  Env -> { [Fail|||e] Value } where
|  (lam x f)              nv  k  ↦  k (Clo x f nv) nv
|  (app (Clo x f nv') v)  nv  k  ↦  k (f ((x, v) :: nv'))!!! nv
|  (app _ _)              _   _  ↦  { fail!!! }
|  (var x)                nv  k  ↦  match (lookup x nv)!!! with
                                    |  (mthunk)   ↦  k texc nv 
                                    |  v          ↦  k v nv
				    end 
|  (thunk f)              nv  k  ↦  k (Thunk { f nv }) nv 
|  return v               nv     ↦  { v }
\end{code}
\hrule
\caption{A Handler for the |Fun| effect, implementing a call-by-name
  semantics for function arguments. The gray highlights indicate where thunks
  constructed for function arguments are forced.}
\label{fig:handler-cbn}
\end{figure}

The handler in \cref{fig:handler-cbn} shows an implementation of a call-by-name
semantics for the |Fun| effect. The only case that differ from the call-by-value
handler in \cref{fig:handler-cbv} are the |app| and |var| cases.

In the case for |app|, we now put the argument thunk in the environment
immediately, without forcing it first. Instead, in the case for |var|, we check
if the variable we look up in the environment is a |Thunk|. If so, we force it
and pass the resulting value to the continuation. In effect, this means that for
a variable that binds an effectful computation, the associated side effects take
place every time we use that variable, but not until we reference it for the
first time.

\subsection{Example Usage}

To illustrate how to use the reusable components defined in this section, and
the difference between the semantics implemented by |hCBV|
(\cref{fig:handler-cbv}) and |hCBN| (\cref{fig:handler-cbn}), we combine the
|Lambda| module with the modules for |Div| and |Lit|. \cref{fig:example} shows
the example.

When importing the modules |HLambdaCBV| and |HLambdaCBN|, we pass an explicit
effect row that corresponds to the effects that remain after handling the |Fun|
effect. Because we handle |Fun| after handling the |Fail| effect introduced by
|Div|, we pass the empty row.  To evaluate expressions, we have to invoke
|hFail| twice: first to handle the instance of the |Fail| effect introduced by
|eval| for the |Div| constructor, and later to handle the |Fail| instance
introduced by applying |hCBV|/|hCBN|. Consequently, the result of evaluating is
a nested |Maybe|, where the inner instance indicates errors resulting from
division by zero, and the outer instance errors thrown by the
handler. Evaluating |result1| and |result2| shows the difference between using
the call-by-value and call-by-name semantics for functions.

\begin{figure}[t]
\begin{code}
module Test where
  import  ◂  Prelude
          ,  Fun
          ,  Fail

          ,  HLambdaCBV []
          ,  HLambdaCBN []

          ,  Lambda
          ,  Lit
	  ,  Div 

  fun execCBV : Expr -> Maybe (Maybe Value) where   
  | e ↦ (hFail (hCBV (hFail (eval e)) []))!!!

  fun execCBN : Expr -> Maybe (Maybe Value) where
  | e ↦ (hFail (hCBN (hFail (eval e)) []))!!! 

  fun expr : Expr = App (Abs "x" (Lit 10)) (Div (Lit 5) (Lit 0))

  -- evaluates to Just Nothing
  fun result1 : Maybe (Maybe Value) = execCBV expr 

  -- evaluates to Just (Just (Num 10))
  fun result2 : Maybe (Maybe Value) = execCBN expr
end 
\end{code}
\hrule
\caption{Examples of different outcomes when using a call-by-value or
  call-by-name evaluation strategy.}
\label{fig:example}
\end{figure}
