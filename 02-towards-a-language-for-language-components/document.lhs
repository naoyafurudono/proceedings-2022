\documentclass[runningheads]{llncs}

\title{Towards a Language for Defining Reusable Programming Language Components}
\subtitle{(Project Paper)}

\usepackage{mathpartir}
\usepackage{boxedminipage}
\usepackage{amsmath}
\usepackage{amssymb}
%usepackage{stmaryrd}
\usepackage{semantic}
\usepackage{xcolor}
\usepackage{newunicodechar}
\usepackage[hidelinks]{hyperref}
\usepackage[capitalise,noabbrev]{cleveref}

\input{unicode.tex}

% Add page numbers
% \pagestyle{plain}

\bibliographystyle{splncs04}


%include polycode.fmt
%include fmts/strachey.fmt
%include fmts/colors.fmt

% fixes the size difference between in-text code and code in figures. 
\renewcommand{\hscodestyle}{\normalsize}
\renewcommand{\figurename}{Figure}

\newcommand{\strachey}{\textsc{CS}}
\newcommand{\todo}[1]{{\color{red}\textbf{TODO:} #1}}

\author{Cas van der Rest\orcidID{0000-0002-0059-5353} \and Casper Bach Poulsen\orcidID{0000-0003-0622-7639}}
\institute{\email{c.r.vanderrest@@tudelft.nl} \ \ \ \ 
  \email{c.b.poulsen@@tudelft.nl}
 \\  Delft University of Technology, Delft, The Netherlands}

% \author{Anonymous Authors}

\titlerunning{Towards a Language for Defining Resuable Language Components}

\begin{document}

\maketitle

\begin{abstract}

  Developing programming languages is a difficult task that requires a lot of
  time, effort, and expertise. Reusable programming language components make
  this task easier, by allowing language designers to grab off-the-shelf
  components for common language features. Modern functional programming
  languages, however, lack support for reuse of definitions, and thus language
  components defined using algebraic data types and pattern matching functions
  cannot be reused without modifying or copying existing code. To improve the
  situation, we introduce \strachey{}, a functional meta-language for developing
  reusable programming language components, that features built-in support for
  extensible data types and functions, as well as effects and handlers. In
  \strachey{}, we can define language components using algebraic data types and
  pattern matching functions, such that we can compose language components into
  larger languages and define new interpretations for existing components
  without modifying existing definitions.
  
\end{abstract}


%include sections/introduction.lhs
%include sections/showcase.lhs
%include sections/components.lhs
%include sections/related.lhs
%include sections/future.lhs

\subsubsection*{Acknowledgements.}

We would like to thank Peter Mosses, for his valuable advice during several
discussions about this work. Furthermore, we thank the anonymous reviewers for
their reviews and helpful feedback.

\bibliography{references}

\end{document}
