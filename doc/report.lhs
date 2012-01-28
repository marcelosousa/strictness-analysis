\documentclass[a4paper,11pt]{article}
\usepackage[english]{babel}
\usepackage[sort&compress]{natbib}
\usepackage{color , hyperref , tikz} 
\usepackage{verbatim , parskip, fullpage}
\usepackage{amsmath}
\usepackage{alltt}

\definecolor{syntax}{RGB}{0, 0, 0}
\definecolor{datatype}{RGB}{196, 6, 11}
\definecolor{class}{RGB}{168,37,39}
\definecolor{fieldname}{RGB}{0,0,162}
\definecolor{prelude}{RGB}{64,80,117}
\definecolor{numeral}{RGB}{0,0,205}
\definecolor{infixoperator}{RGB}{19, 19, 168}
\definecolor{constructor}{RGB}{196, 6, 11}
\definecolor{keyword}{RGB}{4, 58, 252}
\definecolor{special1}{RGB}{159,138,0}
\definecolor{string}{RGB}{3, 106, 7}
\definecolor{char}  {RGB}{3, 106, 7}

\newcommand{\hKeyword}[1]{\textbf{\color{steelblue}{#1}}}
\newcommand{\hChar}[1]{\textbf{#1}}
\newcommand{\hString}[1]{\textbf{#1}}
\newcommand{\hNumeral}[1]{\textbf{#1}}

%include lhs2TeX.fmt
%format β = "\beta"
%format β2 = "\beta2"
%format α  = "\alpha"
%format ⊑  = "\sqsubseteq"
%format ... = "\ \dots\ "
%format if  = "\hKeyword{if}"
%format then   = "\hKeyword{then}"
%format else   = "\hKeyword{else}"
%format endif  = "\hKeyword{endif}"
%format nfun   = "\hKeyword{nfun}"
%format fun    = "\hKeyword{fun}"
%format let    = "\hKeyword{let}"
%format in    = "\hKeyword{in}"
%format end    = "\hKeyword{end}"

%subst char a    	= "\color{char}\text{\tt ''" a "''}"
%subst string a  	= "\color{string}\text{\tt \char34 " a "\char34}"
%subst numeral a =  "\color{numeral}{ " a " }"



\def\mytitle{Dead-code elimination for a functional language}

\def\myauthor{Thijs Alkemade, Alessandro Vermeulen, Marcelo Barbosa de Sousa,\\ Utrecht University, 2011.}

\title{\mytitle}
\author{\myauthor}


\frenchspacing
\definecolor{steelblue}{RGB}{3,46,135}
\hypersetup{pdftitle=\mytitle,pdfauthor=\myauthor,linkcolor=steelblue}
\hypersetup{colorlinks=true}


\begin{document}
  
\maketitle

\begin{abstract}
  Dead code elimination is an important part of optimizing a program, as a lot
  of analyses and optimizations can benefit of having a smaller input. We have
  implemented dead code analysis for a simple functional language, and we have
  investigated how it relates to strictness analysis in that language.
\end{abstract}

\section{Introduction}
  Initially we set out to do strictness analysis and tried a Hindley-Milner
  like approach. However, after reading the assigned papers we chose to use
  Stefan Holdermans' one-pass algorithm for live variable analysis instead.
  The algorithm deals with a small functional language and instead of adapting
  it to do strictness analysis we chose to extend it with several extras
  instead, such as binary operators, |if ... then ... else ...
  endif|-statements, and recursive functions of the form |nfun f x => ...|.
  These are similar to |fun| but they get a name. We tried adding the
  recursive functionality to the |let|-statement but we couldn't get it to
  work properly. The limitation to this is that we can only have recursive
  functions with one parameter.

\section{The Language}
The language is a functional language based on
\cite{Holdermans:2010:RMT:1868281.1868283,Holdermans:2010:MSM:1706356.1706379}
and contains the following constructs:

\begin{itemize}
  \item Numbers
  \item Booleans
  \item Operators (|==|, |+|, |-|, |*|, |`div`|, |`mod`|, |<|, and |>|)
  \item |let ... = ... in ... end|\\
        A non-recursive-let-statement
  \item |if ... then ... else ... endif|\\
        The usual if-then-else-statement
  \item |fun  x    => ...|\\
        A lambda abstraction
  \item |nfun f x  => ...|\\ 
        A named lambda abstraction used for recursion.
\end{itemize}

  Booleans, operators, named lambda abstraction and the conditional branching
  are our additions.
  
\section{The `Holdermans'-algorithm}

  The algorithm essentially works in the same way as the
  $\mathcal{W}$-algorithm by Damas-Milner. The types of expressions are
  calculated in exactly the same way, but in addition annotations are guessed,
  and constraints are generated and solved. These constraints essentially keep
  a record of where the annotations came from so as not to lose information.
  $\mathcal{R}$ is the reconstruction algorithm that is analogous to
  $\mathcal{W}$. The unification algorithm $\mathcal{U}$ works in same way but
  also takes the annotation variables into account. Next to this, the
  generated constraints from $\mathcal{R}$ are `solved' by $\mathcal{S}$ in
  the |let|-case and as a final step for the top level of the program. The
  $\mathcal{S}$ (worklist-)algorithm solves the constraints by finding all
  constraints that influence the free annotation variables in a given
  constraint set.

  This ensures that a term with an annotation $\beta$ and an associated
  constraint set $C$ is considered dead if $\neg \exists \beta' : (\beta'
  \sqsubseteq \beta) \in C$.
  
  The implementation of the algorithm can be found in the directory
  `src/Analysis/Inferencing/HTm/'. The file `HH.hs' contains the algorithms
  itself, where `HHLib.hs' contains helper function, and `HHTy.hs' contains
  the additional types used for the inferencing, and `HHSubs.hs' contains a
  type class to make applying substitutions easier.

\section{Changes from the original algorithm}
   As mentioned before we have received the basic one-pass algorithm for
   determining the minimal principal types from Stefan Holdermans. When
   implementing the algorithm we have made a few changes to the algorithm.
   Next to the inevitable addition of support for our AST.


  \begin{enumerate}
    \item In the worklist algorithm we have added the statement
          |infl[β2] := {}| and lookups are replaced by |findByDefault| where
          the default is the empty set.
    \item To ensure that the whole program is considered live, we insert |α|
          into the active variables, and also add the constraint |α ⊑ α| to
          ensure that trivial programs (such as ``|5|'') work.
  \end{enumerate}

\section{The program explained}
  In principle liveness analysis and strictness analysis are closely
  related. A term is considered live if there is at least one path in
  which the term is used. A term is considered strict if it is used in
  every path. By default, it is assumed a term is dead, so it can be
  optimized away. However, for strictness analysis an argument is assumed
  strict, so the application can be replaced by a strict application. See
  Figure \ref{fig:live} and Figure \ref{fig:strict} for the lattices used.

  It is possible to combine the analyses to the lattice in Figure
  \ref{fig:strictlive}. Note that $\perp$ here means a term that is both dead
  and strict, which means it is both never used and always used, so this is
  impossible to occur in the result. We have not implemented or further
  investigated this.
  
  \tikzstyle{state}=[]
  \tikzstyle{mainedge}=[<-,thick]

  In our initial implementation there was no difference between liveness and
  strictness analysis: if something was live, it was also necessarily strict.
  This was because the algorithm never needed to join two alternatives as we
  had no |if|-statements or similar constructs. This meant that every term was
  either live and strict or dead and not strict, and all the operations we
  supported only produced live and strict or dead and non-strict terms.

  |if|-statements are different, though, as these have the following
  signature:
  \begin{align*}
    (L, S) \to (L, N) \to (L, N) \to\text{ ?}
  \end{align*}
  (The signature of the result depends only on where this statement is used,
  not on the statement itself.)

  So we made the choice to implement |if|-statements, and not rewrite it to do
  strictness analysis instead of liveness analysis, as liveness analysis
  without adding |if|-statements would be rather uninteresting.

  \begin{figure}
    \begin{center}
    \begin{tikzpicture}[]      
      \node[state] (D) at (0,0) {Dead}
        ;
      \node[state] (L) at (0,2) {Live}
        edge[mainedge] (D);
    \end{tikzpicture}
    \end{center}
    \caption{The lattice of liveness analysis.} \label{fig:live}
  \end{figure}
  
  \begin{figure}
    \begin{center}
    \begin{tikzpicture}[]      
      \node[state] (D) at (0,0) {Strict}
        ;
      \node[state] (L) at (0,2) {Not Strict}
        edge[mainedge] (D);
    \end{tikzpicture}
    \end{center}
    \caption{The lattice of strictness analysis.} \label{fig:strict}
  \end{figure}

  \begin{figure}[h]
    \begin{center}
    \begin{tikzpicture}[]      
      \node[state] (Bot) at (0,0) {$\perp$}
        ;
      \node[state] (DNS) at (-3,2) {Dead, Not Strict}
        edge[mainedge] (Bot);
      \node[state] (LS) at (3,2) {Live, Strict}
        edge[mainedge] (Bot);
      \node[state] (LNS) at (0,4) {Live, Not Strict}
        edge[mainedge] (LS)
        edge[mainedge] (DNS);
    \end{tikzpicture}
    \end{center}
    \caption{The lattice of combined strictness and liveness analysis.}
    \label{fig:strictlive}
  \end{figure}

\section{Usage}
  Our program depends on the standard packages {\tt base}, {\tt containers}
  (for |Data.Set| etc.), {\tt mtl} (for our |State| monad), {\tt cmdargs} (to
  make it easier to pass arguments to the executable) and the Compiler
  Construction package {\tt cco}\footnote{Which can be downloaded from \url{
  http://www.cs.uu.nl/wiki/bin/view/Cco/CourseResources\#CCO\_Library}}, from
  which we use the pretty printing module to print the resulting program.
  
  After {\tt cabal build}, our program can be run with:
  \begin{verbatim}
./dist/build/strictness/strictness input.fun
  \end{verbatim}
  This will run the algorithm on {\tt input.fun}, replace all dead code with
  |undefined| (here standing for {\tt undefined}, not to be confused with
  bottom in the lattice) and pretty print the result. Next, it will take the
  output and run the algorithm on it again, and compare the result. Applying
  the algorithm a second time should not change the result. We used this check
  to verify that we found the minimal solution given the constraints we can
  find.

  We also evaluate the program and the transformed program, to again compare
  the result to make sure our solution is sound.

  In case you were wondering where `HH' comes from in the naming, it stands
  for `Holdermans and Hage'. This is before we heard that Jurriaan had not
  seen this algorithm before.

\section{Test cases}
  The sample functions to test our program with are residing in the `test'
  directory. One can call the main function as shown in Figure
  \ref{fig:sample-run} and Figure \ref{fig:const} or by invoking the command
  above with one of the files in the `test' directory.

\begin{figure}
{\small
\begin{alltt}
  *Main> :main "../test/fib.fun" 
  Program: let name: fib = (\(\mu\)fib.\(\lambda\)x -> if (x == 0) then 0 else if (x == 1) then 
            1 else ((fib (x - 1)) + (fib (x - 2))) endif endif) in (fib 10) end
  Pretty printed term:
  let name fib =
      nfun fib x =>
                 if x == 0 then
                   0
                 else
                   if x == 1 then
                     1
                   else
                     (fib (x - 1)) + (fib (x - 2))
                   endif
                 endif
  in fib 10
  end
  Pretty printed term after applying transformation twice (should be the same):
  let name fib =
      nfun fib x =>
                 if x == 0 then
                   0
                 else
                   if x == 1 then
                     1
                   else
                     (fib (x - 1)) + (fib (x - 2))
                   endif
                 endif
  in fib 10
  end
  Equal, yay!
  Eval original: 55
  Eval transformed: 55
  Equal eval, yay!
\end{alltt}
}
\caption{A sample output from running the program on the Fibonacci
test.}\label{fig:sample-run}
\end{figure}

\begin{figure}
  \begin{alltt}
    *Main> :main "../test/const.fun" 
    Program: let name: const = (\(\lambda\)x -> (\(\lambda\)y -> x)) in ((const 1) 2) end
    Pretty printed term:
    let name const =
        fun x =>
              fun y =>
                    x
    in const 1 _||_
    end
    Pretty printed term after applying transformation twice (should be the same):
    let name const =
        fun x =>
              fun y =>
                    x
    in const 1 _||_
    end
    Equal, yay!
    Eval original: 1
    Eval transformed: 1
    Equal eval, yay!
  \end{alltt}
  \caption{Sample a program using |const|.}\label{fig:const}
\end{figure}

\section{Conclusion}
  We have implemented liveness analysis using the algorithm by Stefan
  Holdermans in a simple functional language. While strictness was the
  complement of liveness in the original language, we have introduced
  |if|-statements to avoid this and make the analysis more interesting.

\bibliographystyle{plainnat}
\bibliography{references}


\end{document}