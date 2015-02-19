% csuthesis class
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Designed by Leif Anderson to meet the bullshit
% requirements of the Colorado State University
% Graduate School for the formatting of theses
% and dissertations.  This document should comply
% with the requirements as of spring 2014, by the
% time it is completed.
% Much additional input provided by Dr. Dan Brake
% and... others to come

% Licensed under the CC-GNU GPL version 2.0 or later.
% See http://creativecommons.org/licenses/GPL/2.0

% Areas that you might need to copy to your own header,
% or maybe comment out, are marked with big lines like the
% following:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{csuthesis}[2014/06/02 Colorado State University Thesis class]


% This is basically going to be a class wrapper that takes
% amsbook and changes the formatting as little as possible
% to make it conform to  the dumber, uglier CSU standards.

% Class options:
\newif\ifphd \phdtrue
\newif\ifmasterofscience \masterofsciencetrue
\newif\ifsmallcapsok \smallcapsoktrue
\newif\ifcopyrightpage \copyrightpagefalse
\DeclareOption{masters}{\phdfalse}
\DeclareOption{nonscience}{\masterofsciencefalse}
\DeclareOption{masterof}{\phdfalse\masterofsciencefalse}
\DeclareOption{nosmallcaps}{\smallcapsokfalse}

\DeclareOption{copyrightpage}{\OptionNotUsed}
% (As of feb 2014 the copyright page is mandatory.)
\DeclareOption{twoside}{\OptionNotUsed}
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{amsbook}}
\ProcessOptions\relax
\LoadClass[12pt, oneside, reqno]{amsbook}

% User-specified global stuff (author, department, etc):
\makeatletter
	\global \let \@author \@empty 
	\global \let \@date \@empty 
	\global \let \@title \@empty
	\global \let \@departmentname \@empty
	\global \let \@gradterm \@empty
	\gdef\@gradyear{\the\year}
	\gdef\@masterof{Fine Arts}
	\global \let \@advisor \@empty
	
	\newcommand{\committee}[1]{\gdef\@committee{\xandlist{\\ \> }{\\ \>}{\\ \>}{#1}}}%
	\newcommand{\@committee}{\@latex@warning@no@line{No \noexpand\committee given}}%
	
	\def\author#1{\gdef\@author{#1}}%
	\def\@author{\@latex@warning@no@line{No \noexpand\author given}}
	\def\departmentname#1{\gdef\@departmentname{#1}}
	\def\gradterm#1{\gdef\@gradterm{#1}}
	\def\gradyear#1{\gdef\@gradyear{#1}}
	\def\masterof#1{\gdef\@masterof{#1}}
	\def\advisor#1{\gdef\@advisor{#1}}
	\def\coadvisor#1{\gdef\@coadvisor{#1}}
	\def\copyrighttext#1{\gdef\@copyrighttext{#1}}%no longer used
\makeatother

% sideways stuff:   %%I prefer landscape {KHM}
% \usepackage{pdflscape}
\usepackage[pdftex]{lscape}
\usepackage{float}
\usepackage{afterpage}
\usepackage{environ}
\makeatletter

\NewEnviron{sidewayspage}{
% This is still a little hack-ish.  It runs into problems
% with sideways content that extends past one page.
% For multiple page sideways stuff, consider \begin{landscape}
% and a fancyhdr hack to move all pagenumbers sideways.
% The main goal here is to build a sideways endvironment that
% is sort of a float.
	\def\tempreacharound{\clearpage\begin{landscape}\thispagestyle{empty}~\vfill}
	\expandafter\expandafter\expandafter\afterpage\expandafter\expandafter\expandafter{\expandafter\tempreacharound % DON'T EVEN ASK
	% the \BODY command loses its meaning if it is used inside another environment... sometimes.  Trust me, we need to do this.
	% More to the point, the whole afterpage thing makes this landscape page a floating page.
	% the \clearpage empties out the other floats.  Then \thispagestyle clears the page number from the old bottom
	\BODY
	% consider making the sidewayspage environment more of a table wrapper, with \captionof{table}{.... caption ...\label{table:sideways}}
	~\vfill\centerline{\footnotesize\thepage}%this puts the page number back in, but over on the side.
	%there could be problems here if the user changes page number sizes or something.  This one will not change automatically.
	\end{landscape}}
}

	\pagestyle{plain}
\makeatother


% See the ams manual thing, page 40 or so, for the beginning of the section, subsection, and table of contents formatting stuff.
\makeatletter
 \ifsmallcapsok
 	\renewcommand{\contentsname}{{\scshape Table of Contents}}
 	\renewcommand{\listfigurename}{{\scshape List of Figures}}
 	\renewcommand{\listtablename}{{\scshape List of Tables}}
 	\renewcommand{\indexname}{{\scshape Index}}
 \else
 	\renewcommand{\contentsname}{\MakeUppercase{Table of Contents}}
 	\renewcommand{\listfigurename}{\MakeUppercase{List of Figures}}
 	\renewcommand{\listtablename}{\MakeUppercase{List of Tables}}
 	\renewcommand{\indexname}{\MakeUppercase{Index}}
 \fi

\def\tableofcontents{%
   \begin{center}
   \contentsname
   \end{center}
   \@starttoc{toc}
 }
\def\listoftables{%
   \begin{center}
   \listtablename
   \end{center}
   \@starttoc{lot}
 }
 \def\listoffigures{%
   \begin{center}
   \listfigurename
   \end{center}
   \@starttoc{lof}
 }
\def\@makechapterhead#1{\global\topskip .625in\relax
  \begingroup
	\normalsize\normalfont\scshape\centering
    \ifnum\c@secnumdepth>\m@ne
      \leavevmode \hskip-\leftskip
      \rlap{\vbox to\z@{\vss
          \centerline{\normalsize\mdseries
              \uppercase\@xp{\chaptername}\enspace\thechapter}
          \vskip 3pc}}\hskip\leftskip\fi
     \Large#1\par \endgroup%added the \Large to make the chapter headings larger.  The word "chapter" is left alone, only the name of the chapter gets bigger.
  \skip@34\p@ \advance\skip@-\normalbaselineskip
  \vskip\skip@ }
% \def\@makeschapterhead#1{\global\topskip 7.5pc\relax
\def\@makeschapterhead#1{\global\topskip .625in\relax
  \begingroup
%   \fontsize{\@xivpt}{18}\bfseries\centering %from the rulebook: Do not use bold for preliminary page headings.
% added scshape here:
  \normalsize\normalfont\scshape\centering  
  #1\par \endgroup
  \skip@34\p@ \advance\skip@-\normalbaselineskip
  \vskip\skip@ }
\def\@makescsuhead#1{\global\topskip 0pc\relax
  \begingroup
  \normalsize\normalfont\centering% this would be the "correct" spot to add smallcaps formatting, wouldn't it?  
%   #1\par~\\ \endgroup%add/sutract space here, probably.
	\ifsmallcapsok
		\scshape #1\par \endgroup
	\else
		\MakeUppercase{#1}\par \endgroup
	\fi
  \skip@34\p@ \advance\skip@-\normalbaselineskip
  \vskip\skip@ }%this def is used just to make sure the table of contents name does not get bold faced or moved around
\def\@starttoc#1#2{%
  \begingroup
  \setTrue{#1}%
  \let\secdef\@gobbletwo \chapter
  \let\@secnumber\@empty % for \@tocwrite and \chaptermark
  \ifx\contentsname#2%
  \else \@tocwrite{chapter}{#2}\fi
  \typeout{#2}\@xp\chaptermark\@xp{#2}%
	\@makescsuhead{#2}\@afterheading
  \parskip\z@skip
  \makeatletter
  \@input{\jobname.#1}%
  \if@filesw
    \@xp\newwrite\csname tf@#1\endcsname
    \immediate\@xp\openout\csname tf@#1\endcsname \jobname.#1\relax
  \fi
  \global\@nobreakfalse \endgroup
  \newpage
}

\def\section{\@startsection{section}{1}%
  \z@{.7\linespacing\@plus\linespacing}{.5\linespacing}%
  {\normalfont\scshape\centering}} %writes the section headings in smallcaps, centered.

\def\subsection{\@startsection{subsection}{2}%
	\z@{\linespacing}{0.1\linespacing}%
%  \normalparindent{.5\linespacing\@plus.7\linespacing}{-.5em}%
  {\normalfont\itshape}}%writes the subsection headings in smallcaps.

\def\subsubsection{\@startsection{subsubsection}{3}%
  \normalparindent\z@{-.5em}%
  {\normalfont\itshape}}%sub sub sections are italics.

% They should prevent three-digit figure and table numbers
% from running over the name of the fig/tab in the T.o.C.
\def\l@figure{\@tocline{0}{3pt plus2pt}{0pt}{2.3pc}{}}
\let\l@table=\l@figure

% This batch of commands makes the chapter number show up within several of the other numbers.
\numberwithin{table}{chapter}
\numberwithin{figure}{chapter}
\numberwithin{section}{chapter}
\makeatother

%dotted line for TOC.  This is the definition of \@tocline, copied out of amsbook.
\makeatletter
\def\@tocline#1#2#3#4#5#6#7{\relax
  \ifnum #1>\c@tocdepth % then omit
  \else
    \par \addpenalty\@secpenalty\addvspace{#2}%
    \begingroup \hyphenpenalty\@M
    \@ifempty{#4}{%
      \@tempdima\csname r@tocindent\number#1\endcsname\relax
    }{%
      \@tempdima#4\relax
    }%
    \parindent\z@ \leftskip#3\relax \advance\leftskip\@tempdima\relax
    \rightskip\@pnumwidth plus4em \parfillskip-\@pnumwidth
    #5\leavevmode\hskip-\@tempdima #6\nobreak\relax
%     \hfil\hbox to\@pnumwidth{\@tocpagenum{#7}}\par % this was the original line.
    \dotfill\hbox to\@pnumwidth{\@tocpagenum{#7}}\par % the dotfill makes a dotted line across to the page number.
    \nobreak
    \endgroup
  \fi}
\makeatother

%%%%%%%%%%%%%
%   title   %
%%%%%%%%%%%%%

%required stuff:
% 1 inch margins all sides, no bold, no titles on committee
% centered:
% Top line: 
% DISSERTATION (all caps)
% some space
% TITLE IN ALL CAPS AND DOUBLESPACED \MakeUppercase{} will be useful.
% some space
% Submitted by
% blank line
% \@author
% blank line
% \@departmentname
% some space
% In partial fulfullment of the requirements 
% blank line
% For the degree of Doctor of Philosophy
% blank line
% Colorado State University
% blank line
% Fort Collins, Colorado
% blank line
% \@gradterm
% end center
% left align:
% Doctoral Committee:
% blank line
% Indent, still left:
% Advisor: \@advisor
% Co-Advisor: \@coadvisor (if the coadvisor exists)
% blank line
% committee member list (no titles attached anywhere)
% NO PAGE NUMBER ON THIS PAGE but it is page 1 of frontmatter (so page i, unlabeled)
% That last part is a little silly, since it is immediately followed
% by an unlabeled AND uncounted copyright page.  Better to just 
% specify that the abstract is page ii.

% As a leadin to the title page, here is the Abstract 
% definition, copied from AMSbook and modified:
\makeatletter
\renewenvironment{abstract}{%
	\ifx\maketitle\relax
		\ClassWarning{\@classname}{Abstract should precede
			\protect\maketitle\space in CSU Thesis document class; reported}%
	\fi
	\global\setbox\abstractbox=\vtop \bgroup
		\list{}{\labelwidth\z@
			\leftmargin0pc \rightmargin\leftmargin
			\listparindent\normalparindent \itemindent\z@
			\parsep\z@
			\itemsep0.5\baselineskip
			% In order to get equation numbers indented with the rest of the abstract, we have to do this:
			\let\fullwidthdisplay\relax
		}%
	\ifsmallcapsok
		\item \begin{center}\scshape Abstract\end{center}
		\item \begin{center}\scshape \@title\end{center}
% 		\addcontentsline{toc}{chapter}{{\scshape Abstract}}
	\else
		\item \begin{center}\MakeUppercase{Abstract}\end{center}
		\item \begin{center}\MakeUppercase{\@title}\end{center}
% 		\addcontentsline{toc}{chapter}{\MakeUppercase{Abstract}}
	\fi
	\addcontentsline{toc}{chapter}{Abstract}
	\item[\hskip\normalparindent]
}{%
\endlist\egroup

\ifx\@setabstract\relax \@setabstracta \fi
}
% AMS comment about abstract:
% % Because the abstract might be postponed until the end of an article, we can-not simply use the fact of a preceding \maketitle to tell whether \endabstract should immediately put the abstract box contents on the page. So we use an auxiliary function that will be reset to no-op once we have passed the point where the abstract should normally be typeset.

\newbox\ackbox
\newenvironment{acknowledgements}{%
  \ifx\maketitle\relax
    \ClassWarning{\@classname}{Acknowledgements should precede
      \protect\maketitle\space in the csuthesis class; reported}%
  \fi
  \global\setbox\ackbox=\vtop \bgroup
	\list{}{\labelwidth\z@
			\leftmargin0pc \rightmargin\leftmargin
			\listparindent\normalparindent \itemindent\z@
			\parsep\z@
			\itemsep0.5\baselineskip
			% In order to get equation numbers indented with the rest of the abstract, we have to do this:
			\let\fullwidthdisplay\relax
		}%
	\ifsmallcapsok
		\item \begin{center}\scshape Acknowledgements\end{center}
% 		\addcontentsline{toc}{chapter}{{\scshape Acknowledgements}}
	\else
		\item \begin{center}\MakeUppercase{Acknowledgements}\end{center}
% 		\addcontentsline{toc}{chapter}{\MakeUppercase{Acknowledgements}}
	\fi
	\addcontentsline{toc}{chapter}{Acknowledgements}
	\item[\hskip\normalparindent]	
}{%
	% Old version of the 'easter egg':
	% 	Great thanks are also due to Leif Anderson for building this \LaTeX\ document class, allowing me to meet the graduate school formatting requirements with no effort on my part. % You're welcome.
	
%	This \ifphd dissertation \else thesis \fi is typset in \LaTeX\ using a document class designed by Leif Anderson.%The more professional version.
	% Go ahead, comment it out.  But if you're here you probably know how much work went into this.
	% I call this my little "no need to thank me" easter egg.
  \endlist\egroup
  \ifx\@setack\relax \@setacka \fi
}
\def\@setack{\@setacka \global\let\@setack\relax}
\def\@setacka{%
  \ifvoid\ackbox
  \else
    \skip@20\p@ \advance\skip@-\lastskip
    \advance\skip@-\baselineskip \vskip\skip@
    \box\ackbox
    \prevdepth\z@ % because \abstractbox is a vtop (I have no idea what that means.)
  \fi
}
% Now we overwrite \maketitle
% the original had a lot more error handling, which I have ignored here.
% The original was also a lot more elegant.  Begin hack and slash.
% Our \maketitle will be huge, and will do everything needed at the beginning: title page, abstract, acknowledgements, copyright, etc.
\renewcommand{\maketitle}{%
	\begin{titlepage}
		\begin{center}
			\ifphd
				\ifsmallcapsok
					{\scshape Dissertation}
				\else
					\MakeUppercase{Dissertation}
				\fi
			\else
				\ifsmallcapsok
					{\scshape Thesis}
				\else
					\MakeUppercase{Thesis}
				\fi
			\fi
			~\vfill
			\ifsmallcapsok
				{\scshape \@title}
			\else
				\MakeUppercase{\@title}
			\fi
			~\vfill
			Submitted by \\
			\ifx @author \@empty
				\relax% Look, error handling!
			\else
				\@author \\
			\fi

			\ifx\@empty\@departmentname
				\emph{Attention!} $\backslash$departmentname \emph{not defined!}
			\else
				\@departmentname
			\fi

			~\vfill
			In partial fulfillment of the requirements \\
			\ifphd
				For the Degree of Doctor of Philosophy \\ 
			\else
				\ifmasterofscience
					For the Degree of Master of Science \\
				\else
					For the Degree of Master of \@masterof \\
				\fi
			\fi
			Colorado State University \\
			Fort Collins, Colorado \\
			\ifx\@gradterm\@empty
				\emph{Attention!} $\backslash$gradterm \emph{not defined!}
			\else
				\@gradterm~\@gradyear
			\fi
			~\vfill
		\end{center}
		% left align:
		\begin{singlespace}
		\ifphd
			Doctoral Committee:\\
		\else
			Master's Committee:\\
		\fi
		
		\begin{tabbing}
		\hspace*{0.5in}\=
		Advisor: \@advisor \\ \>
		\ifx\@coadvisor\undefined
			\relax
		\else
			Co-Advisor: \@coadvisor \\
		\fi
		~\\ \> 
		\@committee
		\end{tabbing}
		\end{singlespace}
	\end{titlepage}

		\thispagestyle{empty}
		\vspace*{\fill}
		\begin{center}

		Copyright by \@author~\@gradyear \\ All Rights Reserved
.
		\end{center}
		\vspace*{\fill}
		\clearpage

	\setcounter{page}{2}
	
	\unvbox\abstractbox

	\ifvoid\ackbox

	\else
		\clearpage
		\unvbox\ackbox

	\fi
}


\newcommand{\preliminarypage}[2]{
	\clearpage
	\list{}{\labelwidth\z@
			\leftmargin0pc \rightmargin\leftmargin
			\listparindent\normalparindent \itemindent\z@
			\parsep\z@
			\itemsep0.5\baselineskip
			\let\fullwidthdisplay\relax
		}%
	\ifsmallcapsok
		\item \begin{center}\scshape #1\end{center}
	\else
		\item \begin{center}\MakeUppercase{#1}\end{center}
	\fi
	\addcontentsline{toc}{chapter}{#1}
	\item[\hskip0pt]#2
	\clearpage}

\makeatother