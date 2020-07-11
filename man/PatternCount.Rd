\name{PatternCount}
\alias{PatternCount}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
PatternCount
}
\description{
PatternCount is a package to count the number of times a 'Pattern' occurs in a 'Text'. It is developed for use in genomics to identify the number of times a k-mer ('Pattern') appears in a length of DNA sequence ('Text') 
}
\usage{
PatternCount(Text, Pattern)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Text}{A character vector containing one string}
  \item{Pattern}{A character vector containing one small string. Its length should be shorter than text}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
text <- c("ACGTGTACGTGT")
pattern <- c("GTGT")

PatternCount(Text=text,Pattern=pattern)
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
}
