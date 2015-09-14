\name{createCustomWeights}
\alias{createCustomWeights}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Main function for creating custom weights for BESIP
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
createCustomWeights(bes, waves = 1:6, targets)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bes}{
%%     ~~Describe \code{bes} here~~
}
  \item{waves}{
%%     ~~Describe \code{waves} here~~
}
  \item{targets}{
%%     ~~Describe \code{targets} here~~
}
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
library(weightBES)

data(bes.weight.vars)
data(targets)

weights.w1.w2.W3 <- createCustomWeights(bes = bes.weight.vars, waves = c(1,2, 3),
                                        targets = targets)
weights.w1.w4 <- createCustomWeights(bes.weight.vars, waves = c(1,4), 
                                     targets = targets)
weights.w6 <- createCustomWeights(bes.weight.vars, waves = c(6), 
                                     targets = targets)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~BES }
\keyword{ ~Weighting }% __ONLY ONE__ keyword per line
\keyword{ ~Raking }% __ONLY ONE__ keyword per line
