\name{createWeights}
\alias{createWeights}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
createWeights(current.targets, bes)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{current.targets}{
%%     ~~Describe \code{current.targets} here~~
}
  \item{bes}{
%%     ~~Describe \code{bes} here~~
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (current.targets, bes) 
{
    not.there <- names(current.targets)[!names(current.targets) \%in\% 
        names(bes)]
    if (length(not.there) > 0) {
        stop(paste("Var not present in BES", not.there, collapse = ""))
    }
    vars <- bes[complete.cases(bes[, names(current.targets)]), 
        ]
    if (any(!sapply(vars[, names(current.targets)], is.factor))) {
        stop("Following vars are not factor:", names(which(!sapply(vars[, 
            names(current.targets)], is.factor))))
    }
    testVar <- function(name) {
        if (all(sort(levels(vars[[name]])) == sort(names(current.targets[[name]])))) {
            return(NULL)
        }
        else {
            stop(paste0("Failed on ", name))
        }
    }
    sapply(names(current.targets), testVar)
    library(anesrake)
    output <- anesrake(inputter = current.targets, dataframe = vars, 
        caseid = vars$id, verbose = TRUE, cap = 15, choosemethod = "total", 
        type = "nolim", pctlim = 0.005, nlim = 5, iterate = TRUE, 
        force1 = TRUE)
    vars$test_weight <- output$weightvec
    vars <- vars[, c("id", "test_weight", "region.weighted")]
    return(vars)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
