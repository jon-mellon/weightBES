\name{createWeightsList}
\alias{createWeightsList}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Internal targets formatting function
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
createWeightsList(targets, value = "Scotland")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{targets}{
%%     ~~Describe \code{targets} here~~
}
  \item{value}{
%%     ~~Describe \code{value} here~~
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
function (targets, value = "Scotland") 
{
    targets <- targets[!is.na(targets[, value]), ]
    createSubGroup <- function(table.choice) {
        subtarget <- targets[targets$table == table.choice, ]
        sub <- subtarget[, value]
        names(sub) <- subtarget$group
        return(sub)
    }
    all.tables <- unique(targets$table)
    overall.targets <- lapply(all.tables, createSubGroup)
    names(overall.targets) <- all.tables
    return(overall.targets)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
