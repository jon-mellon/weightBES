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
createCustomWeights(bes, waves = c(1, 2, 3, 4, 5, 6), targets)
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (bes, waves = c(1, 2, 3, 4, 5, 6), targets) 
{
    colnames(bes)[grepl(paste0("_w8w", max(waves)), colnames(bes), 
        ignore.case = TRUE)]
    colnames(bes)[grepl(paste0("_w8w", 6), colnames(bes), ignore.case = TRUE)]
    colnames(bes)[grepl(paste0("_w8w", 5), colnames(bes), ignore.case = TRUE)]
    colnames(bes)[grepl(paste0("_w8w", 4), colnames(bes), ignore.case = TRUE)]
    colnames(bes)[grepl(paste0("_w8w", 3), colnames(bes), ignore.case = TRUE)]
    colnames(bes)[grepl(paste0("_w8w", 2), colnames(bes), ignore.case = TRUE)]
    colnames(bes)[grepl(paste0("_w8w", 1), colnames(bes), ignore.case = TRUE)]
    targets[targets$table == "Newspaper", "group"] <- tolower(gsub(" ", 
        "", targets[targets$table == "Newspaper", "group"]))
    scotland.targets <- createWeightsList(targets, value = "Scotland")
    london.targets <- createWeightsList(targets, value = "London")
    england.targets <- createWeightsList(targets, value = "England")
    wales.targets <- createWeightsList(targets, value = "Wales")
    bes.eng <- formatData(bes = bes, region = "England", waves = waves)
    bes.wales <- formatData(bes = bes, region = "Wales", waves = waves)
    bes.london <- formatData(bes = bes, region = "London", waves = waves)
    bes.scot <- formatData(bes = bes, region = "Scotland", waves = waves)
    scot.wt <- createWeights(current.targets = scotland.targets, 
        bes = bes.scot)
    eng.wt <- createWeights(current.targets = england.targets, 
        bes = bes.eng)
    wales.wt <- createWeights(current.targets = wales.targets, 
        bes = bes.wales)
    london.wt <- createWeights(current.targets = london.targets, 
        bes.london)
    weights.df <- rbind(eng.wt, scot.wt, wales.wt, london.wt)
    return(weights.df)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
