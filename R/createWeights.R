createWeights <- function(current.targets, bes) {
  not.there <- names(current.targets)[!names(current.targets) %in% names(bes)]
  
  if(length(not.there) > 0 ) {
    stop(paste("Var not present in BES", not.there, collapse = ""))
  }
  vars <- bes[complete.cases(bes[, names(current.targets)]), ]
  if(any(!sapply(vars[, names(current.targets)], is.factor))) {
    stop("Following vars are not factor:", names(which(!sapply(vars[, names(current.targets)], is.factor))))
  }
  testVar <- function(name) {
    if(all(sort(levels(vars[[name]])) ==sort(names(current.targets[[name]]) ))) {
      return(NULL)
    } else {
      stop(paste0("Failed on ", name))
    }
  }
  sapply(names(current.targets), testVar)
  library(anesrake)
#   levels(bes$Newspaper)
#   targets[targets$table=="Newspaper", ]
  
  output <- anesrake(inputter = current.targets,
                     dataframe = vars, 
                     caseid = vars$id, 
                     verbose = TRUE, 
                     cap = 15, choosemethod = "total",
                     type = "nolim", pctlim = 0.005,
                     nlim = 5, iterate = TRUE, force1 = TRUE)
  vars$test_weight <- output$weightvec
  
  vars <- vars[, c("id", "test_weight", "region.weighted")]
  return(vars)
}