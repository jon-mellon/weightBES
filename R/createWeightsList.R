createWeightsList <- function(targets, value = "Scotland") {
  targets <- targets[!is.na(targets[, value]), ]
  createSubGroup <- function(table.choice) {
    subtarget <- targets[targets$table==table.choice, ]
    sub <- subtarget[, value]
    names(sub) <- subtarget$group
    return(sub)
  }
  all.tables <- unique(targets$table)
  overall.targets <- lapply(all.tables, createSubGroup)
  names(overall.targets) <- all.tables
  return(overall.targets)
}