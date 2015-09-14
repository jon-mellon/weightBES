createCustomWeights <- function(bes, waves = c(1,2,3,4,5,6), targets) {
#   if(max(waves)==2) {
#     stop("Weighting on wave 2 characterisetics is currently not supported. This will be implemented soon.")
#   }
  require(Hmisc, haven)
  colnames(bes)[grepl(paste0("_w8w", max(waves)), colnames(bes), ignore.case = TRUE)]
  colnames(bes)[grepl(paste0("_w8w", 6), colnames(bes), ignore.case = TRUE)]
  colnames(bes)[grepl(paste0("_w8w", 5), colnames(bes), ignore.case = TRUE)]
  colnames(bes)[grepl(paste0("_w8w", 4), colnames(bes), ignore.case = TRUE)]
  colnames(bes)[grepl(paste0("_w8w", 3), colnames(bes), ignore.case = TRUE)]
  colnames(bes)[grepl(paste0("_w8w", 2), colnames(bes), ignore.case = TRUE)]
  colnames(bes)[grepl(paste0("_w8w", 1), colnames(bes), ignore.case = TRUE)]
  
  targets[targets$table=="Newspaper", "group"] <- tolower(gsub(" ", "", targets[targets$table=="Newspaper", "group"]))
  
  scotland.targets <- createWeightsList(targets, value = "Scotland")
  london.targets <- createWeightsList(targets, value = "London")
  england.targets <- createWeightsList(targets, value = "England")
  wales.targets <- createWeightsList(targets, value = "Wales")
  
  bes.eng <- formatData(bes = bes, region = "England", waves = waves)
  bes.wales <- formatData(bes = bes, region = "Wales", waves = waves)
  bes.london <- formatData(bes = bes, region = "London", waves = waves)
  bes.scot <- formatData(bes = bes, region = "Scotland", waves = waves)
  
  scot.wt <- createWeights(current.targets = scotland.targets, bes = bes.scot)
  eng.wt <- createWeights(current.targets = england.targets, bes = bes.eng)
  wales.wt <- createWeights(current.targets = wales.targets, bes = bes.wales)
  london.wt <- createWeights(current.targets = london.targets, bes.london)
  weights.df <- rbind(eng.wt, scot.wt, wales.wt, london.wt)
  return(weights.df)
}