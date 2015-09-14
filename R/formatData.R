formatData <- function(bes, region, wave = 6) {
#   if(wave==6) {
#     bes$partyMemberBinary <- NA
#     bes$partyMemberBinary[bes[, paste0("partyMemberW", wave)] %in% c(0, 2)] <- "None"
#     bes$partyMemberBinary[bes[, paste0("partyMemberW", wave)] %in% c(1)] <- "member"
#     bes$partyMemberBinary <- factor(bes$partyMemberBinary)
#   } else {
#     
#   }
  targets <- targets[targets$table!="partyMemberBinary", ]
  wave <- 6
  region <- "England"  
    
  bes$AgeGender <- factor(names(attributes(bes[, paste0("bpcas2w", wave)])$labels)[bes[, paste0("bpcas2w", wave)]])
  bes$SocialGrade <- factor(names(attributes(bes[, paste0("socgrade4_w8w", wave)])$labels)[bes[, paste0("socgrade4_w8w", wave)]])
  bes$Newspaper <- factor(names(attributes(bes[, paste0("bpcnews2010_w8w", wave)])$labels)[bes[, paste0("bpcnews2010_w8w", wave)]])
  bes$Partyid2010London <- factor(names(attributes(bes[, paste0("bpcnews2010_w8w", wave)])$labels)[
    bes[, paste0("bpcnews2010_w8w", wave)]])
  bes$Ethnicity <- factor(names(attributes(bes[, paste0("ethnicity2_w8w", wave)])$labels)[
    bes[, paste0("ethnicity2_w8w", wave)]])
  
  bes$Region <- factor(names(attributes(bes[, paste0("newgor_eng_w8w", wave)])$labels)[bes[, paste0("newgor_eng_w8w", wave)]])
  bes$WelshRegion <- factor(names(attributes(bes[, paste0("Wales_pcon_w8w", wave)])$labels)[bes[, paste0("Wales_pcon_w8w", wave)]])
  bes$HolyroodPastVote <- factor(c("Con", "Lab", "LD", "SNP", "SNP (Holyrood) & Lab (wmster)", "Oth", "DNV")[
    bes[, paste0("comb_pv_w8w", wave)]])
  
  if(region=="England") {
    names(attributes(bes[, paste0("partyid2010_Eng_w8w", wave)])$labels) <- c("Labour", "Con", "Lib Dem", "Oth", "None / DK")
    bes$Partyid2010 <- factor(names(attributes(bes[, paste0("partyid2010_Eng_w8w", wave)])$labels)
                              [bes[, paste0("partyid2010_Eng_w8w", wave)]])
    bes <- bes[!bes$gor %in% c(7, 11, 10) & !is.na(bes$gor) & is.na(bes[, paste0("Wales_pcon_w8w", wave)]) & 
                 is.na(bes[, paste0("comb_pv_w8w", wave)]), ]    
  }
  
  
  
  if(region!="England") {
    names(attributes(bes[, paste0("partyid2010_Wales_w8w", wave)])$labels) <- 
      c("Labour", "Con", "Lib Dem", "SNP/PC", "Oth", "None / DK")
    bes$Partyid2010 <- factor(names(attributes(bes[, paste0("partyid2010_Wales_w8w", wave)])$labels)
                              [bes[, paste0("partyid2010_Wales_w8w", wave)]])
  }
  if(region=="Wales") {
    bes <- bes[!is.na(bes[, paste0("Wales_pcon_w8w", wave)]) & is.na(bes[, paste0("comb_pv_w8w", wave)]), ]
  }
  if(region=="London") {
    bes <- bes[bes$gor==7 & is.na(bes[, paste0("Wales_pcon_w8w", wave)]) & is.na(bes[, paste0("comb_pv_w8w", wave)]), ]
  }
  
  if(region=="Scotland") {
    bes <- bes[!is.na(bes[, paste0("comb_pv_w8w", wave)]) & is.na(bes[, paste0("Wales_pcon_w8w", wave)]), ]
  }
  print(unique(targets$table)[!unique(targets$table) %in% colnames(bes)])
  bes <- bes[, c("id", unique(targets$table))]
  bes$region.weighted <- region
  return(bes)
}