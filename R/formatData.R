formatData <- function(bes, region, waves = 1:6) {
  #   if(wave==6) {
  #     bes$partyMemberBinary <- NA
  #     bes$partyMemberBinary[bes[, paste0("partyMemberW", wave)] %in% c(0, 2)] <- "None"
  #     bes$partyMemberBinary[bes[, paste0("partyMemberW", wave)] %in% c(1)] <- "member"
  #     bes$partyMemberBinary <- factor(bes$partyMemberBinary)
  #   } else {
  #     
  #   }
#   bes <- bes.backup
  wave <- max(waves)

  bes$AgeGender <- as_factor(bes[, paste0("bpcas2w", wave)])
  bes$SocialGrade <- as_factor(bes[, paste0("socgrade4_w8w", wave)])
  
  bes$Newspaper <- as_factor(bes[, paste0("bpcnews2010_w8w", wave)])
  levels(bes$Newspaper) <- tolower(gsub(" ", "", levels(bes$Newspaper)))
  levels(bes$Newspaper) <- gsub("teleg", "tele", levels(bes$Newspaper), ignore.case = TRUE)
  levels(bes$Newspaper) <- gsub("indy", "indie", levels(bes$Newspaper), ignore.case = TRUE)

  bes$Partyid2010London <- as_factor(bes[, paste0("partyid2010_london_w8w", wave)])
  bes$Ethnicity <- as_factor(bes[, paste0("ethnicity2_w8w", wave)])
  
  bes$Region <- as_factor(bes[, paste0("newgor_eng_w8w", wave)])
  bes$WelshRegion <- as_factor(bes[, paste0("Wales_pcon_w8w", wave)] )

  bes$HolyroodPastVote <- as_factor(bes[, paste0("comb_pv_w8w", wave)])
  
  if(region=="England") {

    bes$Partyid2010 <- as_factor(bes[, paste0("partyid2010_Eng_w8w", wave)])
    bes <- bes[!bes$gor %in% c(7, 11, 10) & !is.na(bes$gor) & is.na(bes[, paste0("Wales_pcon_w8w", wave)]) & 
                 is.na(bes[, paste0("comb_pv_w8w", wave)]), ]
  }
  
  if(region!="England") {
    bes$Partyid2010 <- as_factor(bes[, paste0("partyid2010_Wales_w8w", wave)])
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
  if(length(unique(targets$table)[!unique(targets$table) %in% colnames(bes)])!=0) {
    print(unique(targets$table)[!unique(targets$table) %in% colnames(bes)])  
  }
  
  if(any(waves==1)) {
    bes <- bes[bes$wave1==1, ]
  }
  if(any(waves==2)) {
    bes <- bes[bes$wave2==1, ]
  }
  if(any(waves==3)) {
    bes <- bes[bes$wave3==1, ]
  }
  if(any(waves==4)) {
    bes <- bes[bes$wave4==1, ]
  }
  if(any(waves==5)) {
    bes <- bes[bes$wave5==1, ]
  }
  if(any(waves==6)) {
    bes <- bes[bes$wave6==1, ]
  }
  
  bes <- bes[, c("id", unique(targets$table))]
  bes$region.weighted <- region
  
  return(bes)
}