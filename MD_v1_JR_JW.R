# Order effects: 2 variables, 2 values (1); 4 values (2) 
# 14 separate DVs

require(plyr) || {install.packages("plyr"); require(plyr)}
require(dplyr) || {install.packages("dplyr"); require(dplyr)}
require(tidyr) || {install.packages("tidyr"); require(tidyr)}

setwd("C:/Users/Julian/GDrive/Unprocessed")
d <- tbl_df(read.csv("merged10142015_v2.csv",  header = T, sep=",",skip=0))

dd = data.frame(matrix(ncol = 0, nrow = dim(d)[1]))
dd$Subject = d$Subject
dd$Block = d$Block
dd$dType = d$dilemmaList
dd$dType[dd$dType==1] <- "Reasoning"
dd$dType[dd$dType==2] <- "Dumbfound"
# dd$DilemmaRT[dd$Dilemma=="Heinz"] <- d$HeinzText.RT[d$stimID==1]
# dd$DilemmaRT[dd$Dilemma=="Incest"] <- d$IncestText.RT[d$stimID==2]

# dilemmaList(2)=dumbfound; dilemmaList(1)=reasoning
# dd$Dilemma[dd$dType=="Dumbfound" | dd$dType=="Reasoning" | !is.na(dd$dType)] = d$Procedure.SubTrial.[d$Block==3]
dd$Dilemma[!is.na(dd$dType)] = d$Procedure.SubTrial.[d$Block==3]

#OrderRD: Ordering variable for whether reasoning (1) or dumbfounding (2) came first
dd$OrderRD[!is.na(dd$dType)] = d$dilemmaList[d$Block==3]
dd$OrderRD[dd$OrderRD==1] <- "ReasonFirst"
dd$OrderRD[dd$OrderRD==2] <- "DumbfoundFirst"

dd %>% group_by(Subject) %>% filter(Block==3) %>% 
  mutate(OrderRD=ifelse(dType %>% first()=="Reason","Dumbfound","Reason"))

# dd$Dilemma = d$dumbfoundList
# dd$Dilemma = d$reasonList  
# dd$Dilemma[dd$dType=="Reasoning"] = d$reasonList[d$dilemmaList==1]  
# dd$Dilemma[dd$dType=="Dumbfound"] = d$dumbfoundList[d$dilemmaList==2]
# dd$Trial = d$Trial
# dd$DV[dd$Dilemma=="Heinz"] <- d$HeinzDVImage.RESP[d$stimID==1]
# dd$DV[dd$Dilemma=="Incest"] <- d$IncestDVImage.RESP[d$stimID==2]
# dd$DV.RT[dd$Dilemma=="Heinz"] <- d$HeinzDVImage.RT[d$stimID==1]
# dd$DV.RT[dd$Dilemma=="Incest"] <- d$IncestDVImage.RT[d$stimID==2]

dd %>% group_by(Subject) %>% filter(Block==3) %>% 
  mutate(OrderRD=ifelse(dType %>% first()=="Reason","Reason","Dumbfound")) %>% View()

dT <- dd %>% group_by(Subject) %>% filter(Block==3) %>% distinct(Dilemma) %>% select(-Block,-OrderRD) %>% 
  mutate(dilemmaCode=substr(Dilemma,1,1)) %>% select(dilemmaCode)

dT2 <- dT %>% add_rownames() %>% mutate(rowname=as.integer(rowname)) %>% 
  group_by(Subject) %>% mutate(rowname=rowname-min(rowname)+1) %>% 
  spread(rowname,dilemmaCode) %>% unite(code,2:5,sep="")

dT2$code[dT2$code=="iche"] <- 1
dT2$code[dT2$code=="iceh"] <- 2
dT2$code[dT2$code=="cihe"] <- 3
dT2$code[dT2$code=="cieh"] <- 4 
dT2$code[dT2$code=="heic"] <- 5
dT2$code[dT2$code=="heci"] <- 6
dT2$code[dT2$code=="ehic"] <- 7
dT2$code[dT2$code=="ehci"] <- 8


# 11.11: import rest ------------------------------------------------------
#- Choice/RT for each DV
#- Order for Abstract (4! -- 24 combos)
#Subject	demoimages.resp	abstractimages.resp 	abstractimages.RT	disgustimages1.resp	disgustimages1.RT	disgustimages2.resp	disgustimages2.RT	cannibalDVimages.resp	cannibalDVimages.RT	euthDVimages.resp	euthDVimages.RT	heinzDVimages.resp	heinzdvimages.RT 	incestDVimages.resp	incestDVimages.ET	demolist 	dilemmalist	IDDlist	Mimiclist	Passivelist	Abstractlist	disgustlist1	disgustlist2	reasonlist	dumbfoundlist	cannibalDVlist	euthDVlist	heinzDVlist	incsetDVlist


load("MD_v1_JR_JW.RData")
d %<>% rename(SubjID=Subject)
dT4 <- dT3 %>% rename(SubjID=Subject)

############################

DVs <- c("incestDV","cannibalDV","heinzDV","euthDV","demo","disgust","abstract")
d2 <- d
d <- d %>% filter(SubjID==18)

#s = DVs[1]
for (s in DVs) {
  if (grepl("disgust",s)) {
    
    ### Disgust Images 1 ###
    dT <- d %>% select(SubjID,matches(paste0(s,"Images1.RESP")),ends_with(paste0(s,"List1"))) %>% 
      na.omit() %>% group_by(SubjID) %>% spread_(paste0(s,"List1"),(paste0(s,"Images1.RESP")))
    colnames(dT) <- paste(paste0(s,"Images1.RESP"), colnames(dT), sep = ".")
    dT4 <- dT4 %>% left_join(dT %>% rename_(SubjID = paste0(s,"Images1.RESP.SubjID")),by="SubjID")

    dT <- d %>% select(SubjID,ends_with(paste0(s,"Images1.RT")),ends_with(paste0(s,"List1"))) %>% 
      na.omit() %>% group_by(SubjID) %>% spread_(paste0(s,"List1"),(paste0(s,"Images1.RT")))
    colnames(dT) <- paste(paste0(s,"Images1.RT"), colnames(dT), sep = ".")
    dT4 <- dT4 %>% left_join(dT %>% rename_(SubjID = paste0(s,"Images1.RT.SubjID")),by="SubjID")
    
    
    ### Disgust Images 2 ###
    dT <- d %>% select(SubjID,matches(paste0(s,"Images2.RESP")),ends_with(paste0(s,"List2"))) %>% 
      na.omit() %>% group_by(SubjID) %>% spread_(paste0(s,"List2"),(paste0(s,"Images2.RESP")))
    colnames(dT) <- paste(paste0(s,"Images2.RESP"), colnames(dT), sep = ".")
    dT4 <- dT4 %>% left_join(dT %>% rename_(SubjID = paste0(s,"Images2.RESP.SubjID")),by="SubjID")
    
    dT <- d %>% select(SubjID,ends_with(paste0(s,"Images2.RT")),ends_with(paste0(s,"List2"))) %>% 
      na.omit() %>% group_by(SubjID) %>% spread_(paste0(s,"List2"),(paste0(s,"Images2.RT")))
    colnames(dT) <- paste(paste0(s,"Images2.RT"), colnames(dT), sep = ".")
    dT4 <- dT4 %>% left_join(dT %>% rename_(SubjID = paste0(s,"Images2.RT.SubjID")),by="SubjID")
    
  }
  else if (grepl("abstract",s)) {
    dT <- d %>% select(SubjID,matches(paste0(s,"Images.RESP")),matches(paste0(s,"List.S"))) %>%
      na.omit()  %>% group_by(SubjID) %>% spread_(paste0(s,"List.Sample"),(paste0(s,"Images.RESP")))
    colnames(dT) <- paste(paste0(s,"Images.RESP"), colnames(dT), sep = ".")
    dT4 <- dT4 %>% left_join(dT %>% rename_(SubjID = paste0(s,"Images.RESP.SubjID")),by="SubjID")
    
    dT <- d %>% select(SubjID,ends_with(paste0(s,"Images.RT")),matches(paste0(s,"List.S"))) %>% 
      na.omit() %>% group_by(SubjID) %>% spread_(paste0(s,"List.Sample"),(paste0(s,"Images.RT")))
    colnames(dT) <- paste(paste0(s,"Images.RT"), colnames(dT), sep = ".")
    dT4 <- dT4 %>% left_join(dT %>% rename_(SubjID = paste0(s,"Images.RT.SubjID")),by="SubjID")
    
    ## Record order of abtract images
    dT <- d %>% select(SubjID,ends_with(paste0(s,"List.Sample")),ends_with(paste0(s,"List"))) %>%
      na.omit() %>% group_by(SubjID) %>% spread_(paste0(s,"List.Sample"),(paste0(s,"List"))) %>% 
      unite(abtractOrder,2:5,sep="")
    dT4 <- dT4 %>% left_join(dT,by="SubjID")
  }
  else {
    dT <- d %>% select(SubjID,matches(paste0(s,"Images.RESP")),ends_with(paste0(s,"List"))) %>% 
      na.omit() %>% group_by(SubjID) %>% spread_(paste0(s,"List"),(paste0(s,"Images.RESP")))
    colnames(dT) <- paste(paste0(s,"Images.RESP"), colnames(dT), sep = ".")
    dT4 <- dT4 %>% left_join(dT %>% rename_(SubjID = paste0(s,"Images.RESP.SubjID")),by="SubjID")

    dT <- d %>% select(SubjID,ends_with(paste0(s,"Images.RT")),ends_with(paste0(s,"List"))) %>% 
      na.omit() %>% group_by(SubjID) %>% spread_(paste0(s,"List"),(paste0(s,"Images.RT")))
    colnames(dT) <- paste(paste0(s,"Images.RT"), colnames(dT), sep = ".")
    dT4 <- dT4 %>% left_join(dT %>% rename_(SubjID = paste0(s,"Images.RT.SubjID")),by="SubjID")
  }
}

setwd("C:/Users/Julian/GDrive/PGGfMRI/Behav/")
write.csv(dT4,"MD_v1_JR_JW_clean.csv",row.names = F)



##### Old Code #####
# 
# for (i in subjVec){
#   dd$OrderRD[dd$Subject==i] <- d$dilemmaList[d$Subject==i & d$Block == 3 & d$dumbfoundList.Sample==1 & d$LogLevel5 == 1]
# }
# 
# 
# 
# # adding demo
# dT <- d %>% select(SubjID,demoImages.RESP,demoList) %>% na.omit() %>% group_by(SubjID) %>% 
#   spread(demoList,demoImages.RESP)
# colnames(dT) <- paste("demo", colnames(dT), sep = ".")
# dT4 %<>% left_join(dT %>% rename(SubjID = demo.SubjID))
# 
# ############################
# 
# # adding disgust RESP and RT
# dT <- d %>% select(SubjID,disgustImages1.RESP,disgustList1) %>% na.omit() %>% group_by(SubjID) %>% 
#   spread(disgustList1,disgustImages1.RESP) 
# colnames(dT) <- paste("disgust.RESP", colnames(dT), sep = ".")
# dT4 %<>% left_join(dT %>% rename(SubjID = disgust.RESP.SubjID))
# 
# dT <- d %>% select(SubjID,disgustImages1.RT,disgustList1) %>% na.omit() %>% group_by(SubjID) %>% 
#   spread(disgustList1,disgustImages1.RT) 
# colnames(dT) <- paste("disgust.RT", colnames(dT), sep = ".")
# dT4 %<>% left_join(dT %>% rename(SubjID = disgust.RT.SubjID))