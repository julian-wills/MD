###MD preproc

#-- Create long and wide versions

require(plyr) || {install.packages("plyr"); require(plyr)}
require(dplyr) || {install.packages("dplyr"); require(dplyr)}

setwd("C:/Users/Julian/GDrive/MoralDumbfound/cleaned/")
d <- tbl_df(read.table("MD13-71.csv",  header = T, sep=",",skip=1))

dd = data.frame(matrix(ncol = 0, nrow = dim(d)[1]))
dd$Subject = d$Subject
dd$Block = d$Block
dd$Dilemma = d$stimID
dd$Dilemma[dd$Dilemma==1] <- "Heinz"
dd$Dilemma[dd$Dilemma==2] <- "Incest"
dd$DilemmaRT[dd$Dilemma=="Heinz"] <- d$HeinzText.RT[d$stimID==1]
dd$DilemmaRT[dd$Dilemma=="Incest"] <- d$IncestText.RT[d$stimID==2]
dd$Trial = d$Trial
dd$DV[dd$Dilemma=="Heinz"] <- d$HeinzDVImage.RESP[d$stimID==1]
dd$DV[dd$Dilemma=="Incest"] <- d$IncestDVImage.RESP[d$stimID==2]
dd$DV.RT[dd$Dilemma=="Heinz"] <- d$HeinzDVImage.RT[d$stimID==1]
dd$DV.RT[dd$Dilemma=="Incest"] <- d$IncestDVImage.RT[d$stimID==2]

subjVec = (unique(select(dd,Subject))) %>% .$Subject
for (i in subjVec){
  dd$Order[dd$Subject==i] <- d$stimID[d$Subject==i & d$Block == 1 & d$Trial == 1]
}

write.csv(dd,file= paste('MD_preproc.csv',sep=""),row.names =FALSE)

# Merge w/ lesion data ----------------------------------------------------
setwd("C:/Users/Julian/GDrive/MoralDumbfound/cleaned/")
dMDLong <- tbl_df(read.table("MD_preproc.csv", header = T, sep=",")) %>% 
  rename(SubjID=Subject)

setwd("C:/Users/Julian/GDrive/LesionProject/PGGtemp")
dMDPatLong <- tbl_df(read.table("PAT_PGG_Long_v12-LSMfiltWide.csv", header = T, sep=",")) %>% 
  left_join(dMDLong,"SubjID") %>% 
  mutate(Question = plyr::mapvalues(Trial,
                  c(1:14),
                  c("Harm","HConf","Wrong","WConf","Cnfsed","Gut","Reason",
                    "Disgust","Happy","Angry","Neutral","Sad","Fear","Surprise")))

dMDPatLong$Question = factor(dMDPatLong$Question, levels = dMDPatLong$Question[order(dMDPatLong$Trial)])

ggplot(dMDPatLong %>% filter(!is.na(Trial)), aes(y = DV, x = Question)) +
  geom_violin() +
  stat_summary(fun.y = mean, geom = "point", shape = 5, size=2) +
  geom_errorbar(stat = "hline", linetype="dotted", yintercept = "mean",
                width=.7,aes(ymax=..y..,ymin=..y..))

# violin plots for each DV, split by dilemma
ggplot(dMDPatLong %>% filter(!is.na(Trial)), aes(y = DV, x = Question)) +
  geom_violin() +
  stat_summary(fun.y = mean, geom = "point", shape = 5, size=2) +
  geom_errorbar(stat = "hline", linetype="dotted", yintercept = "mean",
                width=.7,aes(ymax=..y..,ymin=..y..)) +
  scale_y_continuous(breaks=1:7,"DV Rating") +
  facet_wrap(~ Dilemma, nrow=2)

# violin plots for each DV, split by dilemma and order
# order=1 (Heinz first); order=2 (Incest first)
ggplot(dMDPatLong %>% filter(!is.na(Trial)), aes(y = DV, x = Question)) +
  geom_violin() +
  stat_summary(fun.y = mean, geom = "point", shape = 5, size=2) +
  geom_errorbar(stat = "hline", linetype="dotted", yintercept = "mean",
                width=.7,aes(ymax=..y..,ymin=..y..)) +
  scale_y_continuous(breaks=1:7,"DV Rating") +
  facet_grid(Order ~ Dilemma)
#! Incest more harmful (less confident), more wrong, more confusing, less scary/surprising when shown first
#! Heinz less wrong and less disgusting when presented second

Lesion.pallet <- c("gray80","gray60","palegreen2","lightcoral","cornflowerblue")

# violin plots for each DV, split ROI
ggplot(dMDPatLong %>% filter(!is.na(Trial)) %>% 
        mutate(ROI = factor(ROI, levels = c("HC", "BDC", "Amyg","VMPFC","DLPFC"))),
        aes(y = DV, x = Question, fill=ROI)) +
  scale_fill_manual(values=Lesion.pallet) +
  geom_violin() +
  stat_summary(fun.y = mean, geom = "point", shape = 5, size=2) +
  geom_errorbar(stat = "hline", linetype="dotted", yintercept = "mean",
                width=.7,aes(ymax=..y..,ymin=..y..)) +
  scale_y_continuous(breaks=1:7,"DV Rating") +
  facet_grid(ROI ~ Dilemma)

# violin plots for each DV, split ROI, only Harm|Wrong|Gut|Reason|Disgust
ggplot(dMDPatLong %>% filter(!is.na(Trial),Question=="Harm"|Question=="Wrong"|
                               Question=="Gut"|Question=="Reason"|Question=="Disgust") %>% 
         mutate(ROI = factor(ROI, levels = c("HC", "BDC", "Amyg","VMPFC","DLPFC"))),
       aes(y = DV, x = Question, fill=ROI)) +
  scale_fill_manual(values=Lesion.pallet) +
  geom_violin() +
  stat_summary(fun.y = mean, geom = "point", shape = 5, size=2) +
  geom_errorbar(stat = "hline", linetype="dotted", yintercept = "mean",
                width=.7,aes(ymax=..y..,ymin=..y..)) +
  scale_y_continuous(breaks=1:7,"DV Rating") +
  facet_grid(ROI ~ Dilemma)

# exploring mean differences
require(tidyr)
dMDPatLong %>% select(SubjID,Dilemma,Order) %>% filter(SubjID==16)
dMDPatLong %>% group_by(ROI) %>% dplyr::summarise(count=n()/28)
nsdMDPatLong %>% group_by(ROI,Dilemma,Question) %>% na.omit() %>% dplyr::summarise(DV=mean(DV)) %>% 
  spread(Dilemma,DV) %>% mutate(Diff=Incest-Heinz) %>% select(-Heinz,-Incest) %>% 
  spread(ROI,Diff) %>% select(Question,HC,BDC,Amyg,VMPFC,DLPFC)

?spread
# old code ----------------------------------------------------------------
# dd$Label[dd$Trial == 1] <- "Harm"ns
# dd$Label[dd$Trial == 2] <- "HarmConfident"
# dd$Label[dd$Trial == 3] <- "Wrong"
# dd$Label[dd$Trial == 4] <- "WrongConfident"
# dd$Label[dd$Trial == 5] <- "Confused"
# dd$Label[dd$Trial == 6] <- "Gut"
# dd$Label[dd$Trial == 7] <- "Reasoning"
# dd$Label[dd$Trial == 8] <- "Disgust"
# dd$Label[dd$Trial == 9] <- "Happy"
# dd$Label[dd$Trial == 10] <- "Angry"
# dd$Label[dd$Trial == 11] <- "Neutral"
# dd$Label[dd$Trial == 12] <- "Sad"
# dd$Label[dd$Trial == 13] <- "Fear"
# dd$Label[dd$Trial == 14] <- "Surprise"
