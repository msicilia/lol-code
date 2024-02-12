library(ggplot2)
library(reshape2)

SPGQ_No_Master$Summ_RANK <- as.factor(SPGQ_No_Master$Summ_RANK)
SPGQ_No_Master$Gender <- as.factor(SPGQ_No_Master$Gender)


wilcox.test(SPGQ_No_Master$EMPATHY~SPGQ_No_Master$Gender, data = SPGQ_No_Master)
wilcox.test(SPGQ_No_Master$`NEG. FEELINGS`~SPGQ_No_Master$Gender, data = SPGQ_No_Master)
wilcox.test(SPGQ_No_Master$`BEHAVIOURAL ENG.`~SPGQ_No_Master$Gender, data = SPGQ_No_Master)

#woman feel same Social Presence


GenderPE <- SPGQ_No_Master[c(3,47:49)]
GenderPEGraph <- melt(GenderPE)
ggplot(GenderPEGraph, aes(x=GenderPEGraph$variable, 
                           y=GenderPEGraph$value, 
                           fill=GenderPEGraph$Gender)) + geom_boxplot(outlier.shape = NA) + ylim(0,4)+
  labs(list(x = "Subscale", y = "PENS", fill = "Gender"))+ scale_fill_discrete(name="Gender",
                                                                                labels=c("Female", "Male")) + 
  theme(axis.text.x = element_text(colour="grey20",size=10,angle=0,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=16,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=16,angle=90,hjust=.5,vjust=.5,face="plain"))



SPGQ_No_Master$PlayPreference <- factor(SPGQ_No_Master$PlayPreference, levels = c("Solo", "Premade de dos personas", "Premade de tres o cuatro", "Equipo completo premade"))

kruskal.test(SPGQ_No_Master$EMPATHY~SPGQ_No_Master$PlayPreference, data = SPGQ_No_Master)
kruskal.test(SPGQ_No_Master$`NEG. FEELINGS`~SPGQ_No_Master$PlayPreference, data = SPGQ_No_Master)
kruskal.test(SPGQ_No_Master$`BEHAVIOURAL ENG.`~SPGQ_No_Master$PlayPreference, data = SPGQ_No_Master)


SPGQ_No_Master$Role <- as.factor(SPGQ_No_Master$Role)

kruskal.test(SPGQ_No_Master$EMPATHY~SPGQ_No_Master$Role, data = SPGQ_No_Master)
kruskal.test(SPGQ_No_Master$`NEG. FEELINGS`~SPGQ_No_Master$Role, data = SPGQ_No_Master)
kruskal.test(SPGQ_No_Master$`BEHAVIOURAL ENG.`~SPGQ_No_Master$Role, data = SPGQ_No_Master)


#diferencies en EMP NOMÉS i amb Play Preference, no en rol!

#deeper:

library(FSA)
dunnTest(SPGQ_No_Master$EMPATHY~SPGQ_No_Master$PlayPreference,
         data=SPGQ_No_Master)

#poc rellevant

levels(SPGQ_No_Master$PlayPreference) <- c("Solo","Duo","Three/Four","Full Premade")
ggplot(SPGQ_No_Master, aes(x=SPGQ_No_Master$PlayPreference, 
                          y=SPGQ_No_Master$EMPATHY)) + geom_boxplot(outlier.shape = NA) + ylim(0,4)+
  labs(list(x = "Play preference", y = "SPGQ")) + theme(axis.text.x = element_text(colour="grey20",size=14,angle=0,hjust=.5,vjust=.5,face="plain"),
                                             axis.text.y = element_text(colour="grey20",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
                                             axis.title.x = element_text(colour="grey20",size=16,angle=0,hjust=.5,vjust=0,face="plain"),
                                             axis.title.y = element_text(colour="grey20",size=16,angle=90,hjust=.5,vjust=.5,face="plain"))




SPGQ_No_Master$Expense <- as.factor(SPGQ_No_Master$Expense)

kruskal.test(SPGQ_No_Master$EMPATHY~SPGQ_No_Master$Expense, data = SPGQ_No_Master)
kruskal.test(SPGQ_No_Master$`NEG. FEELINGS`~SPGQ_No_Master$Expense, data = SPGQ_No_Master)
kruskal.test(SPGQ_No_Master$`BEHAVIOURAL ENG.`~SPGQ_No_Master$Expense, data = SPGQ_No_Master)


#################################################################################################

#res de res

plot(SPGQ_No_Master$Age,SPGQ_No_Master$`NEG. FEELINGS`)

kruskal.test(SPGQ_No_Master$EMPATHY~SPGQ_No_Master$Age, data = SPGQ_No_Master)
kruskal.test(SPGQ_No_Master$`NEG. FEELINGS`~SPGQ_No_Master$Age, data = SPGQ_No_Master)
kruskal.test(SPGQ_No_Master$`BEHAVIOURAL ENG.`~SPGQ_No_Master$Age, data = SPGQ_No_Master)





#####OTROS QUE NO APLICAN
#####
#####


kruskal.test(SPGQ_No_Master$totalRanked,SPGQ_No_Master$Summ_RANK, data=SPGQ_No_Master)
dunnTest(SPGQ_No_Master$totalRanked~SPGQ_No_Master$Summ_RANK,
         data=SPGQ_No_Master)


ggplot(SPGQ_No_Master, aes(x=SPGQ_No_Master$Summ_RANK, 
                           y=SPGQ_No_Master$totalRanked)) + geom_boxplot(outlier.shape = NA) + ylim(0,1000)+
  labs(list(x = "Rank", y = "Total Ranked Matches")) + theme(axis.text.x = element_text(colour="grey20",size=10,angle=0,hjust=.5,vjust=.5,face="plain"),
                                                                   axis.text.y = element_text(colour="grey20",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
                                                                   axis.title.x = element_text(colour="grey20",size=16,angle=0,hjust=.5,vjust=0,face="plain"),
                                                                   axis.title.y = element_text(colour="grey20",size=16,angle=90,hjust=.5,vjust=.5,face="plain"))
#rank depends on total number of wins

library(vioplot)
x1 <- SPGQ_No_Master$totalRanked[SPGQ_No_Master$Summ_RANK=="BRONZE"]
x2 <- SPGQ_No_Master$totalRanked[SPGQ_No_Master$Summ_RANK=="SILVER"]
x3 <- SPGQ_No_Master$totalRanked[SPGQ_No_Master$Summ_RANK=="GOLD"]
x4 <- SPGQ_No_Master$totalRanked[SPGQ_No_Master$Summ_RANK=="PLATINUM"]
x5 <- SPGQ_No_Master$totalRanked[SPGQ_No_Master$Summ_RANK=="DIAMOND"]
vioplot(x1, x2, x3, x4, x5, names=c("BRONZE", "SILVER", "GOLD", "PLATINUM","DIAMOND"), 
        col="gold")



SPGQ_No_Master$RankedWinRate <- SPGQ_No_Master$totalRankedWins/SPGQ_No_Master$totalRanked

kruskal.test(SPGQ_No_Master$RankedWinRate,SPGQ_No_Master$Summ_RANK, data=SPGQ_No_Master)
dunnTest(SPGQ_No_Master$RankedWinRate~SPGQ_No_Master$Summ_RANK,
         data=SPGQ_No_Master)
ggplot(SPGQ_No_Master, aes(x=SPGQ_No_Master$Summ_RANK, 
                          y=SPGQ_No_Master$totalRanked)) + geom_boxplot(outlier.shape = NA) + 
  labs(list(x = "Rank", y = "WinRate")) + theme(axis.text.x = element_text(colour="grey20",size=10,angle=0,hjust=.5,vjust=.5,face="plain"),
                                             axis.text.y = element_text(colour="grey20",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
                                             axis.title.x = element_text(colour="grey20",size=16,angle=0,hjust=.5,vjust=0,face="plain"),
                                             axis.title.y = element_text(colour="grey20",size=16,angle=90,hjust=.5,vjust=.5,face="plain"))




#és més strong amb l'absolut que el relatiu. S'ha de posar abans del PENS de Ranked
library(vioplot)
x1 <- SPGQ_No_Master$RankedWinRate[SPGQ_No_Master$Summ_RANK=="BRONZE"]
x2 <- SPGQ_No_Master$RankedWinRate[SPGQ_No_Master$Summ_RANK=="SILVER"]
x3 <- SPGQ_No_Master$RankedWinRate[SPGQ_No_Master$Summ_RANK=="GOLD"]
x4 <- SPGQ_No_Master$RankedWinRate[SPGQ_No_Master$Summ_RANK=="PLATINUM"]
x5 <- SPGQ_No_Master$RankedWinRate[SPGQ_No_Master$Summ_RANK=="DIAMOND"]
vioplot(x1, x2, x3, x4, x5, names=c("BRONZE", "SILVER", "GOLD", "PLATINUM","DIAMOND"), 
        col="gold")




#mirem si rank y expense estan relacionats
table_rank_expense <- table(SPGQ_No_Master$Summ_RANK, SPGQ_No_Master$Expense)

#dir que lamentablement no es pot saber quants skins i champions es tenen (protecció de dades)

#gender y rank
table_rank_gender <- table(SPGQ_No_Master$Summ_RANK, SPGQ_No_Master$Gender)

#test independence
chisq.test(table_rank_expense)
chisq.test(table_rank_gender)

#assume independence


#juguen més homes o dones?
wilcox.test(SPGQ_No_Master$totalRanked~SPGQ_No_Master$Gender, data=SPGQ_No_Master)

#(no hi ha diff)

table_role_gender <- table(SPGQ_No_Master$Role, SPGQ_No_Master$Gender)
table_role_gender_orig <- table(SPGQ_No_Master$Role, SPGQ_No_Master$Gender)
chisq.test(table_role_gender)

table_role_gender <- table(SPGQ_No_Master$Role, SPGQ_No_Master$Gender)
round(prop.table(table_role_gender, margin = 2),2)
prop.test(table_role_gender)
pairwise.prop.test(x = table_role_gender)
