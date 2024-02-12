library(ggplot2)
library(reshape2)

wilcox.test(LVP_No_Master$PENS_Competence~LVP_No_Master$Gender, data = LVP_No_Master)
wilcox.test(LVP_No_Master$PENS_Autonomy~LVP_No_Master$Gender, data = LVP_No_Master)
wilcox.test(LVP_No_Master$PENS_Controls~LVP_No_Master$Gender, data = LVP_No_Master)
wilcox.test(LVP_No_Master$PENS_Presence~LVP_No_Master$Gender, data = LVP_No_Master)
wilcox.test(LVP_No_Master$PENS_Relatedness~LVP_No_Master$Gender, data = LVP_No_Master)

#woman feel less competent


GenderPE <- LVP_No_Master[c(3,47:51)]
GenderPEGraph <- melt(GenderPE)
ggplot(ExpensePEGraph, aes(x=GenderPEGraph$variable, 
                           y=GenderPEGraph$value, 
                           fill=GenderPEGraph$Gender)) + geom_boxplot(outlier.shape = NA) + ylim(1,7)+
  labs(list(x = "Subscale", y = "PENS", fill = "Gender"))+ scale_fill_discrete(name="Gender",
                                                                                labels=c("Female", "Male")) + 
  theme(axis.text.x = element_text(colour="grey20",size=10,angle=0,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=16,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=16,angle=90,hjust=.5,vjust=.5,face="plain"))



#only support

PENS_Supp_Only <- subset(LVP_No_Master, Role=="Support")



wilcox.test(PENS_Supp_Only$PENS_Competence~PENS_Supp_Only$Gender, data = LVP_No_Master)
wilcox.test(PENS_Supp_Only$PENS_Autonomy~PENS_Supp_Only$Gender, data = LVP_No_Master)
wilcox.test(PENS_Supp_Only$PENS_Controls~PENS_Supp_Only$Gender, data = LVP_No_Master)
wilcox.test(PENS_Supp_Only$PENS_Presence~PENS_Supp_Only$Gender, data = LVP_No_Master)
wilcox.test(PENS_Supp_Only$PENS_Relatedness~PENS_Supp_Only$Gender, data = LVP_No_Master)




LVP_No_Master$PlayPreference <- as.factor(LVP_No_Master$PlayPreference)

kruskal.test(LVP_No_Master$PENS_Competence~LVP_No_Master$PlayPreference, data = LVP_No_Master)
kruskal.test(LVP_No_Master$PENS_Autonomy~LVP_No_Master$PlayPreference, data = LVP_No_Master)
kruskal.test(LVP_No_Master$PENS_Controls~LVP_No_Master$PlayPreference, data = LVP_No_Master)
kruskal.test(LVP_No_Master$PENS_Presence~LVP_No_Master$PlayPreference, data = LVP_No_Master)
kruskal.test(LVP_No_Master$PENS_Relatedness~LVP_No_Master$PlayPreference, data = LVP_No_Master)

#no significant differences in any case (play preference)

LVP_No_Master$Role <- as.factor(LVP_No_Master$Role)

kruskal.test(LVP_No_Master$PENS_Competence~LVP_No_Master$Role, data = LVP_No_Master)
kruskal.test(LVP_No_Master$PENS_Autonomy~LVP_No_Master$Role, data = LVP_No_Master)
kruskal.test(LVP_No_Master$PENS_Controls~LVP_No_Master$Role, data = LVP_No_Master)
kruskal.test(LVP_No_Master$PENS_Presence~LVP_No_Master$Role, data = LVP_No_Master)
kruskal.test(LVP_No_Master$PENS_Relatedness~LVP_No_Master$Role, data = LVP_No_Master)

#diferencies en autonomy i en controls (presence marginally out)

#deeper:

library(FSA)
dunnTest(LVP_No_Master$PENS_Autonomy~LVP_No_Master$Role,
         data=LVP_No_Master)

dunnTest(LVP_No_Master$PENS_Controls~LVP_No_Master$Role,
         data=LVP_No_Master)

#poc rellevant
ggplot(LVP_No_Master, aes(x=LVP_No_Master$Role, 
                          y=LVP_No_Master$PENS_Controls)) + geom_boxplot(outlier.shape = NA) + ylim(1,7)+
  labs(list(x = "Rank", y = "Wins")) + theme(axis.text.x = element_text(colour="grey20",size=10,angle=0,hjust=.5,vjust=.5,face="plain"),
                                             axis.text.y = element_text(colour="grey20",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
                                             axis.title.x = element_text(colour="grey20",size=16,angle=0,hjust=.5,vjust=0,face="plain"),
                                             axis.title.y = element_text(colour="grey20",size=16,angle=90,hjust=.5,vjust=.5,face="plain"))





kruskal.test(LVP_No_Master$PENS_Competence~LVP_No_Master$Expense, data = LVP_No_Master)
kruskal.test(LVP_No_Master$PENS_Autonomy~LVP_No_Master$Expense, data = LVP_No_Master)
kruskal.test(LVP_No_Master$PENS_Controls~LVP_No_Master$Expense, data = LVP_No_Master)
kruskal.test(LVP_No_Master$PENS_Presence~LVP_No_Master$Expense, data = LVP_No_Master)
kruskal.test(LVP_No_Master$PENS_Relatedness~LVP_No_Master$Expense, data = LVP_No_Master)

# autonomy i presence
dunnTest(LVP_No_Master$PENS_Autonomy~LVP_No_Master$Expense,
         data=LVP_No_Master)

dunnTest(LVP_No_Master$PENS_Presence~LVP_No_Master$Expense,
         data=LVP_No_Master)

#interesant, els que gasten més tenen més autonomy i, en general, la presence és diferent en tots els grups


library(ggplot2)
library(reshape2)
ExpensePE <- LVP_No_Master[c(25,47:51)]
ExpensePEGraph <- melt(ExpensePE)
ggplot(ExpensePEGraph, aes(x=ExpensePEGraph$variable, 
                        y=ExpensePEGraph$value, 
                        fill=ExpensePEGraph$Expense)) + geom_boxplot(outlier.shape = NA) + ylim(1,7)+
  labs(list(x = "Subscale", y = "PENS", fill = "Expense"))+ scale_fill_discrete(name="Expense Level",
                                                                                labels=c("0€", "(0-5)€", "[5-10]€", ">10€")) + 
  theme(axis.text.x = element_text(colour="grey20",size=10,angle=0,hjust=.5,vjust=.5,face="plain"),
                                                                axis.text.y = element_text(colour="grey20",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
                                                                axis.title.x = element_text(colour="grey20",size=16,angle=0,hjust=.5,vjust=0,face="plain"),
                                                                axis.title.y = element_text(colour="grey20",size=16,angle=90,hjust=.5,vjust=.5,face="plain"))




kruskal.test(LVP_No_Master$totalRanked,LVP_No_Master$Summ_RANK, data=LVP_No_Master)
dunnTest(LVP_No_Master$totalRanked~LVP_No_Master$Summ_RANK,
         data=LVP_No_Master)


ggplot(LVP_No_Master, aes(x=LVP_No_Master$Summ_RANK, 
                           y=LVP_No_Master$totalRanked)) + geom_boxplot(outlier.shape = NA) + ylim(0,1000)+
  labs(list(x = "Rank", y = "Total Ranked Matches")) + theme(axis.text.x = element_text(colour="grey20",size=10,angle=0,hjust=.5,vjust=.5,face="plain"),
                                                                   axis.text.y = element_text(colour="grey20",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
                                                                   axis.title.x = element_text(colour="grey20",size=16,angle=0,hjust=.5,vjust=0,face="plain"),
                                                                   axis.title.y = element_text(colour="grey20",size=16,angle=90,hjust=.5,vjust=.5,face="plain"))
#rank depends on total number of wins

library(vioplot)
x1 <- LVP_No_Master$totalRanked[LVP_No_Master$Summ_RANK=="BRONZE"]
x2 <- LVP_No_Master$totalRanked[LVP_No_Master$Summ_RANK=="SILVER"]
x3 <- LVP_No_Master$totalRanked[LVP_No_Master$Summ_RANK=="GOLD"]
x4 <- LVP_No_Master$totalRanked[LVP_No_Master$Summ_RANK=="PLATINUM"]
x5 <- LVP_No_Master$totalRanked[LVP_No_Master$Summ_RANK=="DIAMOND"]
vioplot(x1, x2, x3, x4, x5, names=c("BRONZE", "SILVER", "GOLD", "PLATINUM","DIAMOND"), 
        col="gold")



LVP_No_Master$RankedWinRate <- LVP_No_Master$totalRankedWins/LVP_No_Master$totalRanked

kruskal.test(LVP_No_Master$RankedWinRate,LVP_No_Master$Summ_RANK, data=LVP_No_Master)
dunnTest(LVP_No_Master$RankedWinRate~LVP_No_Master$Summ_RANK,
         data=LVP_No_Master)
ggplot(LVP_No_Master, aes(x=LVP_No_Master$Summ_RANK, 
                          y=LVP_No_Master$RankedWinRate)) + geom_boxplot(outlier.shape = NA) + 
  labs(list(x = "Rank", y = "WinRate")) + theme(axis.text.x = element_text(colour="grey20",size=10,angle=0,hjust=.5,vjust=.5,face="plain"),
                                             axis.text.y = element_text(colour="grey20",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
                                             axis.title.x = element_text(colour="grey20",size=16,angle=0,hjust=.5,vjust=0,face="plain"),
                                             axis.title.y = element_text(colour="grey20",size=16,angle=90,hjust=.5,vjust=.5,face="plain"))




#és més strong amb l'absolut que el relatiu. S'ha de posar abans del PENS de Ranked
library(vioplot)
x1 <- LVP_No_Master$RankedWinRate[LVP_No_Master$Summ_RANK=="BRONZE"]
x2 <- LVP_No_Master$RankedWinRate[LVP_No_Master$Summ_RANK=="SILVER"]
x3 <- LVP_No_Master$RankedWinRate[LVP_No_Master$Summ_RANK=="GOLD"]
x4 <- LVP_No_Master$RankedWinRate[LVP_No_Master$Summ_RANK=="PLATINUM"]
x5 <- LVP_No_Master$RankedWinRate[LVP_No_Master$Summ_RANK=="DIAMOND"]
vioplot(x1, x2, x3, x4, x5, names=c("BRONZE", "SILVER", "GOLD", "PLATINUM","DIAMOND"), 
        col="gold")




#mirem si rank y expense estan relacionats
table_rank_expense <- table(LVP_No_Master$Summ_RANK, LVP_No_Master$Expense)

#dir que lamentablement no es pot saber quants skins i champions es tenen (protecció de dades)

#gender y rank
table_rank_gender <- table(LVP_No_Master$Summ_RANK, LVP_No_Master$Gender)

#test independence
chisq.test(table_rank_expense)
chisq.test(table_rank_gender)

#assume independence


#juguen més homes o dones?
wilcox.test(LVP_No_Master$totalRanked~LVP_No_Master$Gender, data=LVP_No_Master)

#(no hi ha diff)

table_role_gender <- table(LVP_No_Master$Role, LVP_No_Master$Gender)
table_role_gender_orig <- table(LVP_No_Master$Role, LVP_No_Master$Gender)
chisq.test(table_role_gender)

table_role_gender <- table(LVP_No_Master$Role, LVP_No_Master$Gender)
round(prop.table(table_role_gender, margin = 2),2)
prop.test(table_role_gender)
pairwise.prop.test(x = table_role_gender)
