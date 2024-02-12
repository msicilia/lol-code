SPGQ_EMP_cronbach <- SPGQ_No_Master[c(26:32)]
SPGQ_NF_cronbach <- SPGQ_No_Master[c(33:38)]
SPGQ_BE_cronbach <- SPGQ_No_Master[c(39:46)]
SPGQ_cronbach <- SPGQ_No_Master[c(47:49)]
#psych::alpha(SPGQ_cronbach)
library(psy)
library(psych)

sink("cronbach3.txt")

#calculo alpha
cronbach(SPGQ_EMP_cronbach)
psych::alpha(SPGQ_EMP_cronbach)

cronbach(SPGQ_NF_cronbach)
psych::alpha(SPGQ_NF_cronbach)

cronbach(SPGQ_BE_cronbach)
psych::alpha(SPGQ_BE_cronbach)

#calculo r entre variables
cor(SPGQ_cronbach)
cor.test(SPGQ_cronbach$EMP_MEAN,SPGQ_cronbach$NF_MEAN)$p.value
cor.test(SPGQ_cronbach$EMP_MEAN,SPGQ_cronbach$BE_MEAN)$p.value
cor.test(SPGQ_cronbach$BE_MEAN,SPGQ_cronbach$NF_MEAN)$p.value


sink()


LVP_SPGQ_No_Master$Gender <- as.factor(LVP_SPGQ_No_Master$Gender)
LVP_SPGQ_No_Master$Summ_RANK <- as.factor(LVP_SPGQ_No_Master$Summ_RANK)
LVP_SPGQ_No_Master[,16] <- as.factor(LVP_SPGQ_No_Master[,16])

LVP_SPGQ_No_Master[,17] <- as.factor(LVP_SPGQ_No_Master[,17])
LVP_SPGQ_No_Master[,18] <- as.factor(LVP_SPGQ_No_Master[,18])
LVP_SPGQ_No_Master[,19] <- as.factor(LVP_SPGQ_No_Master[,19])
LVP_SPGQ_No_Master[,20] <- as.factor(LVP_SPGQ_No_Master[,20])
LVP_SPGQ_No_Master[,21] <- as.factor(LVP_SPGQ_No_Master[,21])
LVP_SPGQ_No_Master[,22] <- as.factor(LVP_SPGQ_No_Master[,22])
LVP_SPGQ_No_Master[,23] <- as.factor(LVP_SPGQ_No_Master[,23])
LVP_SPGQ_No_Master[,24] <- as.factor(LVP_SPGQ_No_Master[,24])
LVP_SPGQ_No_Master[,25] <- as.factor(LVP_SPGQ_No_Master[,25])

FINAL <- merge(LVP_SPGQ_No_Master,clustering_tesis,by.x=c('Summ_ID'),
                            by.y=c('ego_id'))


FINAL$Clusters_AF <- as.factor(FINAL$Clusters_AF)



LVP_No_Master <- merge(LVP_No_Master,clustering_tesis,by.x=c('Summ_ID'),
               by.y=c('ego_id'))

LVP_No_Master$Clusters_AF <- as.factor(LVP_No_Master$Clusters_AF)

clusterPENS <- LVP_No_Master[c(53,47:51)]

clusterSPGQ <- FINAL[c(53,47:49)]

kruskal.test(clusterSPGQ$EMPATHY~clusterSPGQ$Clusters_AF, data = clusterSPGQ)
kruskal.test(clusterSPGQ$`NEG. FEELINGS`~clusterSPGQ$Clusters_AF, data = clusterSPGQ)
kruskal.test(clusterSPGQ$`BEHAVIOURAL ENG.`~clusterSPGQ$Clusters_AF, data = clusterSPGQ)

kruskal.test(clusterPENS$PENS_Competence~clusterPENS$Clusters_AF, data = clusterPENS)
kruskal.test(clusterPENS$PENS_Autonomy~clusterPENS$Clusters_AF, data = clusterPENS)
kruskal.test(clusterPENS$PENS_Controls~clusterPENS$Clusters_AF, data = clusterPENS)
kruskal.test(clusterPENS$PENS_Presence~clusterPENS$Clusters_AF, data = clusterPENS)
kruskal.test(clusterPENS$PENS_Relatedness~clusterPENS$Clusters_AF, data = clusterPENS)

pairwise.wilcox.test(clusterPENS$PENS_Relatedness, 
                     clusterPENS$Clusters_AF,
                     conf.level = 0.95,
                     p.adjust.method="bonferroni")

library(FSA)
dunnTest(clusterPENS$PENS_Relatedness~clusterPENS$Clusters_AF,
         data=clusterPENS)

library(ggplot2)
library(reshape2)
clusterPENSGraph <- melt(clusterPENS)
ggplot(clusterPENSGraph, aes(x=clusterPENSGraph$variable, 
                          y=clusterPENSGraph$value, 
                          fill=clusterPENSGraph$Clusters_AF)) + geom_boxplot(outlier.shape = NA) + ylim(1,7)+
  labs(list(x = "Subscale", y = "PENS", fill = "Cluster")) + theme(axis.text.x = element_text(colour="grey20",size=10,angle=0,hjust=.5,vjust=.5,face="plain"),
                                                                axis.text.y = element_text(colour="grey20",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
                                                                axis.title.x = element_text(colour="grey20",size=16,angle=0,hjust=.5,vjust=0,face="plain"),
                                                                axis.title.y = element_text(colour="grey20",size=16,angle=90,hjust=.5,vjust=.5,face="plain"))



wilcox.test(LVP_SPGQ_No_Master$EMPATHY~LVP_SPGQ_No_Master$Gender, data = LVP_SPGQ_No_Master)
wilcox.test(LVP_SPGQ_No_Master$`NEG. FEELINGS`~LVP_SPGQ_No_Master$Gender, data = LVP_SPGQ_No_Master)
wilcox.test(LVP_SPGQ_No_Master$`BEHAVIOURAL ENG.`~LVP_SPGQ_No_Master$Gender, data = LVP_SPGQ_No_Master)

