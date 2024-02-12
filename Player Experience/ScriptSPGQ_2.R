library(ggplot2)
library(reshape2)
RankSPGQ <- SPGQ_No_Master[c(6,47:49)]
RankSPGQ$Summ_RANK <- as.factor(RankSPGQ$Summ_RANK)
RankSPGQGraph <- melt(RankSPGQ)
ggplot(RankSPGQGraph, aes(x=RankSPGQGraph$variable, 
                             y=RankSPGQGraph$value, 
                             fill=RankSPGQGraph$Summ_RANK)) + geom_boxplot(outlier.shape = NA) + ylim(0,4)+
  labs(list(x = "Subscale", y = "SPGQ", fill = "Rank")) + theme(axis.text.x = element_text(colour="grey20",size=10,angle=0,hjust=.5,vjust=.5,face="plain"),
                                                                axis.text.y = element_text(colour="grey20",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
                                                                axis.title.x = element_text(colour="grey20",size=16,angle=0,hjust=.5,vjust=0,face="plain"),
                                                                axis.title.y = element_text(colour="grey20",size=16,angle=90,hjust=.5,vjust=.5,face="plain"))



kruskal.test(RankSPGQ$EMPATHY~RankSPGQ$Summ_RANK, data = RankSPGQ)
kruskal.test(RankSPGQ$`NEG. FEELINGS`~RankSPGQ$Summ_RANK, data = RankSPGQ)
kruskal.test(RankSPGQ$`BEHAVIOURAL ENG.`~RankSPGQ$Summ_RANK, data = RankSPGQ)


pairwise.wilcox.test(RankSPGQ$EMPATHY, 
                     RankSPGQ$Summ_RANK,
                     conf.level = 0.95,
                     p.adjust.method="bonferroni")

pairwise.wilcox.test(RankSPGQ$NF_MEAN, 
                     RankSPGQ$Summ_RANK,
                     conf.level = 0.95,
                     p.adjust.method="bonferroni")

pairwise.wilcox.test(RankSPGQ$BE_MEAN, 
                     RankSPGQ$Summ_RANK,
                     conf.level = 0.95,
                     p.adjust.method="bonferroni")

library(FSA)
dunnTest(RankSPGQ$EMPATHY~RankSPGQ$Summ_RANK,
         data=RankSPGQ)

dunnTest(RankSPGQ$NF_MEAN~RankSPGQ$Summ_RANK,
         data=RankSPGQ)

dunnTest(RankSPGQ$BE_MEAN~RankSPGQ$Summ_RANK,
         data=RankSPGQ)
