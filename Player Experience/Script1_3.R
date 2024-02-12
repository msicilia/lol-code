library(ggplot2)
library(reshape2)
RankPE <- LVP_No_Master[c(6,47:51)]
RankPEGraph <- melt(RankPE)
ggplot(RankPEGraph, aes(x=RankPEGraph$variable, 
                             y=RankPEGraph$value, 
                             fill=RankPEGraph$Summ_RANK)) + geom_boxplot(outlier.shape = NA) + ylim(1,7)+
  labs(list(x = "Subscale", y = "PENS", fill = "Rank")) + theme(axis.text.x = element_text(colour="grey20",size=14,angle=0,hjust=.5,vjust=.5,face="plain"),
                                                                axis.text.y = element_text(colour="grey20",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
                                                                axis.title.x = element_text(colour="grey20",size=16,angle=0,hjust=.5,vjust=0,face="plain"),
                                                                axis.title.y = element_text(colour="grey20",size=16,angle=90,hjust=.5,vjust=.5,face="plain"),
                                                                legend.text = element_text(size=14),
                                                                legend.title = element_text(size=14))



kruskal.test(RankPE$PENS_Competence~RankPE$Summ_RANK, data = RankPE)
kruskal.test(RankPE$PENS_Autonomy~RankPE$Summ_RANK, data = RankPE)
kruskal.test(RankPE$PENS_Controls~RankPE$Summ_RANK, data = RankPE)
kruskal.test(RankPE$PENS_Presence~RankPE$Summ_RANK, data = RankPE)
kruskal.test(RankPE$PENS_Relatedness~RankPE$Summ_RANK, data = RankPE)


pairwise.wilcox.test(RankPE$PENS_Competence, 
                     RankPE$Summ_RANK,
                     conf.level = 0.95,
                     p.adjust.method="bonferroni")

pairwise.wilcox.test(RankPE$PENS_Presence, 
                     RankPE$Summ_RANK,
                     conf.level = 0.95,
                     p.adjust.method="bonferroni")

library(FSA)
dunnTest(RankPE$PENS_Competence~RankPE$Summ_RANK,
         data=RankPE)

dunnTest(RankPE$PENS_Presence~RankPE$Summ_RANK,
         data=RankPE)
