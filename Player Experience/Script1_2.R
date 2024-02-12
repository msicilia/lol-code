LOLvsOthers$Type <- as.factor(LOLvsOthers$Type)

wilcox.test(LOLvsOthers$PENS_Competence~LOLvsOthers$Type, data = LOLvsOthers)
wilcox.test(LOLvsOthers$PENS_Autonomy~LOLvsOthers$Type, data = LOLvsOthers)
wilcox.test(LOLvsOthers$PENS_Controls~LOLvsOthers$Type, data = LOLvsOthers)
wilcox.test(LOLvsOthers$PENS_Presence~LOLvsOthers$Type, data = LOLvsOthers)
wilcox.test(LOLvsOthers$PENS_Relatedness~LOLvsOthers$Type, data = LOLvsOthers)

kruskal.test(LOLvsOthers$PENS_Competence~LOLvsOthers$Type, data = LOLvsOthers)
kruskal.test(LOLvsOthers$PENS_Autonomy~LOLvsOthers$Type, data = LOLvsOthers)
kruskal.test(LOLvsOthers$PENS_Controls~LOLvsOthers$Type, data = LOLvsOthers)
kruskal.test(LOLvsOthers$PENS_Presence~LOLvsOthers$Type, data = LOLvsOthers)
kruskal.test(LOLvsOthers$PENS_Relatedness~LOLvsOthers$Type, data = LOLvsOthers)

boxplot(LOLvsOthers$PENS_Relatedness~LOLvsOthers$Type, data = LOLvsOthers)

library(ggplot2)
library(reshape2)
#qplot(LOLvsOthers$Type,LOLvsOthers$PENS_Competence, data=LOLvsOthers, geom="boxplot")
LOLvsOthersGraph <- melt(LOLvsOthers)
boxplot(LOLvsOthersGraph$value~LOLvsOthersGraph$variable*LOLvsOthersGraph$Type, col=c("dodgerblue1","coral1"),outline = FALSE)
legend("bottom", inset=.05,
       c("LOL","NoMOBA"), fill=c("dodgerblue1","coral1"), horiz=TRUE)

ggplot(LOLvsOthersGraph, aes(x=LOLvsOthersGraph$variable, 
                             y=LOLvsOthersGraph$value, 
                             fill=LOLvsOthersGraph$Type)) + geom_boxplot(outlier.shape = NA) + ylim(1,7)+
  labs(list(x = "Subscale", y = "PENS", fill = "Type")) + theme(axis.text.x = element_text(colour="grey20",size=10,angle=0,hjust=.5,vjust=.5,face="plain"),
                                                                axis.text.y = element_text(colour="grey20",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
                                                                axis.title.x = element_text(colour="grey20",size=16,angle=0,hjust=.5,vjust=0,face="plain"),
                                                                axis.title.y = element_text(colour="grey20",size=16,angle=90,hjust=.5,vjust=.5,face="plain"))
