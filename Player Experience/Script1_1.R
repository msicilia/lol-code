PENS_cronbach <- LVP_No_Master[c(47:51)]
alpha(PENS_cronbach)
library(psy)
library(psych)

sink("cronbach.txt")

#calculo alpha
cronbach(PENS_cronbach)
alpha(PENS_cronbach)
#calculo r entre variables
cor(PENS_cronbach)



PENS_cronbach_Others <- subset(LOLvsOthers, Type=="NoMOBA",select=2:6)
cronbach(PENS_cronbach_Others)
alpha(PENS_cronbach_Others)
cor(PENS_cronbach_Others)




cor.test(PENS_cronbach$PENS_Competence,PENS_cronbach$PENS_Autonomy)$p.value
cor.test(PENS_cronbach$PENS_Competence,PENS_cronbach$PENS_Relatedness)$p.value
cor.test(PENS_cronbach$PENS_Competence,PENS_cronbach$PENS_Presence)$p.value
cor.test(PENS_cronbach$PENS_Competence,PENS_cronbach$PENS_Controls)$p.value
cor.test(PENS_cronbach$PENS_Autonomy,PENS_cronbach$PENS_Relatedness)$p.value
cor.test(PENS_cronbach$PENS_Autonomy,PENS_cronbach$PENS_Presence)$p.value
cor.test(PENS_cronbach$PENS_Autonomy,PENS_cronbach$PENS_Controls)$p.value
cor.test(PENS_cronbach$PENS_Relatedness,PENS_cronbach$PENS_Presence)$p.value
cor.test(PENS_cronbach$PENS_Relatedness,PENS_cronbach$PENS_Controls)$p.value
cor.test(PENS_cronbach$PENS_Presence,PENS_cronbach$PENS_Controls)$p.value


cor.test(PENS_cronbach_Others$PENS_Competence,PENS_cronbach_Others$PENS_Autonomy)$p.value
cor.test(PENS_cronbach_Others$PENS_Competence,PENS_cronbach_Others$PENS_Relatedness)$p.value
cor.test(PENS_cronbach_Others$PENS_Competence,PENS_cronbach_Others$PENS_Presence)$p.value
cor.test(PENS_cronbach_Others$PENS_Competence,PENS_cronbach_Others$PENS_Controls)$p.value
cor.test(PENS_cronbach_Others$PENS_Autonomy,PENS_cronbach_Others$PENS_Relatedness)$p.value
cor.test(PENS_cronbach_Others$PENS_Autonomy,PENS_cronbach_Others$PENS_Presence)$p.value
cor.test(PENS_cronbach_Others$PENS_Autonomy,PENS_cronbach_Others$PENS_Controls)$p.value
cor.test(PENS_cronbach_Others$PENS_Relatedness,PENS_cronbach_Others$PENS_Presence)$p.value
cor.test(PENS_cronbach_Others$PENS_Relatedness,PENS_cronbach_Others$PENS_Controls)$p.value
cor.test(PENS_cronbach_Others$PENS_Presence,PENS_cronbach_Others$PENS_Controls)$p.value

sink()
