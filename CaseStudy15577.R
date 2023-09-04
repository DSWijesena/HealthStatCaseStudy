setwd("/Users/darsh/Documents/Uni/Health stat")
attach(Case_Study)

##DESCRIPTIVE ANALYSIS
summary(Case_Study)

#Age distribution
hist(Age,col ="#6ca0c0")

#Gender distribution 
gender = table(Gender)
gender_percent = round((gender/sum(gender)*100))
gender_lbls = paste(names(gender),gender_percent,"%")
pie(gender, col=c("steelblue", "navyblue","grey"), main = "Pie Chart of Gender",labels = gender_lbls)

library(descriptr)

#Gender with obesity
table_ob = table(Gender,`Pre-Obesity`)
prop_ob = prop.table(table_ob)
prop_ob
table_ob

barplot(table_ob,xlab ="Obesity Factor - Pre Program",ylab = "Count",
        col = c("steelblue", "navyblue","grey"),legend.text = TRUE,beside = TRUE)

#Gender with Hypertension
table_hyp = table(Gender,`Pre-Post-Hypertension`)
prop_hyp = prop.table(table_hyp)
prop_hyp
table_hyp

barplot(table_hyp,xlab ="Hypertension Factor - Pre Program",ylab = "Count",
        col = c("steelblue", "navyblue","grey"),legend.text = TRUE,beside = TRUE)

##Pre and post program changes 

#Obesity before and after the program comparison - gender categorized 
ob_before = table(`Pre-Obesity`)
ob_before

ob_after = table(`Post-Obesity`)
ob_after

ob_change = table(`Pre-Obesity`,`Post-Obesity`,Gender)
ob_change

#Hypertension before and after the program comparison - gender categorizes
hyp_before = table(`Pre-Post-Hypertension`)
hyp_before

hyp_after = table(`Post-Hypertension`)
hyp_after

hyp_change = table(`Pre-Post-Hypertension`,`Post-Hypertension`,Gender)
hyp_change
  
##Effect of Demographic factors on health outcomes 

#BMI
hist(`Pre-BMI`,col='#5969A8',xlim = c(20,40),xlab="Pre-BMI" ,ylim = c(0,70), ylab="Frequency",main="")
hist(`Post BMI`,col='#9f9cff',xlim = c(20,40),xlab="Post-BMI" ,ylim = c(0,70), ylab="",main="")


#BMI against age 
library(ggplot2)
install.packages("ggExtra")
library(ggExtra)

age_pre_bmi = ggplot(Case_Study, aes(x=Age, y=`Pre-BMI`)) +geom_point() 
age_post_bmi = ggplot(Case_Study,aes(x=Age,y=`Post BMI`)) + geom_point()

#p = ggMarginalGadget(age_pre_bmi)
p1 = ggMarginal(p = age_pre_bmi,type = 'histogram',margins = 'both',size = 5,colour = 'gray98',fill = '#5969A8')
p2 = ggMarginal(p = age_post_bmi,type = 'histogram',margins = 'both',size = 5,colour = 'gray98',fill = '#9f9cff')
p1
p2

cor(Age,`Pre-BMI`)
cor(Age,`Post BMI`)

#BMI against Gender

boxplot(`Pre-BMI`~Gender,col=c("steelblue", "navyblue","grey"),ylim = c(22,36))
boxplot(`Post BMI`~Gender,col=c("steelblue", "navyblue","grey"),ylim = c(22,36))

#Testing normality
qqnorm(`Pre-BMI`,main = "Normal Q-Q plot for Pre_BMI")
qqline(`Pre-BMI`)
shapiro.test(`Pre-BMI`) #p value significant --> Doesn't follow a normal distribution

#change in location for at least one gender category
kruskal.test(`Pre-BMI`~Gender)

#BMI against ethnicity

boxplot(`Pre-BMI`~Ethnicity,col=c("#0dad8d", "#30bfbf","#0c98ba","#1164b4"),,ylim = c(22,36))
boxplot(`Post BMI`~Ethnicity,col=c("#0dad8d", "#30bfbf","#0c98ba","#1164b4"),,ylim = c(22,36))

ethnicity = table(Ethnicity)
ethnicity
eth_percent = round((ethnicity/sum(ethnicity)*100))
eth_lbls = paste(names(ethnicity),eth_percent,"%")
pie(ethnicity, col=c("#0dad8d", "#30bfbf","#0c98ba","#1164b4"), main = "Pie Chart of Ethnicity",labels = eth_lbls)

kruskal.test(`Pre-BMI`,Ethnicity)

#Height
hist(`Pre-Height`,col='#064e40',xlab="Pre-Height" , ylab="Frequency",main="")
hist(`Post- height`,col='#0dad8d',xlab="Post-Height" , ylab="",main="")


qqnorm(`Pre-Height`)
qqline(`Pre-Height`)
shapiro.test(`Pre-Height`)

#Height against age  
age_pre_height = ggplot(Case_Study, aes(x=Age, y=`Pre-Height`)) +geom_point() 
age_post_height = ggplot(Case_Study,aes(x=Age,y=`Post- height`)) + geom_point()

p3 = ggMarginal(p = age_pre_height,type = 'histogram',margins = 'both',size = 5,colour = 'gray98',fill = '#064e40')
p4 = ggMarginal(p = age_post_height,type = 'histogram',margins = 'both',size = 5,colour = 'gray98',fill = '#0dad8d')
p3
p4

#Height against gender
boxplot(`Pre-Height`~Gender,col=c("steelblue", "navyblue","grey"))
boxplot(`Post- height`~Gender,col=c("steelblue", "navyblue","grey"))

kruskal.test(`Pre-Height`,Gender)


#Height against ethnicity
boxplot(`Pre-Height`~Ethnicity,col=c("#0dad8d", "#30bfbf","#0c98ba","#1164b4"))
boxplot(`Post- height`~Ethnicity,col=c("#0dad8d", "#30bfbf","#0c98ba","#1164b4"))

kruskal.test(`Pre-Height`,Ethnicity)

#Weight
hist(`Pre-Weight`,col='#b4f1f1',xlab="Pre-Weight" , ylab="Frequency",xlim=c(60,100),ylim = c(0,50),main="")
hist(`Post weight`,col='#35bcbf',xlab="Post-Weight" ,xlim=c(60,100),ylim = c(0,50), ylab="",main="")

qqnorm(`Pre-Weight`)
qqline(`Pre-Weight`)
shapiro.test(`Pre-Weight`)

#Weight against age 
age_pre_weight = ggplot(Case_Study, aes(x=Age, y=`Pre-Weight`)) +geom_point() 
age_post_weight = ggplot(Case_Study,aes(x=Age,y=`Post weight`)) + geom_point()

p5 = ggMarginal(p = age_pre_weight,type = 'histogram',margins = 'both',size = 5,colour = 'gray98',fill = '#b4f1f1')
p6 = ggMarginal(p = age_post_weight,type = 'histogram',margins = 'both',size = 5,colour = 'gray98',fill = '#35bcbf')
p5
p6

cor(`Pre-Weight`,Age)
cor(`Post weight`,Age)

#weights against gender
boxplot(`Pre-Weight`~Gender,col=c("steelblue", "navyblue","grey"))
boxplot(`Post weight`~Gender,col=c("steelblue", "navyblue","grey"))

kruskal.test(`Pre-Weight`,Gender)

#weight against ethnicity
boxplot(`Pre-Weight`~Ethnicity,col=c("#0dad8d", "#30bfbf","#0c98ba","#1164b4"))
boxplot(`Post weight`~Ethnicity,col=c("#0dad8d", "#30bfbf","#0c98ba","#1164b4"))

kruskal.test(`Pre-Weight`,Ethnicity)


##Correlations between health risk factors and health outcomes [Obesity and hypertention with BMI, Height,Weight]

#Hypertension
boxplot(`Post BMI`~`Post-Hypertension`,col=c("#fec9c9","#afc7d0"))
boxplot(`Post weight`~`Post-Hypertension`,col=c("#fec9c9","#afc7d0"))
boxplot(`Post- height`~`Post-Hypertension`,col=c("#fec9c9","#afc7d0"))

#n1 = smaller sample --> hypertension - NO = 128
#n2 = larger sample --> hypertension - YES = 162
wilcox.test(`Post BMI`~`Post-Hypertension`,mu=0,alternative = "two.sided",
            conf.int=T,conf.level = 0.95,paired=F,exact = F, correct = T)
wilcox.test(`Post- height`~`Post-Hypertension`,mu=0,alternative = "two.sided",
            conf.int=T,conf.level = 0.95,paired=F,exact = F, correct = T)
wilcox.test(`Post weight`~`Post-Hypertension`,mu=0,alternative = "two.sided",
            conf.int=T,conf.level = 0.95,paired=F,exact = F, correct = T)

#Obesity
boxplot(`Post BMI`~`Post-Obesity`,col=c("#fec9c9","#afc7d0"))
boxplot(`Post weight`~`Post-Obesity`,col=c("#fec9c9","#afc7d0"))
boxplot(`Post- height`~`Post-Obesity`,col=c("#fec9c9","#afc7d0"))

#n1 = smaller sample --> obesity - NO = 128
#n2 = larger sample --> obesity - YES = 162
wilcox.test(`Post BMI`~`Post-Obesity`,mu=0,alternative = "two.sided",
            conf.int=T,conf.level = 0.95,paired=F,exact = F, correct = T)
wilcox.test(`Post- height`~`Post-Obesity`,mu=0,alternative = "two.sided",
            conf.int=T,conf.level = 0.95,paired=F,exact = F, correct = T)
wilcox.test(`Post weight`~`Post-Obesity`,mu=0,alternative = "two.sided",
            conf.int=T,conf.level = 0.95,paired=F,exact = F, correct = T)

library(dplyr)
post_data = data.frame(`Post BMI`,`Post weight`,`Post- height`,`Post-Hypertension`,`Post-Obesity`,Gender)

#Recoding post-hypertension : Yes = 1 and No = 0
coded_hyp = recode(post_data$Post.Hypertension,Yes = 1,No = 0)
#Recoding post-obesity : Yes = 1 and No = 0
coded_obe = recode(post_data$Post.Obesity,Yes = 1,No = 0)
post_data_coded= cbind(post_data,coded_hyp,coded_obe)

cor.test(post_data_coded$coded_hyp,post_data_coded$Post.BMI)
cor.test(post_data_coded$coded_hyp,post_data_coded$Post.weight)
cor.test(post_data_coded$coded_hyp,post_data_coded$Post..height)

cor.test(post_data_coded$coded_obe,post_data_coded$Post.BMI)
cor.test(post_data_coded$coded_obe,post_data_coded$Post.weight)
cor.test(post_data_coded$coded_obe,post_data_coded$Post..height)


