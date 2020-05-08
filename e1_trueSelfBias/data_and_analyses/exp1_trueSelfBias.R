## 6 July 2016
## Author: Julian De Freitas, Harvard University

## clear workpace  
rm(list = ls()) 

## necessary packages
if (!require(ggplot2)) {install.packages("ggplot2"); require(ggplot2)}
if (!require(plyr)) {install.packages("plyr"); require(plyr)}
if (!require(lsr)) {install.packages("lsr"); require(lsr)}
if (!require(psy)) {install.packages("psy"); require(psy)}
if (!require(compute.es)) {install.packages("compute.es"); require(compute.es)}
if (!require(lmtest)) {install.packages("lmtest"); require(lmtest)}
if (!require(bbmle)) {install.packages("bbmle"); require(bbmle)}
if (!require(scales)) {install.packages("scales"); require(scales)}

## get data
dir <- setwd("/Users/Julian/Dropbox (Personal)/Research/socialPsych/intergroupTS/intergroupTS_onlineMaterials/e1_trueSelfBias/data_and_analyses")
data <- read.csv("exp1_trueSelfBias.csv")
dim(data) # 1020

## get rid of unnecessary rows
data <- data[,c(10,11,344:362)] 

## exclusions: only incl. white subjects who answered attention and comprehension questions correctly
data <- subset(data,(data$ethn_ss == 6 & data$exclude == 0)) 
dim(data) # 613

## assign variable names
threat <- as.numeric(data$threat.28)
attitude <- as.numeric(data$attitude.34)
identification <-as.numeric(data$identification)
ts_scale <- as.numeric(data$ts_scaled)
ts_forced <- as.factor(data$ts_forced_recode)
cond <- as.factor(data$cond_name)
cond_num <- as.factor(data$condition)
vignette <- as.factor(data$vignette_name)
vignette_num <- as.factor(data$vignette)
eth <- as.factor(data$eth_cond_name)
eth_num <- as.factor(data$eth_cond)
gender <- as.factor(data$Gender)
age <- as.numeric(levels(data$Age))[data$Age]

table(eth,cond)

## Mean age and gender
mean(age,na.rm = TRUE) #35.95
table(gender)[2]/sum(table(gender)) #53.51%

##===== Aside: Export data for bayes analysis to be conducted in separate script ======

#dataExport <- data[,c(7,9,11,19,20)]
#dataExport <- as.data.frame(dataExport)
#write.csv(dataExport,"e1_tsIntergroup.csv")

##========================= comparison of english vs. arab names =======================

#Al v Alhadin
#Daniel v Danyal
#Harris v Haris
#Jeffrey v Jafri
#Chad v Chahid
#Adam v Adnan

#syallables
english_syllables <- c(1,2,2,2,1,2)
arab_syllables <- c(3,2,2,2,2,2)
syllable_test <- t.test(english_syllables,arab_syllables) #t = -1.86, p = .094
tes(as.numeric(syllable_test[1]), 6, 6) #cohen's d = 1.07

#characters
english_characters <- c(2,6,6,7,4,4)
arab_characters <- c(7,6,5,5,6,5)
character_test <- t.test(english_characters, arab_characters) #t = -1.02, p = .344
tes(as.numeric(character_test[1]), 6, 6) #cohen's d = 0.59

#vowels
english_vowels <- c(1,3,2,2,1,2)
arab_vowels <- c(3,2,2,2,2,2)
vowel_test <- t.test(english_vowels, arab_vowels) #t = 0.95, p=.369
tes(as.numeric(vowel_test[1]),6, 6) #cohen's d = 0.55

#consonants
english_consonants <- c(1,3,4,5,4,2)
arab_consonants <- c(4,4,3,3,4,3)
consonant_test <- t.test(english_consonants,arab_consonants) #t = 0.52, p=.621
tes(as.numeric(consonant_test[1]), 6, 6) #cohen's d = 0.30

##================ Cronbach's alpha, identification items ==================

ident_mat_white <- array(0,dim=c(219,3))
ident_mat_white[,1] <- data$value.36[data$eth_cond_name == "white"]
ident_mat_white[,2] <- data$like.36[data$eth_cond_name == "white"]
ident_mat_white[,3] <- data$connected.36[data$eth_cond_name == "white"]
cronbach(ident_mat_white) #0.86

ident_mat_native <- array(0,dim=c(194,3))
ident_mat_native[,1] <- data$value.36[data$eth_cond_name == "syrian"]
ident_mat_native[,2] <- data$like.36[data$eth_cond_name == "syrian"]
ident_mat_native[,3] <- data$connected.36[data$eth_cond_name == "syrian"]
cronbach(ident_mat_native) #0.88

ident_mat_immigrant <- array(0,dim=c(200,3))
ident_mat_immigrant[,1] <- data$value.36[data$eth_cond_name == "immigrant"]
ident_mat_immigrant[,2] <- data$like.36[data$eth_cond_name == "immigrant"]
ident_mat_immigrant[,3] <- data$connected.36[data$eth_cond_name == "immigrant"]
cronbach(ident_mat_immigrant) #0.90

##=========================== threat ratings ================================

ethCondNames <- c("White American","Arab Immigrant","Arab Native")
n_white <- table(eth)[3]; n_white
n_native <- table(eth)[2]; n_native
n_immigrant <- table(eth)[1]; n_immigrant

#"How much do you agree: People from white/Arab backgrounds like Al/Alhadin threaten the 
# american way of life?"
threat_aov <- aov(threat ~ eth_num, data = data)
summary(threat_aov)
etaSquared(threat_aov) 

threat_mean <- tapply(threat,eth,mean); threat_mean
threat_sd <- tapply(threat,eth,sd); threat_sd

# white v. syrian 
threat_whiteVnative_t <- t.test(threat[eth=="white" | eth=="syrian"] ~ eth[eth=="white" | eth=="syrian"]); threat_whiteVnative_t
tes(as.numeric(threat_whiteVnative_t[1]), n_white, n_native) #cohen's d

# white v. immigrant
threat_whiteVimmigrant_t <- t.test(threat[eth=="white" | eth=="immigrant"] ~ eth[eth=="white" | eth=="immigrant"], var.equal=TRUE, paired=FALSE); threat_whiteVimmigrant_t
tes(as.numeric(threat_whiteVimmigrant_t[1]), n_white, n_immigrant) #cohen's d

##=========================== attitude ratings ================================

# "How do you feel about people from white/Arab backgrounds like Al/Alhadin? (1=cold,9=warm)"
attitude_aov <- aov(attitude ~ eth_num, data = data)
summary(attitude_aov)
etaSquared(attitude_aov) 

attitude_mean <- tapply(attitude,eth,mean); attitude_mean
attitude_sd <- tapply(attitude,eth,sd); attitude_sd

# white v. syrian
attitude_whiteVnative_t <- t.test(attitude[eth=="white" | eth=="syrian"] ~ eth[eth=="white" | eth=="syrian"], var.equal=TRUE, paired=FALSE); attitude_whiteVnative_t
tes(as.numeric(attitude_whiteVnative_t[1]), n_white, n_native) #cohen's d

# white v. immigrant
attitude_whiteVimmigrant_t <- t.test(attitude[eth=="white" | eth=="immigrant"] ~ eth[eth=="white" | eth=="immigrant"], var.equal=TRUE, paired=FALSE); attitude_whiteVimmigrant_t
tes(as.numeric(attitude_whiteVimmigrant_t[1]), n_white, n_immigrant) #cohen's d

##=========================== identification ratings ================================

##(3 questions averaged) How much do you agree: I like/value/feel connected to
# people from white/Arab backgrounds like Al/Alhadin
identification_aov <- aov(identification ~ eth_num, data = data)
summary(identification_aov)
etaSquared(identification_aov)

identification_mean <- tapply(identification,eth,mean); identification_mean
identification_sd <- tapply(identification,eth,sd); identification_sd

#white v. syrian
identification_whiteVnative_t <- t.test(identification[eth=="white" | eth=="syrian"] ~ eth[eth=="white" | eth=="syrian"], var.equal=TRUE, paired=FALSE); identification_whiteVnative_t
tes(as.numeric(identification_whiteVnative_t[1]), n_white, n_native) #cohen's d

#white v. immigrant
identification_whiteVimmigrant_t <- t.test(identification[eth=="white" | eth=="immigrant"] ~ eth[eth=="white" | eth=="immigrant"], var.equal=TRUE, paired=FALSE); identification_whiteVimmigrant_t
tes(as.numeric(identification_whiteVimmigrant_t[1]), n_white, n_immigrant) #cohen's d

##========================= true self forced choice ===================================

length(ts_forced[ts_forced == 1 & cond_num == 1])/length(ts_forced[cond_num == 1]) #proportion of improvement condition attributed to ts
length(ts_forced[ts_forced == 1 & cond_num == 2])/length(ts_forced[cond_num == 2]) #proportion of deterioration condition attributed to ts

ts_forced_glm_1 <- glm(ts_forced ~ cond_num, family = binomial)
summary(ts_forced_glm_1)

#see separate script for bayesian logistic regression in R

##==================== plot - forced choice data (supp fig. 1) =====================

accuracyTable <- matrix(NA,3,2)
rownames(accuracyTable) <- c('White American','Arab Immigrant','Arab Native')
colnames(accuracyTable) <- c('Improvement','Deterioration')
accuracyTable[1,1] <- sum(as.numeric(ts_forced[eth_num == 1 & cond_num == 1])-1)/length(ts_forced[eth_num == 1 & cond_num == 1])
accuracyTable[1,2] <- sum(as.numeric(ts_forced[eth_num == 1 & cond_num == 2])-1)/length(ts_forced[eth_num == 1 & cond_num == 2])
accuracyTable[2,1] <- sum(as.numeric(ts_forced[eth_num == 2 & cond_num == 1])-1)/length(ts_forced[eth_num == 2 & cond_num == 1])
accuracyTable[2,2] <- sum(as.numeric(ts_forced[eth_num == 2 & cond_num == 2])-1)/length(ts_forced[eth_num == 2 & cond_num == 2])
accuracyTable[3,1] <- sum(as.numeric(ts_forced[eth_num == 3 & cond_num == 1])-1)/length(ts_forced[eth_num == 3 & cond_num == 1])
accuracyTable[3,2] <- sum(as.numeric(ts_forced[eth_num == 3 & cond_num == 2])-1)/length(ts_forced[eth_num == 3 & cond_num == 2])

colors <- hue_pal()(3)
p = barplot(accuracyTable, beside=TRUE,ylim=c(0,1.0), col=colors,cex.names=1.5,cex.main=1.2,cex.main=1.7,
cex.lab=1.5,legend.text=FALSE,ylab="Proportion of Participants Attributing Change to True Self",main="True Self Forced Choice Items", family = "Helvetica")
legend("top", legend = ethCondNames, fill = colors, bty="n",cex = 1.3)

##=========================== true self scaled ratings ================================

n_det <- table(cond)[1]
n_imp <- table(cond)[2]
condNames <- c("Improvement", "Deterioration")

ts_scale_aov <- aov(ts_scale ~ cond_num*vignette_num*eth_num, data = data)
summary(ts_scale_aov)
etaSquared(ts_scale_aov) # see second column

#adjusted alphas
cond_aa <- 0.05/7; cond_aa

ts_scale_mean <- tapply(ts_scale,cond,mean); ts_scale_mean
ts_scale_sd <- tapply(ts_scale,cond,sd); ts_scale_sd
ts_scaled_t <- t.test(ts_scale ~ cond, var.equal=TRUE, paired=FALSE); ts_scaled_t
tes(as.numeric(ts_scaled_t[1]), n_det, n_imp) #cohen's d

#see separate JASP file for bayesian anova

##========================= plot - true self scaled ratings (fig. 1) ========================

alpha <- .05
ts_scale_overall <- matrix(NA,6,7)
colnames(ts_scale_overall) <- c('cond','eth','mean','sd','n','sem','me') 

count <- 1
for(i in 1:2) {
  for(k in 1:3) {
    ts_scale_overall[count,] <- c(i,k,mean(ts_scale[cond_num == i & eth_num == k]),sd(ts_scale[cond_num == i & eth_num == k]),length(ts_scale[cond_num == i & eth_num == k]),0,0)
    ts_scale_overall[count,6] <- ts_scale_overall[count,4]/sqrt(ts_scale_overall[count,5]) #calculate standard error from mean
    ts_scale_overall[count,7] <- qt(1-alpha/2, df=ts_scale_overall[count,5]*ts_scale_overall[count,6]) #calculate margin of error
    count = count+1; 
  }
}
ts_scale.overall <- as.data.frame(ts_scale_overall, stringsAsFactors=F)

p1<-ggplot(ts_scale.overall,aes(x=factor(cond),y=mean,fill=factor(eth))) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar")+theme_bw()+coord_cartesian(ylim=c(1,1,9))+scale_y_discrete(limits=c("1","2","3","4","5","6","7","8","9"))
p1 <- p1+theme(text = element_text(size=16),panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  geom_errorbar(aes(ymax=mean+sem, ymin=mean-sem), position="dodge")+scale_x_discrete(breaks = 1:length(condNames), labels=condNames)+
  xlab("Condition")+ylab("Mean")+scale_fill_discrete(name="Ethnicity",labels=c("White","Arab Immigrant","Arab Native"))+theme(legend.key=element_blank())
p1

##========================================== end ===========================================

