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

## get data
dir <- setwd("/Users/Julian/Dropbox (Personal)/Research/socialPsych/intergroupTS/intergroupTS_onlineMaterials/e2_eliminate_judgments/data_and_analyses")
data <- read.csv("exp2_eliminate_judgments.csv")
dim(data) #1327

## remove unnecessary columns
data <- data[,c(9,10,403:421)] 

## exclusions: only incl. white subjects who answered attention and comprehension checks correctly
data <- subset(data,(data$eth_ss == 6 & data$exclude == 0))
dim(data) #759

## assign variable names
threat <- as.numeric(data$THREAT.43)
attitude <- as.numeric(data$ATTITUDE.43)
identification <-as.numeric(data$IDENTIFICATION)
ts_scale <- as.numeric(data$TS_SCALE)
ts_forced <- as.factor(data$TS_FORCED_RECODE)
cond <- as.factor(data$COND_NAME)
cond_num <- as.factor(data$COND)
vignette <- as.factor(data$VIGNETTE_NAME)
vignette_num <- as.factor(data$VIGNETTE)
eth <- as.factor(data$ETH_COND_NAME)
eth_num <- as.factor(data$ETH_COND)
order <- as.factor(data$ORDER)
gender <- as.factor(data$Gender)
age <- as.numeric(data$Age)

n_immigrant <- table(eth)[1]
n_white <- table(eth)[2]

## mean age and gender
mean(age,na.rm = TRUE) #37.14
table(gender)[2]/sum(table(gender)) #56.55%

## Cronbach's alpha, identification items 
ident_mat_white <- array(0,dim=c(n_white,3))
ident_mat_white[,1] <- data$value.5[data$ETH_COND_NAME == "white"]
ident_mat_white[,2] <- data$like.5[data$ETH_COND_NAME == "white"]
ident_mat_white[,3] <- data$connected.5[data$ETH_COND_NAME == "white"]
cronbach(ident_mat_white) #0.93

ident_mat_immigrant <- array(0,dim=c(n_immigrant,3))
ident_mat_immigrant[,1] <- data$value.5[data$ETH_COND_NAME == "immigrant"]
ident_mat_immigrant[,2] <- data$like.5[data$ETH_COND_NAME == "immigrant"]
ident_mat_immigrant[,3] <- data$connected.5[data$ETH_COND_NAME == "immigrant"]
cronbach(ident_mat_immigrant) #0.89

##========================= true self forced choice =====================================

ts_forced_glm_1 <- glm(ts_forced ~ cond_num, family = binomial)
summary(ts_forced_glm_1)
ts_forced_glm_2 <- glm(ts_forced ~ cond_num*eth_num, family = binomial)
summary(ts_forced_glm_2)
ts_forced_glm_3 <- glm(ts_forced ~ cond_num*eth_num*vignette_num, family = binomial)
summary(ts_forced_glm_3)

lrtest(ts_forced_glm_1,ts_forced_glm_2)
lrtest(ts_forced_glm_1,ts_forced_glm_3)

length(ts_forced[ts_forced == 1 & cond_num == 1])/length(ts_forced[cond_num == 1]) #proportion of improvement condition attributed to ts
length(ts_forced[ts_forced == 1 & cond_num == 2])/length(ts_forced[cond_num == 2]) #proportion of deterioration condition attributed to ts

##========================= true self scaled rating =====================================

ts_scale_aov <- aov(ts_scale ~ cond_num*vignette_num*eth_num*order, data = data)
summary(ts_scale_aov)
etaSquared(ts_scale_aov) 

#adjusted alphas
cond_aa_ts <- 0.05/15
vignette_aa_ts <- 0.05/14
eth_aa_ts <- 0.05/13

n_det <- table(cond)[1]
n_imp <- table(cond)[2]

ts_scale_mean <- tapply(ts_scale,cond,mean); ts_scale_mean
ts_scale_sd <- tapply(ts_scale,cond,sd); ts_scale_sd
ts_scaled_t <- t.test(ts_scale ~ cond, var.equal=TRUE, paired=FALSE); ts_scaled_t
tes(as.numeric(ts_scaled_t[1]), n_det, n_imp) #cohen's d

##====================== plot true self scaled rating ========================

alpha <- 0.05
condNames <- c("Improvement","Deterioration")
ethNames <- c("White", "Immigrant")

## condition effect by vignette
ts_vignettes <- matrix(NA,12,6)
colnames(ts_vignettes) <- c('cond','vignette','mean','sd','n','sem') 

count <- 1
for(i in 1:2) {
  for(k in 1:6) {
    ts_vignettes[count,] <- c(i,k,mean(ts_scale[cond_num == i & vignette_num == k]),sd(ts_scale[cond_num == i & vignette_num == k]),length(ts_scale[cond_num == i & vignette_num == k]),0)
    ts_vignettes[count,6] <- ts_vignettes[count,4]/sqrt(ts_vignettes[count,5]) #calculate standard error from mean
    count = count+1; 
  }
}
ts_vignettes.summary <- as.data.frame(ts_vignettes, stringsAsFactors=F)

## effect of condition by vignette (fig. S1)
title <- c("Effct of Condition by Vignette")
p1<-ggplot(ts_vignettes.summary,aes(x=factor(cond),y=mean,fill=factor(vignette))) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar")+theme_bw()+coord_cartesian(ylim=c(1,9))+scale_y_discrete(limits=c("1","2","3","4","5","6","7","8","9"))  
p1 <- p1+theme(text = element_text(size=16),panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
geom_errorbar(aes(ymax=mean+sem, ymin=mean-sem), position="dodge")+ggtitle(title)+scale_x_discrete(breaks = 1:length(condNames), labels=condNames)+
  xlab("Condition")+ylab("Mean True Self Attributions")+scale_fill_discrete(name="Vignette",labels=c("Caring/deadbeat father","Against/support terrorism","Ethical/unethical businessman","Teetotaler/alcoholic","Respect/mistreat minorities","Honest/corrupt officer"))+theme(legend.key=element_blank())
p1

##=========================== threat ratings =================================

n_immigrant_1 = table(eth,order)[1,1]
n_white_1 = table(eth,order)[2,1]
n_immigrant_2 = table(eth,order)[1,2]
n_white_2 = table(eth,order)[2,2]

#How much do you agree: People from white/Arab backgrounds like Al/Alhadin threaten the 
#american way of life
threat_aov <- aov(threat ~ vignette_num*eth_num*cond_num*order, data = data)
summary(threat_aov)
etaSquared(threat_aov) 

#adjusted alphas
eth_by_order_aa_threat <- 0.05/15; eth_by_order_aa_threat
order_aa_threat <- 0.05/14; order_aa_threat
eth_aa_threat <- 0.05/13; eth_aa_threat
vignette_aa_threat <- 0.05/12; vignette_aa_threat
vignette_order_aa_threat <- 0.05/11; vignette_order_aa_threat

## intergroup measures first
threat_1_mean <- tapply(threat[order==1],eth[order==1],mean); threat_1_mean
threat_1_sd <- tapply(threat[order==1],eth[order==1],sd); threat_1_sd
threat_1_whiteVimmigrant_t <- t.test(threat[order==1] ~ eth[order==1], var.equal=TRUE, paired=FALSE); threat_1_whiteVimmigrant_t
tes(as.numeric(threat_1_whiteVimmigrant_t[1]), n_white_1, n_immigrant_1) #cohen's d

## true self measures first
threat_2_mean <- tapply(threat[order==2],eth[order==2],mean); threat_2_mean
threat_2_sd <- tapply(threat[order==2],eth[order==2],sd); threat_2_sd
threat_2_whiteVimmigrant_t <- t.test(threat[order==2] ~ eth[order==2], var.equal=TRUE, paired=FALSE); threat_2_whiteVimmigrant_t
tes(as.numeric(threat_2_whiteVimmigrant_t[1]), n_white_2, n_immigrant_2) #cohen's d

##=========================== plot threat ratings ===================================

alpha <- 0.05
ethCondNames <- c("White","Immigrant")
out_mat_names <- c('threat','attitude','identification')

#OVERALL
outgroup_mat_overall <- array(0,dim=c(4,7,3)) #averaging across vignettes
colnames(outgroup_mat_overall) <- c('eth','order','mean','sd','n','sem','me') 

count <- 1
for (h in 1:3) {
  count = 1
      for(k in 1:2) {
        for(l in 1:2) {
          outgroup_mat_overall[count,,h] <- c(k,l,mean(get(out_mat_names[h])[eth_num == k & order == l]),sd(get(out_mat_names[h])[eth_num == k & order == l]),length(get(out_mat_names[h])[eth_num == k & order == l]),0,0)
          outgroup_mat_overall[count,6,h] <- outgroup_mat_overall[count,4,h]/sqrt(outgroup_mat_overall[count,5,h]) #calculate standard error from mean
          outgroup_mat_overall[count,7,h] <- qt(1-alpha/2, df=outgroup_mat_overall[count,5,h]*outgroup_mat_overall[count,6,h]) #calculate margin of error
          count = count+1 
      }
    }
  }

outgroup_mat.overall <- as.data.frame(outgroup_mat_overall, stringsAsFactors=F)
colnames(outgroup_mat.overall) <- c('eth','order','mean','sd','n','sem','me','eth','order','mean','sd','n','sem','me','eth','order','mean','sd','n','sem','me') 

## overall means (fig. 2)
p2<-ggplot(outgroup_mat.overall[,1:7],aes(x=factor(order),y=mean,fill=factor(eth)),color=factor(eth)) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar")+theme_bw()+coord_cartesian(ylim=c(1,9))+scale_y_discrete(limits=c("1","2","3","4","5","6","7","8","9")) 
p2<-p2+theme(text = element_text(size=16),panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  geom_errorbar(aes(ymax=mean+sem, ymin=mean-sem), position="dodge")+scale_x_discrete(breaks = 1:length(ethCondNames), labels=c("Threat First", "TS First"))+
  xlab("Order of Measures")+ylab("Mean Threat Ratings")+scale_fill_discrete(name="Ethnicity",labels=c("White","Immigrant"))+theme(legend.key=element_blank())
p2

##=========================== attitude ratings =================================

#How do you feel about people from white/Arab backgrounds like Al/Alhadin? (1=cold,9=warm)
attitude_aov <- aov(attitude ~ vignette_num*eth_num*cond_num*order, data = data)
summary(attitude_aov)
etaSquared(attitude_aov) 

#adjusted alphas
eth_order_aa_attitude <- 0.05/15; eth_order_aa_attitude
vignette_eth_cond_aa_attitude <- 0.05/14; vignette_eth_cond_aa_attitude
vignette_aa_attitude <- 0.05/13; vignette_aa_attitude

## intergroup measures first
attitude_1_mean <- tapply(attitude[order==1],eth[order==1],mean); attitude_1_mean
attitude_1_sd <- tapply(attitude[order==1],eth[order==1],sd); attitude_1_sd
attitude_1_whiteVimmigrant_t <- t.test(attitude[order==1] ~ eth[order==1], var.equal=TRUE, paired=FALSE); attitude_1_whiteVimmigrant_t
tes(as.numeric(attitude_1_whiteVimmigrant_t[1]), n_white_1, n_immigrant_1) #cohen's d

## true self measures first
attitude_2_mean <- tapply(attitude[order==2],eth[order==2],mean); attitude_2_mean
attitude_2_sd <- tapply(attitude[order==2],eth[order==2],sd); attitude_2_sd
attitude_2_whiteVimmigrant_t <- t.test(attitude[order==2] ~ eth[order==2], var.equal=TRUE, paired=FALSE); attitude_2_whiteVimmigrant_t
tes(as.numeric(attitude_2_whiteVimmigrant_t[1]), n_white_2, n_immigrant_2) #cohen's d

## overall means (fig. 2)
p3<-ggplot(outgroup_mat.overall[,8:14],aes(x=factor(order),y=mean,fill=factor(eth)),color=factor(eth)) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar")+theme_bw()+coord_cartesian(ylim=c(1,9))+scale_y_discrete(limits=c("1","2","3","4","5","6","7","8","9")) 
p3<- p3+theme(text = element_text(size=16),panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  geom_errorbar(aes(ymax=mean+sem, ymin=mean-sem), position="dodge")+scale_x_discrete(breaks = 1:length(ethCondNames), labels=c("Attitude First", "TS First"))+
  xlab("Order of Measures")+ylab("Mean Attitude Rating")+scale_fill_discrete(name="Ethnicity",labels=c("White","Immigrant"))+theme(legend.key=element_blank())
p3

##=========================== identification ratings =================================

# (3 questions averaged) How much do you agree: I like/value/feel connected to
# people from white/Arab backgrounds like Al/Alhadin
identification_aov <- aov(identification ~ vignette_num*eth_num*cond_num*order, data = data)
summary(identification_aov)
etaSquared(identification_aov) 

#adjusted alphas
eth_aa_identification <- 0.05/15; eth_aa_identification
eth_order_aa_identification <- 0.05/14; eth_order_aa_identification

## intergroup measures first
identification_1_mean <- tapply(identification[order==1],eth[order==1],mean); identification_1_mean
identification_1_sd <- tapply(identification[order==1],eth[order==1],sd); identification_1_sd
identification_1_whiteVimmigrant_t <- t.test(identification[order==1] ~ eth[order==1], var.equal=TRUE, paired=FALSE); identification_1_whiteVimmigrant_t
tes(as.numeric(identification_1_whiteVimmigrant_t[1]), n_white_1, n_immigrant_1) #cohen's d

## true self measures first
identification_2_mean <- tapply(identification[order==2],eth[order==2],mean); identification_2_mean
identification_2_sd <- tapply(identification[order==2],eth[order==2],sd); identification_2_sd
identification_2_whiteVimmigrant_t <- t.test(identification[order==2] ~ eth[order==2], var.equal=TRUE, paired=FALSE); identification_2_whiteVimmigrant_t
tes(as.numeric(identification_2_whiteVimmigrant_t[1]), n_white_2, n_immigrant_2) #cohen's d

## overall means (fig. 2)
p9<-ggplot(outgroup_mat.overall[,15:21],aes(x=factor(order),y=mean,fill=factor(eth)),color=factor(eth)) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar")+theme_bw()+coord_cartesian(ylim=c(1,9))+scale_y_discrete(limits=c("1","2","3","4","5","6","7","8","9")) 
p9<- p9+theme(text = element_text(size=16),panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  geom_errorbar(aes(ymax=mean+sem, ymin=mean-sem), position="dodge")+scale_x_discrete(breaks = 1:length(ethCondNames), labels=c("Identification First", "TS First"))+
  xlab("Order of Measures")+ylab("Mean Identification Rating")+scale_fill_discrete(name="Ethnicity",labels=c("White","Immigrant"))+theme(legend.key=element_blank())
p9

###=============================== plot - forced choice =============================

accuracyTable <- matrix(NA,2,2)
rownames(accuracyTable) <- c('White','Arab Immigrant')
colnames(accuracyTable) <- c('Improvement','Deterioration')
accuracyTable[1,1] <- sum(as.numeric(ts_forced[eth_num == 1 & cond_num == 1])-1)/length(ts_forced[eth_num == 1 & cond_num == 1])
accuracyTable[1,2] <- sum(as.numeric(ts_forced[eth_num == 1 & cond_num == 2])-1)/length(ts_forced[eth_num == 1 & cond_num == 2])
accuracyTable[2,1] <- sum(as.numeric(ts_forced[eth_num == 2 & cond_num == 1])-1)/length(ts_forced[eth_num == 2 & cond_num == 1])
accuracyTable[2,2] <- sum(as.numeric(ts_forced[eth_num == 2 & cond_num == 2])-1)/length(ts_forced[eth_num == 2 & cond_num == 2])

barplot(accuracyTable, beside=TRUE,ylim=c(0,1.0), col=c("red", "green"),
        legend.text=TRUE, ylab="Proportion of Participants Attributing Change to True Self",main="True Self Bias Forced Choice Items")

####======================================= end ========================================
