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
if (!require(broom)) {install.packages("broom"); require(broom)}
if (!require(bbmle)) {install.packages("bbmle"); require(bbmle)}

## get data
dir <- setwd("/Users/Julian/Dropbox (Personal)/Research/socialPsych/intergroupTS/intergroupTS_onlineMaterials/e3_eliminate_behavior/data_and_analyses")
data <- read.csv("e3_eliminate_behavior.csv")
head(data)
dim(data) # 678

## remove unnecessary columns
data <- data[,c(4,5,154:172)] 

## exclusions: only incl. white subjects who answered attention and comprehension checks correctly
data <- subset(data,(data$exclude == 0)) 
dim(data) #374

## Recode donation values that were coded weirdly (but consistently) in Qualtrics. Code from 1 = ARC to 11 - SARC
donationRecode_mat <- c(12,13,23,14,15,16,18,19,9,20,21)
for (i in 1:dim(data)[1]) {
  data$donation[i] <- match(data$donation[i],donationRecode_mat) 
}

#add ts_forced_recode
for (i in 1:dim(data)[1]) {
  if(data$ts_forced[i] == 1) {
    data$ts_forced_recode[i] = 1
  }
  else if(data$ts_forced[i] == 2 | data$ts_forced[i] == 3) {
    data$ts_forced_recode[i] = 2
  }
}

## Assign variable names
ts_time <- as.factor(data$ts_time)
ts_forced <- as.factor(data$ts_forced_recode)
ts_scaled <- as.numeric(data$ts_scale)
donation_time <- as.numeric(data$donation_time)
donation <- as.numeric(data$donation)
vignette <- as.factor(data$vignette_name)
vignette_num <- as.factor(data$vignette)
eth <- as.factor(data$eth)
eth_name <- as.factor(data$eth_name)
order <- as.factor(data$order)
order_name <- as.factor(data$order_name)
gender <- as.factor(data$Gender)
age <- as.numeric(data$Age)

table(eth,order) 
table(eth,order,vignette)

## Mean age and gender
mean(age,na.rm = TRUE) #36.71
table(gender)[2]/sum(table(gender)) #52.01%

##===== Aside: Export data for bayes analysis to be conducted in separate script ======

dataExport <- data[,c(5,7,9,12,22)]
write.csv(dataExport,"e3_tsIntergroup.csv")

##========================= true self forced choice =====================================

ts_forced_glm_1 <- glm(ts_forced ~ eth, family = 'binomial') 
summary(ts_f_model_1)
ts_forced_glm_2 <- glm(ts_forced ~ eth*order, family = 'binomial') 
summary(ts_f_model_2)
ts_forced_glm_3 <- glm(ts_forced ~ eth*order*vignette_num, family = 'binomial') 
summary(ts_f_model_3)

lrtest(ts_forced_glm_1, ts_forced_glm_2)
lrtest(ts_forced_glm_1, ts_forced_glm_3)

#Table S3
model.names <- c('1 eth','2 eth*order','3 eth*order*vignette')
summ.table <- do.call(rbind, lapply(list(ts_forced_glm_1, ts_forced_glm_2, ts_forced_glm_3), broom::glance))
table.cols <- c("df.residual", "deviance", "AIC")
reported.table <- summ.table[table.cols]
names(reported.table) <- c("Resid. Df", "Resid. Dev", "AIC")

reported.table[['dAIC']] <-  with(reported.table, AIC - min(AIC))
reported.table[['weight']] <- with(reported.table, exp(- 0.5 * dAIC) / sum(exp(- 0.5 * dAIC)))
reported.table$AIC <- NULL
reported.table$weight <- round(reported.table$weight, 2)
reported.table$dAIC <- round(reported.table$dAIC, 1)
row.names(reported.table) <- model.names
reported.table

#with bbmle
reported.table2 <- bbmle::AICtab(ts_forced_glm_1, ts_forced_glm_2, ts_forced_glm_3, weights = TRUE, sort = FALSE, mnames = model.names)
reported.table2[["Resid. Dev"]]  <- summ.table[["deviance"]] 
reported.table2

#--
length(ts_forced[ts_forced == 1 & eth == 1])/length(ts_forced[eth == 1]) #proportion of improvement condition attributed to ts
length(ts_forced[ts_forced == 1 & eth == 2])/length(ts_forced[eth == 2]) #proportion of deterioration condition attributed to ts

##========================= true self scaled =====================================

ts_s_model <- aov(ts_scale ~ eth*order*vignette_num, data = data) #ts scaled
summary(ts_s_model)
etaSquared(ts_s_model)

##===================================== donations =======================================

alpha <- 0.05
ethNames <- c("White","Immigrant")
orderNames <- c("Ts First", "Donation First")

n_order1 <- table(order)[1] #ts first
n_order2 <- table(order)[2] #donation first

donation_model <- aov(donation ~ eth*order*vignette_num, data = data)
summary(donation_model)
etaSquared(donation_model) 

#adjusted alphas
order_aa_donation <- 0.05/7; order_aa_donation

donation_mean <- tapply(donation,order,mean); donation_mean
donation_sd <- tapply(donation,order,sd); donation_sd
donation_t <- t.test(donation ~ order, var.equal=TRUE, paired=FALSE); donation_t
tes(as.numeric(donation_t[1]), n_order1, n_order2) #cohen's d

##================================== plot donations (fig. 3) ======================================

donation_mat_detailed <- array(0,dim=c(4,7)) 
colnames(donation_mat_detailed) <- c('row','order','eth','mean','sd','n','sem') 

donation_mat_detailed[1,] <- c(1,2,1,mean(donation[order == 2 & eth == 1]),sd(donation[order == 2 & eth == 1]),length(donation[order == 2 & eth == 1]),0)
donation_mat_detailed[2,] <- c(1,2,2,mean(donation[order == 2 & eth == 2] ),sd(donation[order == 2 & eth == 2]),length(donation[order == 2 & eth == 2]),0)
donation_mat_detailed[3,] <- c(2,1,1,mean(donation[order == 1 & eth == 1]),sd(donation[order == 1 & eth == 1]),length(donation[order == 1 & eth == 1]),0)
donation_mat_detailed[4,] <- c(2,1,2,mean(donation[order == 1 & eth == 2]),sd(donation[order == 1 & eth == 2]),length(donation[order == 1 & eth == 2]),0)

for (i in 1:4) {
  donation_mat_detailed[i,7] <- donation_mat_detailed[i,5]/sqrt(donation_mat_detailed[i,6]) 
}
donationDetailed.mat <- as.data.frame(donation_mat_detailed, stringsAsFactors=F)

title <- c("Donations")
p1<-ggplot(donationDetailed.mat,aes(x=factor(row),y=mean,fill=factor(eth))) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar")+theme_bw()+coord_cartesian(ylim=c(1,11))+scale_y_discrete(limits=c("0","10","20","30","40","50","60","70","80","90","100"))  
p1 <- p1+theme(text = element_text(size=16),panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  geom_errorbar(aes(ymax=mean+sem, ymin=mean-sem), position="dodge")+scale_x_discrete(breaks = 1:2, labels=c("Donation First", "TS First"))+
  xlab("Order of Measures")+ylab("Mean Donation (%) to Outgroup Charity")+scale_fill_discrete(name="Ethnicity",labels=c("White","Immigrant"))+theme(legend.key=element_blank())
p1

####======================================= end ========================================
