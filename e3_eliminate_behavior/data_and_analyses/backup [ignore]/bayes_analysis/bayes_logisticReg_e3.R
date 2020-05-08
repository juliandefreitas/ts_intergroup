#Question
#1. Don't understand the output of anthonySummary3  

myWd <- "/Users/Julian/Dropbox (Personal)/Research/socialPsych/intergroupTS/intergroupTS_onlineMaterials/e3_eliminate_behavior/data_and_analyses/bayes_analysis"
setwd(myWd)
install.packages("conting")
library("conting")

# I actually don't know what you'd like to do here.
ex3 <- read.csv("e3_tsIntergroup_bayes.csv")
# 
formulas3 <- as.formula(c(
    "ts_forced_recode ~ vignette_name",
    "ts_forced_recode ~ order_name",
    "ts_forced_recode ~ eth_name",
    "ts_forced_recode ~ eth_name + order_name")
)

anthonyObject3 <- bcct(formula=formulas3, data=ex3, n.sample=5000, a=-1, b=0, prior="UIP")

anthonySummary3 <- try(conting::mod_probs(anthonyObject3, scale=0, best=10), silent=TRUE)
anthonySummary3

anthonySummary3$table$prob/max(anthonySummary3$table$prob)

