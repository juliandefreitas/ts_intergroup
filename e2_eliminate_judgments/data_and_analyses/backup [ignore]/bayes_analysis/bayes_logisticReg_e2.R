#Questions
#1. Same issue with last analysis. Is this worth reporting? 

myWd <- "/Users/Julian/Dropbox (Personal)/Research/socialPsych/intergroupTS/intergroupTS_onlineMaterials/e2_eliminate_judgments/data_and_analyses/bayes_analysis"
setwd(myWd)
install.packages("conting")
library("conting")

#----------------- Experiment 2 ----------------#
ex2 <- read.csv("e2_tsIntergroup.csv")

formulas2 <- as.formula(c(
    toupper("ts_forced_recode ~ cond_name + eth_cond_name + order + cond_name * order + order * eth_cond_name + cond_name * eth_cond_name * order"),
    toupper("ts_forced_recode ~ cond_name + eth_cond_name + order + cond_name * order + order * eth_cond_name"),
    toupper("ts_forced_recode ~ cond_name + eth_cond_name + order + order * eth_cond_name"),
    toupper("ts_forced_recode ~ cond_name + eth_cond_name + order + cond_name * order"),
    toupper("ts_forced_recode ~ cond_name + eth_cond_name + order + cond_name * eth_cond_name"),
    toupper("ts_forced_recode ~ cond_name + eth_cond_name + cond_name * eth_cond_name"),
    toupper("ts_forced_recode ~ cond_name + eth_cond_name + order + order * eth_cond_name"),
    toupper("ts_forced_recode ~ cond_name + eth_cond_name + order + cond_name * eth_cond_name"),
    toupper("ts_forced_recode ~ cond_name + eth_cond_name + order + cond_name * order"),
    toupper("ts_forced_recode ~ cond_name + eth_cond_name + order"),
    toupper("ts_forced_recode ~ cond_name + eth_cond_name"))
)

anthonyObject2 <- bcct(formula=formulas2, data=ex2, n.sample=5000, a=-1, b=0, prior="UIP")
anthonySummary2 <- mod_probs(anthonyObject2, scale=0, best=10)

if (anthonySummary2$totmodsvisit==1) {
    anthonySummary2$table$bf10 <- 1
} else if (anthonySummary2$totmodsvisit > 1) {
    anthonySummary2$table$bf10 <- anthonySummary2$table$prob/max(anthonySummary2$table$prob)
}
anthonySummary2

#---------------------------------------------------------------------------
# [I DON'T UNDERSTAND THIS PART]

# Here I wanted to check whether "order" really matters or not and I guess it does
# 
formulas2b <- as.formula(c(
    toupper("ts_forced_recode ~ cond_name + eth_cond_name + order"),
    toupper("ts_forced_recode ~ cond_name + eth_cond_name"))
)

anthonyObject2b <- bcct(formula=formulas2b, data=ex2, n.sample=5000, a=-1, b=0, prior="UIP")
anthonySummary2b <- mod_probs(anthonyObject2b, scale=0, best=10)

if (anthonySummary2b$totmodsvisit==1) {
    anthonySummary2b$table$bf10 <- 1
} else if (anthonySummary2b$totmodsvisit > 1) {
    anthonySummary2b$table$bf10 <- anthonySummary2b$table$prob/max(anthonySummary2b$table$prob)
}
anthonySummary2b


