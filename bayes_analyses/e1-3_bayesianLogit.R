setwd("/Users/Julian/Dropbox (Personal)/Research/socialPsych/intergroupTS/intergroupTS_onlineMaterials/bayes_analyses")
if (!require(conting)) {install.packages("conting"); require(conting)}

# (FYI: For ease of use, this code will soon be implemented in a new release of JASP: https://jasp-stats.org)

## =========================== Experiment 1 ===========================

ex1 <- read.csv("e1_tsintergroup.csv")

set.seed(1) 
# "cond_name" and "eth_cond_name" are nuisance, 
# only testing whether the interaction cond_name*eth_cond_name matters
formulas1 <- as.formula(c(
    "ts_forced_recode ~ cond_name + eth_cond_name + cond_name*eth_cond_name",
    "ts_forced_recode ~ cond_name + eth_cond_name")
)

anthonyObject1 <- bcct(formula=formulas1, data=ex1, n.sample=5000, a=-1, b=0, prior="UIP")
anthonyObject1 <- bcctu(object = anthonyObject1, n.sample = 5000)
anthonySummary1 <- mod_probs(anthonyObject1, scale=0, best=10)
anthonySummary1


0.9976/0.0024 #415.667 .

## ======================== Experiment 2 ===============================

ex2 <- read.csv("e2_tsIntergroup.csv")

set.seed(1) ## Set seed for reproducibility
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
anthonySummary2 <- mod_probs(anthonyObject2, scale=0, best=12)

anthonySummary2

0.70/0.18 #3.9

## ======================== Experiment 3 =================================

ex3 <- read.csv("e3_tsIntergroup_bayes.csv")

# 
set.seed(1) ## Set seed for reproducibility
formulas3 <- as.formula(c(
    "ts_forced_recode ~ vignette_name",
    "ts_forced_recode ~ order_name",
    "ts_forced_recode ~ eth_name",
    "ts_forced_recode")
)

anthonyObject3 <- bcct(formula=formulas3, data=ex3, n.sample=5000, a=-1, b=0, prior="UIP")

firstTime <- proc.time()
for (j in 1:120) {
    startTime <- proc.time()
    for (i in 1:10) {
        anthonyObject3 <- bcctu(object = anthonyObject3, n.sample = 5000)
    }
    endTime <- proc.time()
    
    print(endTime-startTime)
}
lastTime <- proc.time()

anthonyObject3
(lastTime-firstTime)/60

anthonySummary3 <- try(conting::mod_probs(anthonyObject3, scale=0, best=10), silent=TRUE)
anthonySummary3
