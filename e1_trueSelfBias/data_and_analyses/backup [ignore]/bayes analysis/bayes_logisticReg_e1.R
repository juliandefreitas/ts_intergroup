#questions: 
#1. When comparing cond name and eth, only cond shows up. How does this test work? 
  #-What statistic do I report
#2. When you try a different prior, you say that eth cond name wins again, even though you said
#2b found no support for it. 
#3. So, shall I just report the first analysis?

myWd <- "/Users/Julian/Dropbox (Personal)/Research/socialPsych/intergroupTS/intergroupTS_onlineMaterials/e1_trueSelfBias/data_and_analyses/bayes analysis"
setwd(myWd)
install.packages("conting")
library("conting")

ex1 <- read.csv("e1_tsintergroup.csv")

# "cond_name" and "eth_cond_name" are nuisance, 
# only testing whether the interaction cond_name*eth_cond_name matters
formulas1 <- as.formula(c(
    "ts_forced_recode ~ cond_name + eth_cond_name + cond_name*eth_cond_name",
    "ts_forced_recode ~ cond_name + eth_cond_name")
)

anthonyObject1 <- bcct(formula=formulas1, data=ex1, n.sample=5000, a=-1, b=0, prior="SBH")
anthonySummary1 <- mod_probs(anthonyObject1, scale=0, best=10)
anthonySummary1

#anthonySummary1$table$bf10 <- anthonySummary1$table$prob.Freq/(max(anthonySummary1$table$prob.Freq))
anthonySummary1$table$prob/max(anthonySummary1$table$prob) # This means that the first model wins
anthonySummary1 

#----------------------------------------------------------------------
# [I DON'T UNDERSTAND THIS PART]
  
# Only the intercept is a nuisance parameter
# Here I check whether "cond_name" or "eth_cond_name" is most important
# only testing whether the interaction cond_name*eth_cond_name matters
formulas1b <- as.formula(c(
    "ts_forced_recode ~ cond_name",
    "ts_forced_recode ~ eth_cond_name")
)

anthonyObject1b <- bcct(formula=formulas1b, data=ex1, n.sample=5000, a=-1, b=0, prior="SBH")
anthonySummary1b <- mod_probs(anthonyObject1b, best=10)

anthonySummary1b

if (anthonySummary1b$totmodsvisit==1) {
    anthonySummary1b$table$bf10 <- 1
}
anthonySummary1b

# Note that swapping the order, that is, letting the sampler start with "eth_cond_name" first 
# causes the sampler to crash [jdf: IT DOESN'T CRASH FOR ME]

formulas1c <- as.formula(c(
    "ts_forced_recode ~ eth_cond_name",
    "ts_forced_recode ~ cond_name")
)

# Note that I'm trying a different family of priors here, namely the unit information prior: Result eth_cond_name wins again
anthonyObject1c <- bcct(formula=formulas1c, data=ex1, n.sample=5000, a=-1, b=0, prior="UIP")
anthonySummary1c <- mod_probs(anthonyObject1c, scale=0, best=10)

if (anthonySummary1c$totmodsvisit==1) {
    anthonySummary1c$table$bf10 <- 1
}
anthonySummary1c


