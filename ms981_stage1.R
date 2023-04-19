# Clear the workspace
rm(list=ls())

library(dplyr); library(tidyr); library(stargazer)
setwd("C:/Users/HP/OneDrive/EEPSEA_EfD/Coffee_DakLak MS981/Data/Stage1")

mainD <-read.delim("EfD21stage1.tab", header = TRUE, sep = "\t", quote = "")

mainD = mainD %>% mutate(score1 = ifelse(K_Q1 ==3, 1, 0 ), score2 = ifelse(K_Q2 ==3, 1, 0 ),
                         score3 = ifelse(K_Q3 ==3, 1, 0 ), score4 = ifelse(K_Q4__2 ==1, 1, 0 ),
                         score5 = ifelse(K_Q5 ==3, 1, 0 ), score6 = ifelse(K_Q6 ==4, 1, 0 ), 
                         score7 = ifelse(K_Q7 ==4, 1, 0 ), score8 = ifelse(K_Q8 ==1, 1, 0 ), 
                         score9 = ifelse(K_Q9 ==1, 1, 0 ), score10 = ifelse(K_Q10 ==1, 1, 0),
                         score11 = ifelse(K_Q11 ==4, 1, 0 ), score12 = ifelse(K_Q12 ==2, 1, 0 ),
                         score13 = ifelse(K_Q13 ==2, 1, 0 ), score14 = ifelse(K_Q14 ==2, 1, 0 ), 
                         score15 = ifelse(K_Q15 ==2, 1, 0 ), score16 = ifelse(K_Q16 ==4, 1, 0 ), 
                         score17 = ifelse(K_Q17 ==3, 1, 0 ), score18 = ifelse(K_Q18 ==2, 1, 0 ),
                         score19 = ifelse(K_Q19 ==3, 1, 0 ), score20 = ifelse(K_Q20 ==3, 1, 0 ),
                         
  score = score1 + score2 + score3 + score4 + score5 + score6 + score7 + score8 + score9 + score10 + 
    score11 + score12 + score13 + score14 + score15 + score16 + score17 + score18 + score19 + score20)


with(mainD, summary(score))
with(mainD, table(K_Q20))
stargazer(subset(mainD %>% filter(Guess_self>0), 
                 select= c("score1","score2","score3", "score4", "score5", "score6", "score7","score8",
                           "score9", "score10", "score11", "score12", "score13", "score14", "score15",
                           "score16", "score17", "score18", "score19", "score20", "score",
                           "Guess_self", "Guess_spouse", "Guess_women", "Guess_men")), 
          type = "text", digits = 3)

