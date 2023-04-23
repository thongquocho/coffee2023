# Clear the workspace
rm(list=ls())

library(dplyr); library(tidyr); library(stargazer)


mainD <-read.csv("data_cleaned.csv")

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
                           score11 + score12 + score13 + score14 + score15 + score16 + score17 + score18 + score19 + score20,
                         wife = ifelse(husband_wife ==1, 1, 0 ),
                         pri_edu = ifelse(edu <3, 1, 0), sec_edu = ifelse(edu ==3, 1, 0), hig_edu = ifelse(edu>3, 1, 0))


with(mainD, summary(score))
with(mainD, table(K_Q20))
# summary statistics for all respondents

mainD %>% filter() %>% select(score1: score20, score, Guess_self: Guess_men) %>% 
  stargazer::stargazer(type = "text", digits = 3)


mainD %>% filter( )%>%
  select(Gen_Q1:Guess_women_Gen_Q8) %>% stargazer::stargazer(type = "text", digits = 3)


# summary statistics for husbands

mainD %>% filter(Guess_self>0, husband_wife == 2) %>% 
  select(score1:score20, score, Guess_self: Guess_men) %>%
  stargazer::stargazer(type = "text", digits = 3)


mainD %>% filter(husband_wife ==2  )%>%
  select(Gen_Q1:Guess_women_Gen_Q8) %>% stargazer::stargazer(type = "text", digits = 3)



# summary statistics for wives

mainD %>% filter(Guess_self>0, husband_wife == 1) %>% 
  select(score1:score20, score, Guess_self: Guess_men) %>%
  stargazer::stargazer(type = "text", digits = 3)


mainD %>% filter(husband_wife ==1  )%>%
  select(Gen_Q1:Guess_women_Gen_Q8) %>% stargazer::stargazer(type = "text", digits = 3)

with(mainD %>% filter(husband_wife ==1), plot(density(score)))
with(mainD, t.test(score ~ husband_wife))

mainD %>% group_by(husband_wife) %>% filter()%>% 
  summarise(sum_score = sum(score), av_score = mean(score), obs = n())

library(estimatr);library(magrittr)
m1 <- lm_robust(score ~ wife + familysize+ sec_edu + hig_edu , 
                clusters = hhid,
                data = mainD %>% filter()) # wives got lower scores

m2 <- lm_robust(Guess_self ~ wife + familysize+ sec_edu + hig_edu  , 
                clusters = hhid,
                data = mainD %>% filter()) # wives under-evaluate themselve
m3 <- lm_robust(Guess_spouse ~ wife + familysize+ sec_edu + hig_edu  , 
                clusters = hhid,
                data = mainD %>% filter()) # wives under-evaluate themselve

sjPlot::tab_model(m1, m2,m3, show.ci = FALSE)


dep_vars <- c("Gen_Q1", "Gen_Q2", "Gen_Q3", "Gen_Q4", "Gen_Q5", "Gen_Q6", 
              "Gen_Q7", "Gen_Q8")
ind_vars <- c("wife", "familysize", "sec_edu", "hig_edu")

# create an empty list to store the regression results
reg_results <- list()

# loop over each dependent variable and run a regression
for (i in 1:length(dep_vars)) {
  # create a formula for the regression
  formula <- paste(dep_vars[i], "~", paste(ind_vars, collapse = "+"))
  
  # run the regression using lm() function
  reg_results[[i]] <- lm(formula, clusters = hhid, data = mainD)
}

stargazer(reg_results, type = "text")

names(mainD)


with(mainD, table(number_well))



# production data 

mainD %>% filter(production >=0 )  %>%
  mutate(yield = 10* production/total_area, #yield: tons/ ha
         x1 = as.numeric(chem_fer1) + as.numeric(chem_fer2) + as.numeric(chem_fer3),
         x1b = x1/total_area,
         x2 = irrig_number*irrigation_amount*number_trees,
         x2b = 10*x2/total_area,
         x3 = family_lab + hired_lab + exchange_lab,
         x3b = 10000*x3/total_area,
         x4 = biofer_w + organicfer_w,
         x4b = (x4/total_area)/100,
         lny = log(production), lnx1 = log(x1), lnx2 = log(x2), lnx3 = log(x3),
         lnx4 = log(x4+0.001),
         trad_irr = ifelse(irrig_type==1, 1,0)) -> proD 

with(proD %>% filter(),
     summary(cbind(production, total_area, yield, x1b, x2b)))

with(proD%>% filter(), 
     plot(density(yield)))
with(proD%>% filter(), 
     boxplot(x1b))

proD %>% filter(x1b<.01 |x1b>.4 |x2b >3000 |x3b >400) %>% 
  select(hhid, district, phone, chem_fer1, chem_fer2, chem_fer3, yield,
         total_area, x2b, x3b)
proD %>% filter(x2b >2500) %>% 
  select(hhid, district, phone,  yield, total_area, x2b)

m4 <- lm_robust(lny ~ lnx1+lnx2+lnx3 + trad_irr + score, 
                clusters = hhid,
                data = proD %>% filter(production>0, x1>0, x2>0))


sjPlot::tab_model(m4, show.ci = FALSE)
str(proD)


summary(m4)
# BC95 model
require(frontier)
sfm = sfa(lny ~ lnx1 + lnx2 + lnx3 | trad_irr + score  + edu,  
          data = proD %>% filter(production>0, x1>0, x2>0, x3>0))
summary(sfm) 

