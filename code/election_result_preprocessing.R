rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(tidyverse)


#code from TK 
Election_Raw=read.csv("president_county_candidate.csv")
Turnout2020=read.csv("2020 November General Election - Turnout Rates.csv")
Election_county_ind=which(Election_Raw$candidate%in% candidates)
Election_county=Election_Raw[Election_county_ind,]
Election_sum=aggregate(Election_Raw$total_votes,by=list(state=Election_Raw$state),FUN=sum)
Candidates_tot=aggregate(list(total_votes=Election_Raw$total_votes),by=list(state=Election_Raw$state,candidate=Election_Raw$candidate),FUN=sum)
Candidates_tot=Candidates_tot[which(Candidates_tot$candidate%in%c("Donald Trump","Joe Biden")),]
Candidates=cbind(Candidates_tot[1:51,-2],Candidates_tot[-c(1:51),3])
colnames(Candidates)=c("State","Trump","Biden")
Candidates$ratio=Candidates$Trump/Candidates$Biden
write.csv(Candidates,"election_result.csv")

Turnout2020=read.csv("data/2020 November General Election - Turnout Rates.csv", header = FALSE, skip = 2)
colnames(Turnout2020) = c("State", 'Source', 'Official', 
                          'Total Ballots Counted (Estimate)', 
                          "Vote for Highest Office (President)",
                          "VEP Turnout Rate", 
                          "TotalVEP",
                          "TotalVAP",
                          "NonCitizenPropVEP",
                          "PrisonVEP",
                          "ProbationVEP",
                          "paroleVEP",
                          "IneliFelonVEP",
                          "OverseasVEP",
                          "StateAbv"
                          )

Turnout2020 = Turnout2020[c(1,15,2:14)]

covid_state = read.csv("./data/COVID - State - Daily.csv")
geom_state = read.csv("./data/GeoIDs - State.csv")

covid_state1103 = covid_state %>% filter(year == 2020, month == 11, day == 3) %>% select(statefips, case_count, death_count, test_count)
covid_df = covid_state1103 %>% left_join(geom_state)

covid_df = covid_df %>% summarise(statefips = statefips, 
                    StateAbv = stateabbrev, 
                    case_count = case_count,
                    death_count = death_count, 
                    test_count = test_count, 
                    pop_2019 = state_pop2019)

Turnout_df = Turnout2020 %>% left_join(covid_df)
Turnout_df = Turnout_df[-53,]

save(Turnout_df, file = './data/Turnout_df.Rda')
load("./data/Turnout_df.Rda")
