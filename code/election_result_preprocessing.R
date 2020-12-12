## Resetting For Economical Data
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load("../data/Turnout_df.Rda")
# Turnout_df=Turnout_df[,-c(21,22,26)]
# save(Turnout_df, file = '../data/Turnout_df.Rda')
# load("../data/Turnout_df.Rda")
# 
# 
library(dplyr)
library(tidyverse)
library(plyr)
# 
# # Computing Election Results
# # Election_Raw=read.csv("../data/president_county_candidate.csv")
# # Turnout2020=read.csv("../data/2020 November General Election - Turnout Rates.csv")
# # Election_county_ind=which(Election_Raw$candidate%in% candidates)
# # Election_county=Election_Raw[Election_county_ind,]
# # Election_sum=aggregate(Election_Raw$total_votes,by=list(state=Election_Raw$state),FUN=sum)
# # Candidates_tot=aggregate(list(total_votes=Election_Raw$total_votes),by=list(state=Election_Raw$state,candidate=Election_Raw$candidate),FUN=sum)
# # Candidates_tot=Candidates_tot[which(Candidates_tot$candidate%in%c("Donald Trump","Joe Biden")),]
# # Candidates=cbind(Candidates_tot[1:51,-2],Candidates_tot[-c(1:51),3])
# # colnames(Candidates)=c("State","Trump","Biden")
# # Candidates$ratio=Candidates$Trump/Candidates$Biden
# # write.csv(Candidates,"election_result.csv")
# 
# Turnout2020=read.csv("../data/2020 November General Election - Turnout Rates.csv", header = FALSE, skip = 2)
# colnames(Turnout2020) = c("State", 'Source', 'Official', 
#                           'TotalBallots', 
#                           "VoteHigh",
#                           "VEPTurnoutRate", 
#                           "TotalVEP",
#                           "TotalVAP",
#                           "NonCitizenPropVEP",
#                           "PrisonVEP",
#                           "ProbationVEP",
#                           "paroleVEP",
#                           "IneliFelonVEP",
#                           "OverseasVEP",
#                           "StateAbv"
# )
# 
# Turnout2020 = Turnout2020[c(1,15,2:14)]



## During Pandemic

## The COVID Influence
# covid_state = read.csv("../data/COVID - State - Daily.csv")
# geom_state = read.csv("../data/GeoIDs - State.csv")
# 
# covid_state1103 = covid_state %>% filter(year == 2020, month == 11, day == 3) %>% select(statefips, case_count, death_count, test_count)
# covid_df = covid_state1103 %>% left_join(geom_state)
# 
# covid_df = covid_df %>% summarise(statefips = statefips, 
#                                   StateAbv = stateabbrev, 
#                                   case_count = case_count,
#                                   death_count = death_count, 
#                                   test_count = test_count, 
#                                   pop_2019 = state_pop2019)
# 
# Turnout_df = Turnout2020 %>% left_join(covid_df)
# Turnout_df = Turnout_df[-53,]
# Turnout_df = Turnout_df %>% mutate(State = State %>% str_remove_all("[*]"))
# #save(Turnout_df, file = '../data/Turnout_df.Rda')

### Economic Influence during Pandemic 
## Personal Income 2020Q2
# pi20=read.csv("pi20Q2_sum.csv",skip=4)[1:180,]
# 
# pi20_df=data.frame(State = factor(pi20$GeoName %>% unique() %>% str_remove_all("[*]") %>% sub(' $', '',.)), 
#                    PI20Q2_Tot = (pi20 %>% ddply(.(GeoName), summarise, X2020.Q2[LineCode == 1]) %>% .$..1),
#                    PI20Q2_Cap = (pi20 %>% ddply(.(GeoName), summarise, X2020.Q2[LineCode == 3]) %>% .$..1),
#                    pop20Q2= (pi20 %>% ddply(.(GeoName), summarise, X2020.Q2[LineCode == 2]) %>% .$..1))
# pi20_df[,-1]=pi20_df[order(order(pi20_df$State)),-1]
# ## Real GDP
# gdp20=read.csv("qgdp.csv",skip = 4)[1:60,]
# gdp20_df =data.frame(State= gdp20$GeoName %>% str_remove_all("[*]") %>% sub(' $', '',.),
#                      GDP20Q2_Tot=gdp20$X2020.Q2,
#                      GDP20Q2_Cap=gdp20$X2020.Q2*10^6/pi20_df$pop20Q2)
# 
# ### Economy Behavior 2019
# # # rm(list = ls())
# 
# ## Desposable Personal Income 
# dpi2019 = read.csv("../data/dpi2019_sum.csv", skip = 4)[1:180,]
# # 
# dpi19_df = data.frame(State = factor(dpi2019$GeoName %>% unique() %>% str_remove_all("[*]") %>% sub(' $', '',.)), 
#                      DPI19_Tot = dpi2019 %>% ddply(.(GeoName), summarise, X2019[LineCode == 51]) %>% .$..1,
#                      DPI19_Cap = dpi2019 %>% ddply(.(GeoName), summarise, X2019[LineCode == 53]) %>% .$..1,
#                      pop19_bea=dpi2019 %>% ddply(.(GeoName), summarise, X2019[LineCode == 52]) %>% .$..1)
# dpi19_df[,-1]=dpi19_df[order(order(dpi19_df$State)),-1]
# 
# ## Real GDP 
# gdp19 = read.csv("../data/ygdp19.csv", skip = 4)[1:60,]
# gdp19_df =data.frame(State= factor(gdp19$GeoName %>% str_remove_all("[*]") %>% sub(' $', '',.)),
#                      GDP19_Tot=gdp19$X2019,
#                      GDP19_Cap=gdp19$X2019*10^6/dpi19_df$pop19_bea)
# # Merging Economy Data
# Turnout_df = Turnout_df %>% left_join(pi20_df)
# Turnout_df = Turnout_df %>% left_join(gdp20_df)
# Turnout_df = Turnout_df %>% left_join(dpi19_df)
# Turnout_df = Turnout_df %>% left_join(gdp19_df)
# save(Turnout_df, file = '../data/Turnout_df.Rda')
# rm(list = ls())
#### Adding Electorial votes
# load("../data/Turnout_df.Rda")
# electorial.raw=read.csv("../data/Electoral_College.csv")
# electorial=electorial.raw[which(electorial.raw$Year==2020),-1]
# Turnout_df=Turnout_df%>%left_join(electorial)
# Turnout_df[10,]$Votes=electorial[which(electorial$State=="D.C."),]$Votes
# save(Turnout_df,file = "../data/Turnout_df.Rda")
#load("../data/Turnout_df.Rda")

### Add Swing States: the difference is less than 5%; we find the ratio between .89 & 1.12
# blue=which(Turnout_df$Ratio>1.12)
# lb=which(Turnout_df$Ratio>1 & Turnout_df$Ratio<1.12)
# lr=which(Turnout_df$Ratio<1 & Turnout_df$Ratio>0.89)
# red=which(Turnout_df$Ratio<.89)
# swing=sort(union(lb,lr))
# power=rep(NA,dim(Turnout_df)[1])
# power[blue]="Blue";power[red]="Red";power[lb]="Swing-Blue";power[lr]="Swing-Red"
# Turnout_df$Power=power
# save(Turnout_df,file="../data/Turnout_df.Rda")
load("../data/Turnout_df.Rda")
