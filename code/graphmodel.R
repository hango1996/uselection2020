rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(glasso)
library(ggplot2)
library(GGally)
library(huge)
library(igraph)
library(latex2exp)
load("../data/Turnout_df.Rda")

# select some variables in the Turnout.df
Turnout2020 = Turnout_df
Turnout.df = Turnout2020 %>% select(VEPTurnoutRate, TotalVEP, NonCitizenPropVEP, case_count, death_count, test_count, TotalDPI, CapitaDPI, GDP, Ratio) %>% .[-1,]
Turnout.mat = apply(Turnout.df, 2, function(v){
  str = str_remove_all(v, "[,%]")
  as.numeric(str)
})
rownames(Turnout.mat) = Turnout2020$StateAbv[-1]
Turnout.mat = as.data.frame(Turnout.mat)
gg1 = ggpairs((Turnout.mat))
ggsave(plot = gg1, file = "../figure/pair_raw.pdf")

# npn transformation to better normalization
Turnout.trans.mat = huge.npn(Turnout.mat)
gg2 = ggpairs(data.frame(Turnout.trans.mat))
ggsave(plot = gg2, file = '../figure/pair_npn.pdf')

#fit the glasso model with 9 possible lambda values 
Turnout.model = huge(Turnout.trans.mat, nlambda = 10, method = "glasso")
plot(Turnout.model)
out.select = huge.select(Turnout.model,criterion = "ebic") 
plot(out.select)


#use igraph to better the plot
plot_list = 1:3
par(mfrow = c(1,3))
for(i in plot_list){
  graph_from_adjacency_matrix(Turnout.model$path[[i]], mode = 'undirected') %>% 
    plot(vertex.size=10, 
         vertex.label = colnames(Turnout.trans.mat),
         vertex.label.cex = 0.7, 
         vertex.label.dist = 1.5,
         vertex.color	= 'pink',
         margin = c(0.01,0.01,0.01,0.01),
         main = TeX(sprintf("$\\lambda = %f$", Turnout.model$lambda[i])) )
  print(i)
}


plot_list = 4:6
par(mfrow = c(1,3))
for(i in plot_list){
  graph_from_adjacency_matrix(Turnout.model$path[[i]], mode = 'undirected') %>% 
    plot(vertex.size=10, 
         vertex.label = colnames(Turnout.trans.mat),
         vertex.label.cex = 0.7, 
         vertex.label.dist = 1.5,
         vertex.color	= 'pink',
         margin = c(0.01,0.01,0.01,0.01),
         main = TeX(sprintf("$\\lambda = %f$", Turnout.model$lambda[i])) )
  print(i)
}



plot_list = 7:9
par(mfrow = c(1,3))
for(i in plot_list){
  graph_from_adjacency_matrix(Turnout.model$path[[i]], mode = 'undirected') %>% 
    plot(vertex.size=10, 
         vertex.label = colnames(Turnout.trans.mat),
         vertex.label.cex = 0.7, 
         vertex.label.dist = 1.5,
         vertex.color	= 'pink',
         margin = c(0.01,0.01,0.01,0.01),
         main = TeX(sprintf("$\\lambda = %f$", Turnout.model$lambda[i])) )
  print(i)
}



#further visualization on VEPTurnoutRate & ratio 


plot_df = Turnout.mat %>% 
  select(VEPTurnoutRate, Ratio, TotalDPI, CapitaDPI) %>% 
  mutate(State = Turnout2020$State[-1]) %>% 
  mutate(TrumpBiden = ifelse(Ratio >1, 'Trump', 'Biden'), 
         DPIClass = case_when(
           TotalDPI < quantile(TotalDPI, 0.25) ~ 'lowerDPI', 
           TotalDPI < quantile(TotalDPI, 0.75) ~ 'midDPI', 
           TRUE ~ 'upperDPI'
         ), 
         CapitalDPIClass = case_when(
           CapitaDPI < quantile(CapitaDPI, 0.25) ~ 'lowerCapitalDPI', 
           CapitaDPI < quantile(CapitaDPI, 0.75) ~ 'midCapitalDPI', 
           TRUE ~ 'upperCapitalDPI'
         )) 
ggplot(plot_df) + 
  geom_density(aes(x = VEPTurnoutRate)) + 
  facet_wrap(~TrumpBiden)
ggplot(plot_df) + 
  geom_density(aes(x = VEPTurnoutRate)) + 
  facet_wrap(~DPIClass)
ggplot(plot_df) + 
  geom_density(aes(x = VEPTurnoutRate)) + 
  facet_wrap(~CapitalDPIClass)


