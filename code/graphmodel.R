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
Turnout2020 = Turnout_df #raw data 
Turnout.df = Turnout2020 %>% select(VEPTurnoutRate, TotalVEP, NonCitizenPropVEP, case_count, death_count, test_count, pop19_bea, PI20Q2_Cap, PI20Q2_Tot, DPI19_Cap, DPI19_Tot, GDP20Q2_Tot, GDP20Q2_Cap, GDP19_Cap, GDP19_Tot, Ratio) %>% .[-1,]
Turnout.df = apply(Turnout.df, 2, function(v){
  str = str_remove_all(v, "[,%]")
  as.numeric(str)
})
rownames(Turnout.df) = Turnout2020$StateAbv[-1]
Turnout.df = as.data.frame(Turnout.df)
Turnout.df = Turnout.df %>% mutate(case_Cap = case_count / pop19_bea, death_Cap = death_count / pop19_bea, test_Cap = test_count / pop19_bea)

## choose some relevant variables
#Turnout_select.df = Turnout.df %>% select(VEPTurnoutRate, TotalVEP, NonCitizenPropVEP, PI20Q2_Cap, DPI19_Cap, GDP20Q2_Cap, GDP19_Cap, Ratio, case_Cap, death_Cap, test_Cap)
Turnout_select.df = Turnout.df %>% select(VEPTurnoutRate, NonCitizenPropVEP, PI20Q2_Cap, DPI19_Cap, GDP20Q2_Cap, GDP19_Cap, Ratio, case_Cap, death_Cap, test_Cap)


gg1 = ggpairs((Turnout_select.df))
ggsave(plot = gg1, file = "../figure/pair_raw.pdf")

# npn transformation to better normalization
Turnout_select.trans.df = huge.npn(Turnout_select.df)
gg2 = ggpairs(data.frame(Turnout_select.trans.df))
ggsave(plot = gg2, file = '../figure/pair_npn.pdf')

#fit the glasso model with 9 possible lambda values 

Turnout.model = huge(Turnout_select.trans.df, nlambda = 10, method = "glasso")
plot(Turnout.model)
out.select = huge.select(Turnout.model,criterion = "ebic") 
plot(out.select)


#use igraph to better the plot
plot_list = 1:3
par(mfrow = c(1,3))
for(i in plot_list){
  graph_from_adjacency_matrix(Turnout.model$path[[i]], mode = 'undirected') %>% 
    plot(vertex.size=10, 
         vertex.label = colnames(Turnout_select.trans.df),
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
         vertex.label = colnames(Turnout_select.trans.df),
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
         vertex.label = colnames(Turnout_select.trans.df),
         vertex.label.cex = 0.7, 
         vertex.label.dist = 1.5,
         vertex.color	= 'pink',
         margin = c(0.01,0.01,0.01,0.01),
         main = TeX(sprintf("$\\lambda = %f$", Turnout.model$lambda[i])) )
  print(i)
}



#further visualization on VEPTurnoutRate & ratio 


plot_df = Turnout.df %>% 
  select(VEPTurnoutRate, Ratio, case_Cap, PI20Q2_Cap, DPI19_Cap) %>% 
  mutate(State = Turnout2020$State[-1]) %>% 
  mutate(TrumpBiden = ifelse(Ratio >1, 'Trump', 'Biden'), 
         CapitalPI20Q2Class = case_when(
           PI20Q2_Cap < quantile(PI20Q2_Cap, 0.25) ~ 'lowerPI20Q2', 
           PI20Q2_Cap < quantile(PI20Q2_Cap, 0.75) ~ 'midPI20Q2', 
           TRUE ~ 'upperPI20Q2'
         ), 
         CapitalDPIClass = case_when(
           DPI19_Cap < quantile(DPI19_Cap, 0.25) ~ 'lowerCapitalDPI', 
           DPI19_Cap < quantile(DPI19_Cap, 0.75) ~ 'midCapitalDPI', 
           TRUE ~ 'upperCapitalDPI'
         ), 
        case_CapClass = case_when(
          case_Cap < quantile(case_Cap, 0.25) ~ 'lightCapCase', 
          case_Cap < quantile(case_Cap, 0.75) ~ 'mildCapCase',
          TRUE ~ 'severeCapCase'
        ))

plot_df

plot_path = '../figure'
gg.ratio = ggplot(plot_df) + 
  geom_density(aes(x = VEPTurnoutRate, color = TrumpBiden), bw = 4) 
ggsave(filename = paste0('ratio.pdf'), 
       plot = gg.ratio, 
       path = plot_path,
)

gg.CapPI20Q2 = ggplot(plot_df) + 
  geom_density(aes(x = VEPTurnoutRate, color = CapitalPI20Q2Class), bw = 4)
ggsave(filename = paste0('CapPI20Q2.pdf'), 
       plot = gg.CapPI20Q2, 
       path = plot_path,
)

gg.CapDPI = ggplot(plot_df) + 
  geom_density(aes(x = VEPTurnoutRate, color = CapitalDPIClass), bw = 4) 
ggsave(filename = paste0('CapDPI19.pdf'), 
       plot = gg.CapDPI , 
       path = plot_path,
)

gg.CapCase = ggplot(plot_df) + 
  geom_density(aes(x = VEPTurnoutRate, color = case_CapClass),bw = 4)
ggsave(filename = paste0('CapCase.pdf'), 
       plot = gg.CapCase  , 
       path = plot_path,
)


map_df = Turnout.df %>% 
  select(VEPTurnoutRate, TotalVEP, Ratio, DPI19_Cap, PI20Q2_Cap, GDP19_Cap, GDP20Q2_Cap, pop19_bea, case_Cap, death_Cap, test_Cap) %>% 
  mutate(StateAbv = Turnout2020$StateAbv[-1], 
         State = Turnout2020$State[-1],
         Ratio = round(Ratio,2), 
         case_Cap = round(case_Cap, 2), 
         death_Cap = round(death_Cap, 2), 
         test_Cap = round(test_Cap, 2)
         )

map_df = map_df %>% 
  mutate(hover = with(map_df, paste(State, '<br>', 
                                    "TurnoutRate", VEPTurnoutRate, "TotalVEP", TotalVEP, "<br>", 
                                    "GDP19", GDP19_Cap, "DPI19", DPI19_Cap, "<br>", 
                                    "TRUMPvsBiden", Ratio, "Case", case_Cap, "Death", death_Cap, "test", test_Cap)))

#TurnoutRate
# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

fig <- plot_geo(map_df, locationmode = 'USA-states')
fig <- fig %>% add_trace(
  z = ~ VEPTurnoutRate, text = ~hover, locations = ~StateAbv,
  color = ~ VEPTurnoutRate, colors = 'Blues'
)
fig <- fig %>% colorbar(title = "Percentage TurnoutRate")
fig <- fig %>% layout(
  title = '2020 US Turnout Rate',
  geo = g
)
fig

libdir = 'html'
htmlwidgets::saveWidget(widget=fig, "../figure/TurnoutRate.html", libdir = libdir, selfcontained = FALSE)




#Ratio
# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

fig <- plot_geo(map_df, locationmode = 'USA-states')
fig <- fig %>% add_trace(
  z = ~ Ratio, text = ~hover, locations = ~StateAbv,
  color = ~ Ratio, colors = 'Blues'
)
fig <- fig %>% colorbar(title = "Ratio = Trump / Biden")
fig <- fig %>% layout(
  title = '2020 US Election Vote Ratio',
  geo = g
)
fig
libdir = 'html'
htmlwidgets::saveWidget(widget=fig, "../figure/Ratio.html", libdir = libdir, selfcontained = FALSE)



#DPI
# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

fig <- plot_geo(map_df, locationmode = 'USA-states')
fig <- fig %>% add_trace(
  z = ~ DPI19_Cap, text = ~hover, locations = ~StateAbv,
  color = ~ DPI19_Cap, colors = 'Blues'
)
fig <- fig %>% colorbar(title = "Dollars")
fig <- fig %>% layout(
  title = '2019 US Per Capital Disposal Personal Income',
  geo = g
)
fig
libdir = 'html'
htmlwidgets::saveWidget(widget=fig, "../figure/DPI19Cap.html", libdir = libdir, selfcontained = FALSE)



#GDP
# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

fig <- plot_geo(map_df, locationmode = 'USA-states')
fig <- fig %>% add_trace(
  z = ~ GDP19_Cap, text = ~hover, locations = ~StateAbv,
  color = ~ GDP19_Cap, colors = 'Blues'
)
fig <- fig %>% colorbar(title = "Dollars")
fig <- fig %>% layout(
  title = '2019 US Per Capital GDP',
  geo = g
)
fig
libdir = 'html'
htmlwidgets::saveWidget(widget=fig, "../figure/GDP19Cap.html", libdir = libdir, selfcontained = FALSE)


