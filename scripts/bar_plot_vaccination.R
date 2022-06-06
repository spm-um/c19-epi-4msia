#load packages
library(tidyverse)
library(plotly)

#load data
vaccination <- read.csv('data/vaccination.csv')

#convert character to date
vaccination <- vaccination %>% mutate(date=as.Date(date))

#legends & margisn
m <- list(
  l = 20,
  r = 40,
  b = 20,
  t = 20,
  pad = 5
)

l <- list(
  bgcolor = "transparent",
  bordercolor = "transparent")

#plot the data
#plot the total population
latest_vax_df <- vaccination %>% filter(date==max(date)) %>% 
  mutate(arrange_check=ifelse(state=="Malaysia",1,2)) %>%
  group_by(arrange_check) %>%
  arrange(desc(perc_booster), .by_group = TRUE) 
latest_vax_df$state <- factor(latest_vax_df$state, levels = rev(latest_vax_df$state), ordered=TRUE)
latest_vax_plot_total <- plot_ly(data=latest_vax_df, x = ~perc_total, y = ~state, type = 'bar', orientation = 'h', name = 'Total population',
                           marker = list(color = 'rgba(190,200,209, 1.0)',
                                         line = list(color = 'rgba(190,200,209, 1.0)',
                                                     width = 3)),
                           text = ~pop,
                           hovertemplate = paste(
                             "<b>%{y}</b><br><br>",
                             "Proportion vaccinated: %{x:}%<br>",
                             "Population: %{text:,}",
                             "<extra></extra>"
                           ))
latest_vax_plot_total <- latest_vax_plot_total %>% add_trace(x = ~perc_vax1, name = 'Partial vaccination',
                                                 marker = list(color = 'rgba(134,206,203, 1.0)',
                                                               line = list(color = 'rgba(134,206,203, 1.0)',
                                                                           width = 3)),
                                                 text = ~pop,
                                                 hovertemplate = paste(
                                                   "<b>%{y}</b><br><br>",
                                                   "Proportion vaccinated: %{x:}%<br>",
                                                   "Population: %{text:,}",
                                                   "<extra></extra>"
                                                 ))
latest_vax_plot_total <- latest_vax_plot_total %>% add_trace(x = ~perc_vax2, name = 'Full vaccination',
                                                 marker = list(color = 'rgba(19,122,127, 1.0)',
                                                               line = list(color = 'rgba(19,122,127, 1.0)',
                                                                           width = 3)),
                                                 text = ~pop,
                                                 hovertemplate = paste(
                                                   "<b>%{y}</b><br><br>",
                                                   "Proportion vaccinated: %{x:}%<br>",
                                                   "Population: %{text:,}",
                                                   "<extra></extra>"
                                                 ))
latest_vax_plot_total <- latest_vax_plot_total %>% add_trace(x = ~perc_booster, name = 'Booster vaccination',
                                                 marker = list(color = 'rgba(19,24,98, 1.0)',
                                                               line = list(color = 'rgba(19,24,98, 1.0)',
                                                                           width = 3)),
                                                 text = ~pop,
                                                 hovertemplate = paste(
                                                   "<b>%{y}</b><br><br>",
                                                   "Proportion vaccinated: %{x:}%<br>",
                                                   "Population: %{text:,}",
                                                   "<extra></extra>"
                                                 ))
latest_vax_plot_total <- latest_vax_plot_total %>% layout(barmode = 'overlay',
                      xaxis = list(title = " % population vaccinated"),
                      yaxis = list(title ="States"),
                      legend = list(x = 1.05, y = 0.9)) %>%
  config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian","lasso2d", "select2d", "pan2d","zoom2d")) %>%
  layout(legend = list(orientation = "h",   
                       xanchor = "center",  
                       x = 0.5, y=-0.1)) %>%           
  layout(legend = l,
         margin=m)
saveRDS(latest_vax_plot_total, paste0("plots/vax_bar_all.rds"))


#adults
latest_vax_plot_adult <- plot_ly(data=latest_vax_df, x = ~perc_total, y = ~state, type = 'bar', orientation = 'h', name = 'adult population',
                                 marker = list(color = 'rgba(190,200,209, 1.0)',
                                               line = list(color = 'rgba(190,200,209, 1.0)',
                                                           width = 3)),
                                 text = ~pop_adult,
                                 hovertemplate = paste(
                                   "<b>%{y}</b><br><br>",
                                   "Proportion vaccinated: %{x:}%<br>",
                                   "Population: %{text:,}",
                                   "<extra></extra>"
                                 ))
latest_vax_plot_adult <- latest_vax_plot_adult %>% add_trace(x = ~perc_vax1_adult, name = 'Partial vaccination',
                                                             marker = list(color = 'rgba(134,206,203, 1.0)',
                                                                           line = list(color = 'rgba(134,206,203, 1.0)',
                                                                                       width = 3)),
                                                             text = ~pop_adult,
                                                             hovertemplate = paste(
                                                               "<b>%{y}</b><br><br>",
                                                               "Proportion vaccinated: %{x:}%<br>",
                                                               "Population: %{text:,}",
                                                               "<extra></extra>"
                                                             ))
latest_vax_plot_adult <- latest_vax_plot_adult %>% add_trace(x = ~perc_vax2_adult, name = 'Full vaccination',
                                                             marker = list(color = 'rgba(19,122,127, 1.0)',
                                                                           line = list(color = 'rgba(19,122,127, 1.0)',
                                                                                       width = 3)),
                                                             text = ~pop_adult,
                                                             hovertemplate = paste(
                                                               "<b>%{y}</b><br><br>",
                                                               "Proportion vaccinated: %{x:}%<br>",
                                                               "Population: %{text:,}",
                                                               "<extra></extra>"
                                                             ))
latest_vax_plot_adult <- latest_vax_plot_adult %>% add_trace(x = ~perc_booster, name = 'Booster vaccination',
                                                             marker = list(color = 'rgba(19,24,98, 1.0)',
                                                                           line = list(color = 'rgba(19,24,98, 1.0)',
                                                                                       width = 3)),
                                                             text = ~pop_adult,
                                                             hovertemplate = paste(
                                                               "<b>%{y}</b><br><br>",
                                                               "Proportion vaccinated: %{x:}%<br>",
                                                               "Population: %{text:,}",
                                                               "<extra></extra>"
                                                             ))
latest_vax_plot_adult <- latest_vax_plot_adult %>% layout(barmode = 'overlay',
                                                          xaxis = list(title = " % population vaccinated"),
                                                          yaxis = list(title ="States"),
                                                          legend = list(x = 1.05, y = 0.9)) %>%
  config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian","lasso2d", "select2d", "pan2d","zoom2d")) %>%
  layout(legend = list(orientation = "h",   
                       xanchor = "center",  
                       x = 0.5, y=-0.1)) %>%           
  layout(legend = l,
         margin=m)
saveRDS(latest_vax_plot_adult, paste0("plots/vax_bar_adult.rds"))

#children
latest_vax_plot_child <- plot_ly(data=latest_vax_df, x = ~perc_total, y = ~state, type = 'bar', orientation = 'h', name = 'adult population',
                                 marker = list(color = 'rgba(190,200,209, 1.0)',
                                               line = list(color = 'rgba(190,200,209, 1.0)',
                                                           width = 3)),
                                 text = ~pop_child,
                                 hovertemplate = paste(
                                   "<b>%{y}</b><br><br>",
                                   "Proportion vaccinated: %{x:}%<br>",
                                   "Population: %{text:,}",
                                   "<extra></extra>"
                                 ))
latest_vax_plot_child <- latest_vax_plot_child %>% add_trace(x = ~perc_vax1_child, name = 'Partial vaccination',
                                                             marker = list(color = 'rgba(134,206,203, 1.0)',
                                                                           line = list(color = 'rgba(134,206,203, 1.0)',
                                                                                       width = 3)),
                                                             text = ~pop_child,
                                                             hovertemplate = paste(
                                                               "<b>%{y}</b><br><br>",
                                                               "Proportion vaccinated: %{x:}%<br>",
                                                               "Population: %{text:,}",
                                                               "<extra></extra>"
                                                             ))
latest_vax_plot_child <- latest_vax_plot_child %>% add_trace(x = ~perc_vax2_child, name = 'Full vaccination',
                                                             marker = list(color = 'rgba(19,122,127, 1.0)',
                                                                           line = list(color = 'rgba(19,122,127, 1.0)',
                                                                                       width = 3)),
                                                             text = ~pop_child,
                                                             hovertemplate = paste(
                                                               "<b>%{y}</b><br><br>",
                                                               "Proportion vaccinated: %{x:}%<br>",
                                                               "Population: %{text:,}",
                                                               "<extra></extra>"
                                                             ))
latest_vax_plot_child <- latest_vax_plot_child %>% layout(barmode = 'overlay',
                                                          xaxis = list(title = " % population vaccinated"),
                                                          yaxis = list(title ="States"),
                                                          legend = list(x = 1.05, y = 0.9)) %>%
  config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian","lasso2d", "select2d", "pan2d","zoom2d")) %>%
  layout(legend = list(orientation = "h",   
                       xanchor = "center",  
                       x = 0.5, y=-0.1)) %>%           
  layout(legend = l,
         margin=m)
saveRDS(latest_vax_plot_child, paste0("plots/vax_bar_child.rds"))