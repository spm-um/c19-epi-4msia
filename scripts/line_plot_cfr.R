library(htmltools)
library(tidyverse)
library(plotly)

#load data
vaccination <- read.csv('data/vaccination.csv')
cfr <- read.csv('data/cfr.csv')

#convert character to date
vaccination <- vaccination %>% mutate(date=as.Date(date))
cfr <- cfr %>% mutate(date=as.Date(date))

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

#plot for malaysia
#plot cfr fro all age groups in malaysia
mas_vax_df <- vaccination %>% filter(state=="Malaysia")
mas_cfr_df <- cfr %>% filter(age=="All age groups")
mas_cfr_plot_all <- plot_ly() %>% 
  add_lines(data = mas_cfr_df, x = ~date,
            y = ~cfr, name = "Case fatality rate", 
            line=list(width = 4, color = 'rgb(233,0,82)')) %>%
  add_lines(data = mas_vax_df, x = ~date,
            y = ~perc_vax1, name = "Partial vaccination", 
            line=list(width = 4, color = 'rgb(134,206,203)'),
            yaxis = 'y2') %>%
  add_lines(data = mas_vax_df, x = ~date,
            y = ~perc_vax2, name = "Full vaccination", 
            line=list(width = 4, color = 'rgb(9,122,127)'),
            yaxis = 'y2') %>%
  add_lines(data = mas_vax_df, x = ~date,
            y = ~perc_booster, name = "Booster vaccination", 
            line=list(width = 4, color = 'rgb(19,24,98)'),
            yaxis = 'y2') %>%
  layout(titlefont=list(size=12),
         xaxis = list( title="Date",
                       rangeselector = list(
                         buttons = list(
                           list(
                             count = 3,
                             label = "3 mo",
                             step = "month",
                             stepmode = "backward"),
                           list(
                             count = 6,
                             label = "6 mo",
                             step = "month",
                             stepmode = "backward"),
                           list(
                             count = 1,
                             label = "1 yr",
                             step = "year",
                             stepmode = "backward"),
                           list(
                             count = 1,
                             label = "YTD",
                             step = "year",
                             stepmode = "todate"),
                           list(step = "all"))),
                       
                       rangeslider = list(type = "date")),
         yaxis = list(side="left", tickfont= list(color = 'grey9', size=8), color='grey9', 
                      range=c(0,max(mas_cfr_df$cfr, na.rm=T)*1.1),
                      title="CFR (%)"),
         yaxis2 = list(overlaying = "y", side = "right",
                       tickfont = list(color = 'grey9', size=10), color='grey9', 
                       range=c(0,max(mas_vax_df$perc_vax1)*1.2),
                       title = "% population vaccinated"),
         legend = list(x = 1.05, y = 0.9)) %>%
  config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian","lasso2d", "select2d", "pan2d","zoom2d")) %>%
  layout(legend = list(x = 0.01, y = 0.99)) %>% 
  layout(legend = l,
         margin=m,
         hovermode = "x unified")
saveRDS(mas_cfr_plot_all, paste0("plots/mas_cfr_plot_all.rds"))


#plot cfr fro all age groups in malaysia
mas_vax_df <- vaccination %>% filter(state=="Malaysia")
mas_cfr_df <- cfr %>% filter(age=="<18 years")
mas_cfr_plot_child <- plot_ly() %>% 
  add_lines(data = mas_cfr_df, x = ~date,
            y = ~cfr, name = "Case fatality rate", 
            line=list(width = 4, color = 'rgb(233,0,82)')) %>%
  add_lines(data = mas_vax_df, x = ~date,
            y = ~perc_vax1_child, name = "Partial vaccination", 
            line=list(width = 4, color = 'rgb(134,206,203)'),
            yaxis = 'y2') %>%
  add_lines(data = mas_vax_df, x = ~date,
            y = ~perc_vax2_child, name = "Full vaccination", 
            line=list(width = 4, color = 'rgb(9,122,127)'),
            yaxis = 'y2') %>%
  layout(titlefont=list(size=12),
         xaxis = list( title="Date",
                       rangeselector = list(
                         buttons = list(
                           list(
                             count = 3,
                             label = "3 mo",
                             step = "month",
                             stepmode = "backward"),
                           list(
                             count = 6,
                             label = "6 mo",
                             step = "month",
                             stepmode = "backward"),
                           list(
                             count = 1,
                             label = "1 yr",
                             step = "year",
                             stepmode = "backward"),
                           list(
                             count = 1,
                             label = "YTD",
                             step = "year",
                             stepmode = "todate"),
                           list(step = "all"))),
                       
                       rangeslider = list(type = "date")),
         yaxis = list(side="left", tickfont= list(color = 'grey9', size=8), color='grey9', 
                      range=c(0,max(mas_cfr_df$cfr, na.rm=T)*1.1),
                      title="CFR (%)"),
         yaxis2 = list(overlaying = "y", side = "right",
                       tickfont = list(color = 'grey9', size=10), color='grey9', 
                       range=c(0,max(mas_vax_df$perc_vax1)*1.2),
                       title = "% population vaccinated"),
         legend = list(x = 1.05, y = 0.9)) %>%
  config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian","lasso2d", "select2d", "pan2d","zoom2d")) %>%
  layout(legend = list(x = 0.01, y = 0.99)) %>% 
  layout(legend = l,
         margin=m,
         hovermode = "x unified")
saveRDS(mas_cfr_plot_child, paste0("plots/mas_cfr_plot_child.rds"))


#audlt -18-59 years
mas_vax_df <- vaccination %>% filter(state=="Malaysia")
mas_cfr_df <- cfr %>% filter(age=="18-59 years")
mas_cfr_plot_adult <- plot_ly() %>% 
  add_lines(data = mas_cfr_df, x = ~date,
            y = ~cfr, name = "Case fatality rate", 
            line=list(width = 4, color = 'rgb(233,0,82)')) %>%
  add_lines(data = mas_vax_df, x = ~date,
            y = ~perc_vax1_adult, name = "Partial vaccination", 
            line=list(width = 4, color = 'rgb(134,206,203)'),
            yaxis = 'y2') %>%
  add_lines(data = mas_vax_df, x = ~date,
            y = ~perc_vax2_adult, name = "Full vaccination", 
            line=list(width = 4, color = 'rgb(9,122,127)'),
            yaxis = 'y2') %>%
  add_lines(data = mas_vax_df, x = ~date,
            y = ~perc_booster, name = "Booster vaccination", 
            line=list(width = 4, color = 'rgb(19,24,98)'),
            yaxis = 'y2') %>%
  layout(titlefont=list(size=12),
         xaxis = list( title="Date",
                       rangeselector = list(
                         buttons = list(
                           list(
                             count = 3,
                             label = "3 mo",
                             step = "month",
                             stepmode = "backward"),
                           list(
                             count = 6,
                             label = "6 mo",
                             step = "month",
                             stepmode = "backward"),
                           list(
                             count = 1,
                             label = "1 yr",
                             step = "year",
                             stepmode = "backward"),
                           list(
                             count = 1,
                             label = "YTD",
                             step = "year",
                             stepmode = "todate"),
                           list(step = "all"))),
                       
                       rangeslider = list(type = "date")),
         yaxis = list(side="left", tickfont= list(color = 'grey9', size=8), color='grey9', 
                      range=c(0,max(mas_cfr_df$cfr, na.rm=T)*1.1),
                      title="CFR (%)"),
         yaxis2 = list(overlaying = "y", side = "right",
                       tickfont = list(color = 'grey9', size=10), color='grey9', 
                       range=c(0,max(mas_vax_df$perc_vax1)*1.2),
                       title = "% population vaccinated"),
         legend = list(x = 1.05, y = 0.9)) %>%
  config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian","lasso2d", "select2d", "pan2d","zoom2d")) %>%
  layout(legend = list(x = 0.01, y = 0.99)) %>% 
  layout(legend = l,
         margin=m,
         hovermode = "x unified")
saveRDS(mas_cfr_plot_adult, paste0("plots/mas_cfr_plot_adult.rds"))

#audlt ->60 years
mas_vax_df <- vaccination %>% filter(state=="Malaysia")
mas_cfr_df <- cfr %>% filter(age==">59 years")
mas_cfr_plot_elderly <- plot_ly() %>% 
  add_lines(data = mas_cfr_df, x = ~date,
            y = ~cfr, name = "Case fatality rate", 
            line=list(width = 4, color = 'rgb(233,0,82)')) %>%
  add_lines(data = mas_vax_df, x = ~date,
            y = ~perc_vax1_adult, name = "Partial vaccination", 
            line=list(width = 4, color = 'rgb(134,206,203)'),
            yaxis = 'y2') %>%
  add_lines(data = mas_vax_df, x = ~date,
            y = ~perc_vax2_adult, name = "Full vaccination", 
            line=list(width = 4, color = 'rgb(9,122,127)'),
            yaxis = 'y2') %>%
  add_lines(data = mas_vax_df, x = ~date,
            y = ~perc_booster, name = "Booster vaccination", 
            line=list(width = 4, color = 'rgb(19,24,98)'),
            yaxis = 'y2') %>%
  layout(titlefont=list(size=12),
         xaxis = list( title="Date",
                       rangeselector = list(
                         buttons = list(
                           list(
                             count = 3,
                             label = "3 mo",
                             step = "month",
                             stepmode = "backward"),
                           list(
                             count = 6,
                             label = "6 mo",
                             step = "month",
                             stepmode = "backward"),
                           list(
                             count = 1,
                             label = "1 yr",
                             step = "year",
                             stepmode = "backward"),
                           list(
                             count = 1,
                             label = "YTD",
                             step = "year",
                             stepmode = "todate"),
                           list(step = "all"))),
                       
                       rangeslider = list(type = "date")),
         yaxis = list(side="left", tickfont= list(color = 'grey9', size=8), color='grey9', 
                      range=c(0,max(mas_cfr_df$cfr, na.rm=T)*1.1),
                      title="CFR (%)"),
         yaxis2 = list(overlaying = "y", side = "right",
                       tickfont = list(color = 'grey9', size=10), color='grey9', 
                       range=c(0,max(mas_vax_df$perc_vax1)*1.2),
                       title = "% population vaccinated"),
         legend = list(x = 1.05, y = 0.9)) %>%
  config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian","lasso2d", "select2d", "pan2d","zoom2d")) %>%
  layout(legend = list(x = 0.01, y = 0.99)) %>% 
  layout(legend = l,
         margin=m,
         hovermode = "x unified")
saveRDS(mas_cfr_plot_elderly, paste0("plots/mas_cfr_plot_elderly.rds"))
