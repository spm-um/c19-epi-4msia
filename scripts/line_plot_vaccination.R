library(htmltools)
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

# malaysia- testing
#all ages
pvax <- vaccination %>% select(state, date, perc_vax1) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "perc_vax1") %>%
  rename("Klang"="Klang valley",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

fvax <- vaccination %>% select(state, date, perc_vax2) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "perc_vax2") %>%
  rename("Klang"="Klang valley",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

boost <- vaccination %>% select(state, date, perc_booster) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "perc_booster") %>%
  rename("Klang"="Klang valley",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")


#plot epidemic curves for all cases
#write a loop to plot and save all the plots
loop.vector <- 2:ncol(pvax)

for (i in loop.vector) { # Loop over loop.vector
  
  # store data in column.i as x, y, z and a
  x <- pvax[,c(1,i)]
  colnames(x) <- c("date", "pvax")
  y <- fvax[,c(1,i)]
  colnames(y) <- c("date", "fvax")
  z <- boost[,c(1,i)]
  colnames(z) <- c("date", "boost")
  
  # Plot
  vax_plot <- plot_ly() %>% 
    add_lines(data = x, x = ~date,
              y = ~pvax, name = "Partial vaccination", 
              fill="tozeroy", fillcolor = 'rgb(134,206,203, 0.8)',
              line=list(width = 4, color = 'rgb(134,206,203)')) %>%
    add_lines(data = y, x = ~date,
              y = ~fvax, name = "Full vaccination", 
              fill="tozeroy", fillcolor = 'rgb(9,122,127, 0.8)',
              line=list(width = 4, color = 'rgb(9,122,127)')) %>%
    add_lines(data = z, x = ~date,
              y = ~boost, name = "Booster vaccination", 
              fill="tozeroy", fillcolor = 'rgb(19,24,98, 0.8)',
              line=list(width = 4, color = 'rgb(19,24,98)')) %>%
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
                        range=c(0,max(x$pvax, na.rm=T)*1.2),
                        title="% population vaccinated"),
           legend = list(x = 1.05, y = 0.9)) %>%
    config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian","lasso2d", "select2d", "pan2d","zoom2d")) %>%
    layout(legend = list(x = 0.01, y = 0.99)) %>% 
    layout(legend = l,
           margin=m,
           hovermode = "x unified")
  
  
  
  #save the plot
  saveRDS(vax_plot, paste0("plots/vax_curve_",names(pvax[i]), "_all",".rds"))
}

#adult 
pvax <- vaccination %>% select(state, date, perc_vax1_adult) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "perc_vax1_adult") %>%
  rename("Klang"="Klang valley",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

fvax <- vaccination %>% select(state, date, perc_vax2_adult) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "perc_vax2_adult") %>%
  rename("Klang"="Klang valley",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

boost <- vaccination %>% select(state, date, perc_booster_adult) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "perc_booster_adult") %>%
  rename("Klang"="Klang valley",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")


#plot epidemic curves for all cases
#write a loop to plot and save all the plots
loop.vector <- 2:ncol(pvax)

for (i in loop.vector) { # Loop over loop.vector
  
  # store data in column.i as x, y, z and a
  x <- pvax[,c(1,i)]
  colnames(x) <- c("date", "pvax")
  y <- fvax[,c(1,i)]
  colnames(y) <- c("date", "fvax")
  z <- boost[,c(1,i)]
  colnames(z) <- c("date", "boost")
  
  # Plot
  vax_plot <- plot_ly() %>% 
    add_lines(data = x, x = ~date,
              y = ~pvax, name = "Partial vaccination", 
              fill="tozeroy", fillcolor = 'rgb(134,206,203, 0.8)',
              line=list(width = 4, color = 'rgb(134,206,203)')) %>%
    add_lines(data = y, x = ~date,
              y = ~fvax, name = "Full vaccination", 
              fill="tozeroy", fillcolor = 'rgb(9,122,127, 0.8)',
              line=list(width = 4, color = 'rgb(9,122,127)')) %>%
    add_lines(data = z, x = ~date,
              y = ~boost, name = "Booster vaccination", 
              fill="tozeroy", fillcolor = 'rgb(19,24,98, 0.8)',
              line=list(width = 4, color = 'rgb(19,24,98)')) %>%
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
                        range=c(0,max(x$pvax, na.rm=T)*1.2),
                        title="% population vaccinated"),
           legend = list(x = 1.05, y = 0.9)) %>%
    config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian","lasso2d", "select2d", "pan2d","zoom2d")) %>%
    layout(legend = list(x = 0.01, y = 0.99)) %>% 
    layout(legend = l,
           margin=m,
           hovermode = "x unified")
  
  
  
  #save the plot
  saveRDS(vax_plot, paste0("plots/vax_curve_",names(pvax[i]), "_adult",".rds"))
}

#child
pvax <- vaccination %>% select(state, date, perc_vax1_child) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "perc_vax1_child") %>%
  rename("Klang"="Klang valley",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

fvax <- vaccination %>% select(state, date, perc_vax2_child) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "perc_vax2_child") %>%
  rename("Klang"="Klang valley",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

#plot epidemic curves for all cases
#write a loop to plot and save all the plots
loop.vector <- 2:ncol(pvax)

for (i in loop.vector) { # Loop over loop.vector
  
  # store data in column.i as x, y, z and a
  x <- pvax[,c(1,i)]
  colnames(x) <- c("date", "pvax")
  y <- fvax[,c(1,i)]
  colnames(y) <- c("date", "fvax")
  z <- boost[,c(1,i)]
  colnames(z) <- c("date", "boost")
  
  # Plot
  vax_plot <- plot_ly() %>% 
    add_lines(data = x, x = ~date,
              y = ~pvax, name = "Partial vaccination", 
              fill="tozeroy", fillcolor = 'rgb(134,206,203, 0.8)',
              line=list(width = 4, color = 'rgb(134,206,203)')) %>%
    add_lines(data = y, x = ~date,
              y = ~fvax, name = "Full vaccination", 
              fill="tozeroy", fillcolor = 'rgb(9,122,127, 0.8)',
              line=list(width = 4, color = 'rgb(9,122,127)')) %>%
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
                        range=c(0,max(x$pvax, na.rm=T)*1.2),
                        title="% population vaccinated"),
           legend = list(x = 1.05, y = 0.9)) %>%
    config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian","lasso2d", "select2d", "pan2d","zoom2d")) %>%
    layout(legend = list(x = 0.01, y = 0.99)) %>% 
    layout(legend = l,
           margin=m,
           hovermode = "x unified")
  
  
  
  #save the plot
  saveRDS(vax_plot, paste0("plots/vax_curve_",names(pvax[i]), "_child",".rds"))
}



