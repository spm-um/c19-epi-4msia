library(htmltools)
library(tidyverse)
library(plotly)

#load data
mobility <- read.csv('data/mobility.csv')

#convert character to date
mobility <- mobility %>% mutate(date=as.Date(date))

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

#all ages
cases <- mobility %>% select(state, date, cases_new) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "cases_new") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

recreation <- mobility %>% select(state, date, ma_7day_recreation) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "ma_7day_recreation") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

grocery <- mobility %>% select(state, date, ma_7day_grocery) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "ma_7day_grocery") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

parks <- mobility %>% select(state, date, ma_7day_parks) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "ma_7day_parks") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

transit <- mobility %>% select(state, date, ma_7day_transit) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "ma_7day_transit") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

workplace <- mobility %>% select(state, date, ma_7day_work) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "ma_7day_work") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

#plot epidemic curves for all cases
#write a loop to plot and save all the plots
loop.vector <- 2:ncol(cases)

for (i in loop.vector) { # Loop over loop.vector
  
  # store data in column.i as x, y, z and a
  x <- cases[,c(1,i)]
  colnames(x) <- c("date", "cases_new")
  y <- recreation[,c(1,i)]
  colnames(y) <- c("date", "ma_7day_recreation")
  z <- grocery[,c(1,i)]
  colnames(z) <- c("date", "ma_7day_grocery")
  a <- parks[,c(1,i)]
  colnames(a) <- c("date", "ma_7day_parks")
  b <- transit[,c(1,i)]
  colnames(b) <- c("date", "ma_7day_transit")
  c <- workplace[,c(1,i)]
  colnames(c) <- c("date", "ma_7day_work")
  
  
  # Plot
  mobility_plot <- plot_ly() %>% 
    add_bars(data=x, x= ~date, y = ~cases_new, 
             name = "New cases", marker=list(color="grey")) %>%
    add_lines(data=y, x= ~date, y = ~ma_7day_recreation, name = "Retail & recreation", 
              line=list(width = 4, color = 'rgb(113,28,145)'), yaxis = 'y2') %>%
    add_lines(data=z, x= ~date, y = ~ma_7day_grocery, name = "Grocery & pharmacy", 
              line=list(width = 4, color = 'rgb(234,0,217)'), yaxis = 'y2') %>%
    add_lines(data=a, x= ~date, y = ~ma_7day_parks, name = "Parks", 
              line=list(width = 4, color = 'rgb(10,189,198)'), yaxis = 'y2') %>%
    add_lines(data=b, x= ~date, y = ~ma_7day_transit, name = "Transit stations", 
              line=list(width = 4, color = 'rgb(19,62,124)'), yaxis = 'y2') %>%
    add_lines(data=c, x= ~date, y = ~ma_7day_work, name = "Workplace", 
              line=list(width = 4, color = 'rgb(9,24,51)'), yaxis = 'y2') %>%
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
                        range=c(0,max(x$cases_new)*1.2),
                        title="Daily cases"),
           yaxis2 = list(overlaying = "y", side = "right",
                         tickfont = list(color = 'grey9', size=10), color='grey9', 
                         #range=c(0,max(x$ma_7day_recreation)*1.1),
                         title = "% change from baseline")) %>%
    config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian","lasso2d", "select2d", "pan2d","zoom2d")) %>%
    layout(legend = list(x = 0.01, y = 0.99)) %>% 
    layout(legend = l,
           margin=m,
           hovermode = "x unified")
  
  #save the plot
  saveRDS(mobility_plot, paste0("plots/mobility_curve_",names(cases[i]),".rds"))
}
