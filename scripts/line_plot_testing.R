library(htmltools)
library(tidyverse)
library(plotly)
library(quantmod)

#load data
testing <- read.csv('data/testing.csv')

#convert character to date
testing <- testing %>% mutate(date=as.Date(date))

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

#plot the rt
# malaysia- testing
tpr <- testing %>% select(state, date, ma_7day_tpr) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "ma_7day_tpr") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

tr <- testing %>% select(state, date, ma_7day_tr) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "ma_7day_tr") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

#plot epidemic curves for all cases
#write a loop to plot and save all the plots
loop.vector <- 2:ncol(tr)

for (i in loop.vector) { # Loop over loop.vector
  
  # store data in column.i as x, y, z and a
  x <- tpr[,c(1,i)]
  colnames(x) <- c("date", "tpr")
  y <- tr[,c(1,i)]
  colnames(y) <- c("date", "tr")
  
  # Plot
  test_plot <- plot_ly() %>% 
    add_lines(data=x, x= ~date, y = ~round(tpr,2), name = "Test positivity ratio", 
              line=list(width = 4, color = 'rgb(197,31,93)')) %>%
    add_lines(data=y, x= ~date, y = ~ round(tr,2), name ="Testing rate" , 
              opacity=0.4, line=list(width = 4, dash = 'dot', color = 'rgb(36,52,71)'), yaxis = 'y2') %>%
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
                        range=c(0,max(x$tpr)*1.1),
                        title="Test positivity ratio"),
           yaxis2 = list(overlaying = "y", side = "right",
                         tickfont = list(color = 'grey9', size=10), color='grey9', 
                         range=c(0,max(y$tr)*1.1),
                         title = "Testing rate (per 1,000 population)"),
           legend = list(x = 1.05, y = 0.9)) %>%
    config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian","lasso2d", "select2d", "pan2d","zoom2d")) %>%
    layout(legend = list(x = 0.01, y = 0.99)) %>% 
    layout(legend = l,
           margin=m,
           hovermode="x unified")
  
  #save the plot
  saveRDS(test_plot, paste0("plots/test_curve_",names(tr[i]),".rds"))
}
