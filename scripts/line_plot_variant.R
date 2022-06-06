library(httr)
library(jsonlite)
library(tidyverse)
library(curl)
library(lubridate)
library(MMWRweek)
library(plotly)

#call gisaid data in
gisaid_data <- POST(url = "https://www.epicov.org/epi3/feed/gisaid_variants_statistics.json", 
                      body = "gisaid_variants_statistics.json",
                      authenticate(user = "vivekjason1987", password = "eTkPrjMn97xz6VT"), 
                      verbose(), 
                      add_headers(), 
                      encode = "json")

#parse gisaid json file
data_raw_ugly <- jsonlite::fromJSON(rawToChar(gisaid_data$content), flatten = T)

#clean up the variants
variant <- enframe(unlist(data_raw_ugly$stats)) %>%
  separate(name, sep = "\\.", c("date", "country", "submission_type", "label_type")) %>%
  filter(country=="Malaysia") %>%
  mutate(date=as.Date(date)) %>% 
  filter(submission_type=="submissions_per_variant") %>%
  separate(label_type, sep = "(?<=[A-Za-z])(?=[0-9])",
           c("label", "id"))

#aplit and rejoin
variant1 <- variant %>% filter (label=="count") %>%
  rename(count=label)
variant2 <- variant %>% filter (label=="value") %>%
  rename(type=value)
variant_df <- full_join(variant1, variant2, by=c("date", "id", "country", "submission_type")) %>%
  select(date, country, type, value) %>%
  rename(variant=type, count=type) %>%
  separate(count, c("label", "variant")) %>%
  mutate(variant=ifelse(is.na(variant), label, variant)) %>%
  select(-label) %>%
  group_by(date) %>%
  mutate(value=as.numeric(value),
         perc=round((value/(sum(value)))*100,1))
  

#clean up the lineages
lineage <- enframe(unlist(data_raw_ugly$stats)) %>%
  separate(name, sep = "\\.", c("date", "country", "submission_type", "label_type")) %>%
  filter(country=="Malaysia") %>%
  mutate(date=as.Date(date)) %>% 
  filter(submission_type=="submissions_per_lineage") %>%
  separate(label_type, sep = "(?<=[A-Za-z])(?=[0-9])",
           c("label", "id"))

#split the dataset and rejoin
lineage1 <- lineage %>% filter (label=="count") %>%
  rename(count=label)
lineage2 <- lineage %>% filter (label=="value") %>%
  rename(type=value)
lineage_df <- full_join(lineage1, lineage2, by=c("date", "id", "country", "submission_type")) %>%
  select(date, country, type, value) %>%
  rename(lineage=type) %>%
  group_by(date) %>%
  mutate(value=as.numeric(value),
         perc=round((value/(sum(value)))*100,1))

#call transmission in 
transmission <- read.csv("P:/COVID-19/COVID19-master/ProcessedData/transmission.csv")

#clean variant df to add to transmission
gene_surv <- variant_df %>% group_by (date) %>%
  mutate(value=sum(value)) %>% ungroup() %>%
  select(date, country, value) %>%
  unique() %>%
  rename(variant=value)

#get transmission in weeks
trans_surv <- transmission %>% select(date, state, cases_new) %>%
  mutate(date=as.POSIXct(date),
         date=date+hours(6)+minutes(00)+seconds(00),
         date=ymd_hms(date),
         week_date=floor_date(date, "week"), week_start = getOption("lubridate.week.start", 7))

#combine the sets
surv_df <- trans_surv %>% 
  select(week_date, cases_new, state) %>%
  filter(state=="Malaysia") %>%
  group_by(week_date) %>%
  mutate(cases_new=sum(cases_new),
         week_date=as.Date(week_date)) %>%
  ungroup() %>% unique() %>%
  rename(country=state, date=week_date) %>%
  full_join(gene_surv, by=c("date", "country")) %>%
  mutate(perc=round((variant/cases_new)*100,1),
         perc=ifelse(is.na(perc), NA, 
                     ifelse(perc==Inf, NA, perc)),
         perc=ifelse(perc>6, 6, perc))

#plot stacked percentage for variants
#first reshape the df
variant_tmp <- variant_df %>% select(date, country, variant, perc) %>%
  pivot_wider(id_cols=c("date", "country"),
              names_from = "variant",
              values_from = "perc")

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


#plot the columns in plotly
perc_plot_variants <- plot_ly(variant_tmp, x = ~date, y = ~Other, name = 'Other variants', 
               type = 'scatter', mode = 'none', stackgroup = 'one', groupnorm = 'percent', fillcolor = '#000004FF') %>%
  add_trace(y = ~Alpha, name = 'Alpha', fillcolor = '#1D1147FF') %>%
  add_trace(y = ~Beta, name = 'Beta', fillcolor = '#51127CFF') %>%
  add_trace(y = ~Delta, name = 'Delta', fillcolor = '#822681FF') %>%
  add_trace(y = ~Theta, name = 'Theta', fillcolor = '#B63679FF') %>%
  add_trace(y = ~Kappa, name = 'Kappa', fillcolor = '#E65164FF') %>%
  add_trace(y = ~Eta, name = 'Eta', fillcolor = '#FB8861FF') %>%
  add_trace(y = ~Omicron, name = 'Omicron', fillcolor = '#FEC287FF') %>%
  add_trace(y = ~Zeta, name = 'Zeta', fillcolor = '#FCFDBFFF') %>% 
  layout(yaxis = list(title = "Percentage of variants",
                      showgrid = FALSE,
                      ticksuffix = '%'),
         titlefont=list(size=12),
         xaxis = list(title="Date",
                       showgrid = FALSE,
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
         hovermode="x unified") 
saveRDS(perc_plot_variants, paste0("plots/genomic_voc_perc.rds"))

#change to absolute counts
#first reshape the df
variant_tmp <- variant_df %>% select(date, country, variant, value) %>%
  pivot_wider(id_cols=c("date", "country"),
              names_from = "variant",
              values_from = "value") %>%
  replace(is.na(.), 0)

#plot the absolute counts
abs_plot_variants <- plot_ly(data=variant_tmp, x = ~date) %>%
  add_lines(y = ~Other, name = 'Other variants', fillcolor = '#000004FF',
            fill="tozeroy", line=list(width = 4, color = '#000004FF')) %>%
  add_lines(y = ~Alpha, name = 'Alpha', fillcolor = '#1D1147FF',
            fill="tozeroy", line=list(width = 4, color = '#1D1147FF')) %>%
  add_lines(y = ~Beta, name = 'Beta', fillcolor = '#51127CFF',
            fill="tozeroy", line=list(width = 4, color = '#51127CFF')) %>%
  add_lines(y = ~Delta, name = 'Delta', fillcolor = '#822681FF',
            fill="tozeroy", line=list(width = 4, color = '#822681FF')) %>%
  add_lines(y = ~Theta, name = 'Theta', fillcolor = '#B63679FF',
            fill="tozeroy", line=list(width = 4, color = '#B63679FF')) %>%
  add_lines(y = ~Kappa, name = 'Kappa', fillcolor = '#E65164FF',
            fill="tozeroy", line=list(width = 4, color = '#E65164FF')) %>%
  add_lines(y = ~Eta, name = 'Eta', fillcolor = '#FB8861FF',
            fill="tozeroy", line=list(width = 4, color = '#FB8861FF')) %>%
  add_lines(y = ~Omicron, name = 'Omicron', fillcolor = '#FEC287FF',
            fill="tozeroy", line=list(width = 4, color = '#FEC287FF')) %>%
  add_lines(y = ~Zeta, name = 'Zeta', fillcolor = '#FCFDBFFF',
            fill="tozeroy", line=list(width = 4, color = '#FCFDBFFF')) %>%
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
                    range=c(0,max(variant_tmp$Omicron, na.rm=T)*1.1),
                    title="Number of variants"),
       hovermode = "x unified") 
saveRDS(abs_plot_variants, paste0("plots/genomic_voc_abs.rds"))

#plot the ratio
cases_gene_plot <- plot_ly(data = surv_df, x = ~date) %>% 
  add_bars(y = ~cases_new, 
           name = "Weekly cases", marker=list(color="grey")) %>%
  add_lines(y = ~perc, name = "Percentage cases sequenced",
            fill="tozeroy",  type = 'scatter', mode = 'lines', 
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
                       range=c(min(surv_df$date, na.rm=T)-7,max(surv_df$date, na.rm=T)+7),
                       rangeslider = list(type = "date")),
         yaxis = list(side="left", tickfont= list(color = 'grey9', size=8), color='grey9', 
                      range=c(0,max(surv_df$cases_new, na.rm=T)*1.1),
                      title="Cases"),
         yaxis2 = list(overlaying = "y", side = "right",
                       tickfont = list(color = 'grey9', size=10), color='grey9', 
                       #range=c(0,max(mas_healthcare_test_df$ma_7day_tr)*1.1),
                       title = "% cases sequenced")) 
saveRDS(cases_gene_plot, paste0("plots/genomic_ratio_abs.rds"))

#chck peak of variant against the epid curve
#first joing the datasets
variant_tmp <- full_join(variant_tmp, surv_df, by=c("date"))

#plot the comparisons
cases_var_plot <- plot_ly(data = variant_tmp, x = ~date) %>% 
  add_bars(y = ~cases_new, 
           name = "Weekly cases", marker=list(color="grey")) %>%
  add_lines(y = ~Alpha, name = "Alpha",
            fill="tozeroy",  type = 'scatter', mode = 'lines', 
            yaxis = 'y2') %>%
  add_lines(y = ~Beta, name = "Beta",
            fill="tozeroy",  type = 'scatter', mode = 'lines', 
            yaxis = 'y2') %>%
  add_lines(y = ~Delta, name = "Delta",
            fill="tozeroy",  type = 'scatter', mode = 'lines', 
            yaxis = 'y2') %>%
  add_lines(y = ~Omicron, name = "Omicron",
            fill="tozeroy",  type = 'scatter', mode = 'lines', 
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
                      range=c(0,max(variant_tmp$cases_new, na.rm=T)*1.1),
                      title="Weekly cases"),
         yaxis2 = list(overlaying = "y", side = "right",
                       tickfont = list(color = 'grey9', size=10), color='grey9', 
                       range=c(0,max(variant_tmp$Omicron)*1.1),
                       title = "Weekly number of variants"),
         hovermode="x unified") 
saveRDS(cases_var_plot, paste0("plots/genomic_epi_abs.rds"))
  
  