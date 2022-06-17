library(reactable)
library(htmltools)
library(tidyverse)
library(lubridate)
library(viridis)

#load data
vaccination <- read.csv('data/vaccination.csv')
cfr <- read.csv('data/cfr.csv')

#convert character to date
vax_summary_total <- vaccination %>% 
  mutate(date=as.Date(date)) %>%
  filter(date==max(date)& state=="Malaysia") %>%
  select(date, perc_vax2, perc_booster, perc_vax2_adult, perc_booster_adult, perc_vax2_child) %>%
  mutate(quarter=NA)

vax_summary_quarter <- vaccination %>% 
  mutate(date=as.Date(date),
         quarter=quarter(date, with_year = T, fiscal_start = 1)) %>%
  group_by(quarter) %>%
  filter(date==max(date))%>%
  filter(date==max(date)& state=="Malaysia") %>%
  select(date, perc_vax2, perc_booster, perc_vax2_adult, perc_booster_adult, perc_vax2_child)

vax_summary=bind_rows(vax_summary_total,vax_summary_quarter) 

cfr_summary_age <- cfr %>% 
  mutate(date=as.Date(date)) %>%
  group_by(age) %>%
  summarise(cfr=(sum(ma_death)/sum(ma_case))*100) %>%
  mutate(quarter=NA)

cfr_summary_quarter_age <- cfr %>% 
  mutate(date=as.Date(date)) %>%
  group_by(age, quarter) %>%
  summarise(cfr=(sum(ma_death)/sum(ma_case))*100)

cfr_summary <- bind_rows(cfr_summary_age,cfr_summary_quarter_age) %>%
  pivot_wider(id_cols="quarter",
              names_from = "age",
              values_from = "cfr") %>%
  full_join(vax_summary, by="quarter") %>%
  separate(quarter, c('year', 'quart')) %>%
  mutate(quart=ifelse(quart==1, "Jan-March",
                      ifelse(quart==2, "Apr-Jun",
                             ifelse(quart==3, "July-Sep", "Oct-Dec"))),
         quarter=paste0(quart, " ", year),
         quarter=ifelse(quarter=="NA NA", "All time", quarter),
         `<18 years`=`<18 years`/100,
         `>59 years`=`>59 years`/100,
         `18-59 years`=`18-59 years`/100,
         `All age groups`=`All age groups`/100,
         perc_vax2=perc_vax2/100,
         perc_booster=perc_booster/100,
         perc_vax2_adult=perc_vax2_adult/100,
         perc_booster_adult=perc_booster_adult/100,
         perc_vax2_child=perc_vax2_child/100) %>%
  select(-c(year, quart))
cfr_summary <- cfr_summary[, c("quarter", "All age groups" , "perc_vax2", "perc_booster",
                               "<18 years", "perc_vax2_child",  "18-59 years", 
                               ">59 years", "perc_vax2_adult", "perc_booster_adult")]

#build table
all_cols <- c("All age groups", "perc_vax2", "perc_booster")
under18_cols <- c("<18 years", "perc_vax2_child")
above18_cols <- c("18-59 years")
above60_cols <- c(">59 years", "perc_vax2_adult", "perc_booster_adult")

perc_column <- function(maxWidth = 70, class = NULL, ...) {
  colDef(
    cell = format_pct,
    maxWidth = maxWidth,
    class = paste("cell number", class),
    style = function(value) {
      # Lighter color for <1%
      if (value < 0.0001) {
        list(color = "#aaa")
      } else {
        list(color = "#111", background = knockout_pct_color(value))
      }
    },
    ...
  )
}

empty_column <- function(class = NULL, ...) {
  colDef(maxWidth = 70, align = "center", class = paste("cell number", class), ...)
}

format_pct <- function(value) {
  if (value == 0) "  \u2013 "    # en dash for 0%
  else if (value == 1) "\u2713"  # checkmark for 100%
  else if (value < 0.0001) " <0.01%"
  else if (value > 0.99) ">99%"
  else formatC(paste0(round(value * 100, 2), "%"), width = 4)
}

make_color_pal <- function(colors, bias = 1) {
  get_color <- colorRamp(colors, bias = bias)
  function(x) rgb(get_color(x), maxColorValue = 255)
}

col <- rev(as.vector(viridis::viridis(100)))
knockout_pct_color <- make_color_pal(col, bias = 2)


tbl <- reactable(
  cfr_summary,
  pagination = FALSE,
  defaultColGroup = colGroup(headerClass = "group-header"),
  columnGroups = list(
    colGroup(name = "Cumulative", columns = all_cols),
    colGroup(name = "<18 years", columns = under18_cols),
    colGroup(name = "18-59 years", columns = above18_cols),
    colGroup(name = ">59 years", columns = above60_cols)
  ),
  defaultColDef = colDef(class = "cell", headerClass = "header"),
  columns = list(
    quarter = colDef(defaultSortOrder = "asc", align = "center", minWidth = 75, 
                     name="Quarter",
                     class = "cell group", headerStyle = list(fontWeight = 700)),
    
    `All age groups` = perc_column(name = "CFR (%)", maxWidth = 90 ),
    perc_vax2 = empty_column(name = "Full vaccination (%)"),
    perc_booster = empty_column(name = "Booster vaccination (%)"),
    
    `<18 years` = perc_column(name = "CFR (%)", maxWidth = 90 ),
    perc_vax2_child = empty_column(name = "Full vaccination (%)"),
    
    `18-59 years` = perc_column(name = "CFR (%)", maxWidth = 90 ),
    
    
    `>59 years` = perc_column(name = "CFR (%)", maxWidth = 90 ),
    perc_vax2_adult = empty_column(name = "Full vaccination (%)"),
    perc_booster_adult = empty_column(name = "Booster vaccination (%)")
  ),
  # Emphasize borders between groups when sorting by group
  rowClass = JS("
    function(rowInfo, state) {
      const firstSorted = state.sorted[0]
      if (firstSorted && firstSorted.id === 'group') {
        const nextRow = state.pageRows[rowInfo.viewIndex + 1]
        if (nextRow && rowInfo.row.group !== nextRow.group) {
          return 'group-last'
        }
      }
    }"
  ),
  showSortIcon = FALSE,
  borderless = TRUE,
  class = "standings-table")



div(class = "standings",
    div(class = "title",
        h2(" "),
        ""
    ),
    tbl,
    " "
)

saveRDS(tbl, 'plots/cfr_tbl.rds')