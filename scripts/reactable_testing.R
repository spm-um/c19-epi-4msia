library(reactable)
library(htmltools)
library(tidyverse)
library(RColorBrewer)

#pull data
testing <- read.csv('data/testing.csv')

#strcuture data for reactable
tpr_cols <- c("ma_7day_tpr", "change_tpr")
tr_cols <- c("ma_7day_tr", "change_tr")

testing <- testing %>% group_by(state) %>%
  mutate(date=as.Date(date),
         ma_7day_tr=round(as.numeric(ma_7day_tr),1),
         ma_7day_tpr=ma_7day_tpr/100) %>%
  filter(date==max(date)) %>%
  ungroup() %>% as.data.frame()
testing <- testing[, c("state", tpr_cols, tr_cols)]

change_column <- function(maxWidth = 55, ...) {
  colDef(maxWidth = maxWidth, align = "center", class = "cell number", ...)
}

tr_column <- function(class = NULL, ...) {
  colDef(maxWidth = 70, align = "center", class = paste("cell number", class), ...)
}

tpr_column <- function(maxWidth = 70, class = NULL, ...) {
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

format_pct <- function(value) {
  if (value == 0) "  \u2013 "    # en dash for 0%
  else if (value == 1) "\u2713"  # checkmark for 100%
  else if (value < 0.0001) " <0.1%"
  else if (value > 0.20) ">20%"
  else formatC(paste0(round(value * 100,2), "%"), width = 4)
}

make_color_pal <- function(colors, bias = 1) {
  get_color <- colorRamp(colors, bias = bias)
  function(x) rgb(get_color(x), maxColorValue = 255)
}

off_rating_color <- make_color_pal(c("#ff2700", "#f8fcf8", "#44ab43"), bias = 1.3)
off_rating_color2 <- make_color_pal(c("#44ab43", "#f8fcf8",  "#ff2700"), bias = 1.3)
col <- rev(as.vector(viridis::viridis(100)))
knockout_pct_color <- make_color_pal(col, bias = 2)




brewer.pal(100, "YlGn")

tbl <- reactable(
  testing,
  pagination = FALSE,
  defaultColGroup = colGroup(headerClass = "group-header"),
  columnGroups = list(
    colGroup(name = "Test Positivity ratio", columns = tpr_cols),
    colGroup(name = "Testing rate", columns = tr_cols)
  ),
  defaultColDef = colDef(class = "cell", headerClass = "header"),
  columns = list(
    state = colDef(
      minWidth = 50,
      headerStyle = list(fontWeight = 700), 
      name="State",
      cell = function(value) {
        img_src <- knitr::image_uri(sprintf("images/%s.png", value))
        image <- img(src = img_src, height = "36px", alt = "")
        tagList(
          div(style = list(display = "inline-block", width = "70px"), image),
          value
        )
      }
    ),
    # date = colDef(defaultSortOrder = "asc", align = "center", maxWidth = 75, name="Date",
    #                class = "cell group", headerStyle = list(fontWeight = 700)),
    ma_7day_tpr = tpr_column(name = "Ratio", maxWidth = 90 ),
    change_tpr = change_column(
      name = "% change*",
      maxWidth = 90,
      cell = function(value) {
        scaled <- 1-(value - min(testing$change_tpr)) / (max(testing$change_tpr) - min(testing$change_tpr))
        color <- off_rating_color(scaled)
        value <- format(round(value, 1), nsmall = 1)
        div(class = "date-rating", style = list(background = color), value)
      }
    ),
    
    ma_7day_tr = tr_column(name = "per 1k pop", class = "border-left"),
    change_tr = change_column(
      name = "% change*", 
      maxWidth = 90,
      defaultSortOrder = "asc",
      cell = function(value) {
        scaled <- 1 - (value - min(testing$change_tr)) / (max(testing$change_tr) - min(testing$change_tr))
        color <- off_rating_color2(scaled)
        value <- format(round(value, 1), nsmall = 1)
        div(class = "date-rating", style = list(background = color), value)
      }
    )
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
    "* absolute change from 7-days prior, 
    Negative (TPR) = decrease TPR, Positive (TPR) = increase TPR,
    Negative (TR) = decrease TR, Positive (TR) = increase TR"
)


saveRDS(tbl, 'plots/testing_tbl.rds')