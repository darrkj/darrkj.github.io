
valid_names <- c('', 3)


add_names <- function(df, names) {
  names(df) <- names
  df
}

data <- 'file.csv' %>%
  read_csv(col_names = FALSE) %>% 
  add_names(valid_names)




df$name %>%
  recode("PERRY, JAMES R (RICK)" = "PERRY, JAMES R") %>%
  recode("CARSON, BENJAMIN S SR MD" = "CARSON, BENJAMIN S") %>%
  recode("CRUZ, RAFAEL EDWARD \"TED\"" = "CRUZ, RAFAEL EDWARD") %>%
  recode("SKEWES, PETER ALAN PH.D." = "SKEWES, PETER ALAN") %>%
  recode("DE  LA  FUENTE, ROQUE ROCKY" = "DE LA FUENTE, ROQUE ROCKY") %>%
  recode("WILLIAM BILL WELD, GARY JOHNSON /" = "JOHNSON, GARY / WILLIAM BILL WELD, ") %>%
  recode("MCMULLIN / NATHAN DANIEL, EVAN JOHNSON" = "MCMULLIN, EVAN / NATHAN DANIEL, JOHNSON") -> df$name

