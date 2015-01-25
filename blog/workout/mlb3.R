
options(stringsAsFactors = FALSE)

library(XML)
library(rvest)
library(dplyr)
library(recurBind)


'http://espn.go.com/mlb/teams' %>% 
  html %>% html_nodes('h5 a') %>% html_text %>% 
  gsub(' ' , '-', .) -> teams



# functor

`%->%` <- function(lhs, rhs) {
  lapply(lhs, rhs)
}



. %>% 
  gsub('xxxx', 2014, .) %>% 
  paste0('http://espn.go.com/mlb/team/schedule/_/name/', .) %>% 
  html %>% 
  html_nodes('.mod-thirdnav-tabs a') %>% 
  html_attr('href') %>% 
  {
    rbind(
      .[2] %>% readHTMLTable %>% .[[1]],
      .[3] %>% readHTMLTable %>% .[[1]]
    )
  } %>%
  cbind(., value, g = 1:nrow(.)) -> getGames




teams %->% getGames -> games
