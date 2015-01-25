
options(stringsAsFactors = FALSE)

library(XML)
library(rvest)
library(dplyr)
library(recurBind)



teams <- c(
  "bal/baltimore-orioles", "bos/boston-red-sox",
  "nyy/new-york-yankees", "tb/tampa-bay-rays",
  "tor/toronto-blue-jays", "chw/chicago-white-sox",
  "cle/cleveland-indians", "det/detroit-tigers",
  "kc/kansas-city-royals", "min/minnesota-twins",
  "hou/houston-astros", "laa/los-angeles-angels",
  "oak/oakland-athletics", "sea/seattle-mariners",
  "tex/texas-rangers", "atl/atlanta-braves",
  "mia/miami-marlins", "nym/new-york-mets",
  "phi/philadelphia-phillies", "wsh/washington-nationals",
  "chc/chicago-cubs", "cin/cincinnati-reds",
  "mil/milwaukee-brewers", "pit/pittsburgh-pirates",
  "stl/st-louis-cardinals", "ari/arizona-diamondbacks",
  "col/colorado-rockies", "lad/los-angeles-dodgers",
  "sd/san-diego-padres", "sf/san-francisco-giants")

pl <- function(var, p) {
  as.numeric(sapply(strsplit(var, '-'), `[`, p))
}


getGames <- function(team, year = 2014) {
  team %>% 
    gsub('/', paste0('/year/', year, '/'), .) %>% 
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
    select(date = V1, opp = V2, score = V3, record = V4) %>%
    filter(opp != 'OPPONENT') %>% 
    filter(score != 'POSTPONED') %>%
    cbind(., team, g = 1:nrow(.), year) %>%
    mutate(team = gsub('year/xxxx/', '', team))
}


games <- list()

for(i in 2014) {
  lapply(teams, getGames, i) %>% 
    recurBind %>% 
    .[[1]] -> games[[as.character(i)]]
  print(i)
}


games %>% 
  recurBind %>% 
  .[[1]] %>%
  mutate(score = gsub(paste0(' F/', 0:30, collapse = '|'), '', score),
         result = substr(score, 1, 1)) %>%
  filter(score != "2:10 PM", score != "5:05 PM", result %in% c('L', 'W')) %>%
  mutate(score = substr(score, 2, nchar(score)),
         hs = ifelse(result == 'W', pl(score, 1), pl(score, 2)),
         os = ifelse(result == 'W', pl(score, 2), pl(score, 1)),
         diff = hs - os,
         w = pl(record, 1), l = pl(record, 2),
         p = ifelse(substr(opp, 1, 1) == '@', 'away', 'home')) %>%
  select(-score, -record) -> game

game$date <- substr(game$date, 6, nchar(game$date)) 
game$date <- paste(game$date, '2014', sep = ' ')

game$d <- mdy(game$date)

library(lubridate)

game %>%
  group_by(team, year) %>%
  mutate(mdiff = mean(hs - os),
         last = max(g),
         rat = w / (w + l),
         mearn = mean(hs), 
         searn = sd(hs),
         mallow = mean(os),
         sallow = sd(os)) %>%
  as.data.frame -> g
  
  
  
  g %>%
  filter(last == g) %>%
  select(team, year, mdiff, rat) %>%
  distinct 

game %>% group_by(team) %>% summarise(mean = mean(h), sd = sd(h))



diff <- game[game$diff > 0, ]$diff



game$score <- NULL


#





#
