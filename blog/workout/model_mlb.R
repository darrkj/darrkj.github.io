
options(stringsAsFactors = FALSE)

library(XML)
library(rvest)
library(dplyr)
library(recurBind)
library(lubridate)



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


games <- getGames(teams)


games %>% 
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
game$date <- mdy(paste(game$date, '2014', sep = ' '))





game$g <- NULL
game$year <- NULL
game$result <- NULL
game$diff <- NULL
game$w <- NULL
game$l <- NULL



game$opp <- gsub('vs', '', game$opp)
game$opp <- gsub('@', '', game$opp)



t <- rbind(
c("Boston",        "bos/boston-red-sox" ),
c("Detroit",       "det/detroit-tigers" ),
c("NY Yankees",    "nyy/new-york-yankees"),
c("Toronto",       "tor/toronto-blue-jays"),
c("Tampa Bay",     "tb/tampa-bay-rays"),
c("Kansas City",   "kc/kansas-city-royals"),
c("Pittsburgh",    "pit/pittsburgh-pirates"), 
c("Minnesota",     "min/minnesota-twins" ),
c("Houston",       "hou/houston-astros" ),
c("Cleveland",     "cle/cleveland-indians"),
c("Milwaukee",     "mil/milwaukee-brewers"),
c("Texas",         "tex/texas-rangers"),
c("Oakland",       "oak/oakland-athletics" ),
c("White Sox",     "chw/chicago-white-sox" ),
c("Washington",    "wsh/washington-nationals"), 
c("LA Angels",     "laa/los-angeles-angels" ),
c("Seattle",       "sea/seattle-mariners" ),
c("St. Louis",     "stl/st-louis-cardinals"),
c("Cubs",          "chc/chicago-cubs" ),
c("Cincinnati",    "cin/cincinnati-reds"),
c("Baltimore",     "bal/baltimore-orioles"), 
c("Atlanta",       "atl/atlanta-braves") ,
c("NY Mets",       "nym/new-york-mets"),
c("Miami",         "mia/miami-marlins" ),
c("Philadelphia",  "phi/philadelphia-phillies"),
c("Colorado",      "col/colorado-rockies"),
c("Arizona",       "ari/arizona-diamondbacks"),
c("San Diego",     "sd/san-diego-padres" ),
c("LA Dodgers",    "lad/los-angeles-dodgers" ),
c("San Francisco", "sf/san-francisco-giants") 
)


for (i in 1:nrow(t)) {
  game$opp <- gsub(t[i, 1], t[i, 2], game$opp)
}

pl1 <- function(var, p) {
  sapply(strsplit(var, '/'), `[`, p)
}

game$opp <- pl1(game$opp, 1)
game$team <- pl1(game$team, 1)
#


library(R6)
Team <- R6Class("Team",
                  public = list(
                    name = NA,
                    games = NA,
                    initialize = function(name) {
                      if (!missing(name)) self$name <- name
                      self$games = data.frame(score = numeric(), 
                                         oscore = numeric())
                    },
                    rec = function() {
                      tmp <- self$games
                      c(sum(tmp$score > tmp$oscore), sum(tmp$score < tmp$oscore))
                    },
                    earn = function() {
                      mean(self$games$score)
                    },
                    allow = function() {
                      mean(self$games$oscore)
                    },
                    game = function(x, y) {
                      tmp <- rbind(self$games, c(score = x, oscore = y))
                      names(tmp) <- c('score', 'oscore')
                      self$games <- tmp
                    }
                  )
)



paste(sort(c(game$team[1], game$opp[1])), collapse = '-')

gg <- game[game$p == 'home', ]
gg$p <- NULL
names(gg) <- c('date', 'away', 'home', 'hs', 'as')


#
name <- unique(game$team)
teams <- list()
for (i in name) {
  teams[[i]] <- Team$new(i)
}


gg <- gg[order(gg$date), ]


pred <- function(g) {
  g <- gg[1, ]
  he <- teams[[g$home]]$earn()
  ha <- teams[[g$home]]$allow()
  ae <- teams[[g$away]]$earn()
  aa <- teams[[g$away]]$allow()
  
}


overall <- data.frame(he = 0, ha = 0, ae = 0, aa = 0, h = 0, a = 0)
for(i in 1:nrow(gg)) {
  tmp <- gg[i, ]
  overall[i, ] <- c(teams[[tmp$home]]$earn(), teams[[tmp$home]]$allow(),
                    teams[[tmp$away]]$earn(), teams[[tmp$away]]$allow(),
                    tmp$hs, tmp$as)
  teams[[tmp$home]]$game(tmp$hs, tmp$as)
  teams[[tmp$away]]$game(tmp$as, tmp$hs)
  print(i)
}




overall <- overall[complete.cases(overall), ]

overall$diff <- overall$h - overall$a
#overall$h <- overall$a <- NULL

#
