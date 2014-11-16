http://espn.go.com/mma/fightcenter?eventId=400557635


get_event <- function(url) {
  
  clean <- function(x) {
    x %>% html_attrs() %>% sapply(function(x) x[[1]]) %>% as.character() %>%
      gsub('http://espn.go.com/mma/fighter/_/id/', '', .) %>% strsplit('/') %>%
      sapply(function(x) x[[1]][1])
  }
    
  url %>% html() -> html
    
  html %>% html_nodes('.winner') %>% html_attrs() %>% 
    sapply(function(x) x[[1]]) %>% strsplit(' ') %>% 
    sapply(function(x) x[[2]]) -> w
    
  html %>% html_nodes('.player1 a') %>% clean() -> a
  html %>% html_nodes('.player2 a') %>% clean() -> b
  
  data.frame(winner = ifelse(w == 'fighter1', a, b),
             loser = ifelse(w == 'fighter1', b, a))
}



new_event <- function(url) {
  
  clean <- function(x) {
    x %>% html_attrs() %>% sapply(function(x) x[[1]]) %>% as.character() %>%
      gsub('http://espn.go.com/mma/fighter/_/id/', '', .) %>% strsplit('/') %>%
      sapply(function(x) x[[1]][1])
  }
  
  url %>% html() -> html
  
  html %>% html_nodes('.player1 a') %>% clean() -> a
  html %>% html_nodes('.player2 a') %>% clean() -> b
  
  data.frame(a, b)
}





new_pred2 <- function(event) {
  
  ev <- get_event(event)
  fig <- merge(data.frame(id = c(ev$winner, ev$loser)), f2, all.x = T)
  
  fig %>% 
    mutate(data = ymd(date)) %>%
    select(id, date, exp, cl, bet, eig, hub, auth, page) %>%
    group_by(id) %>%
    mutate(max = max(date)) %>%
    filter(max == date) %>%
    select(-max, -date) %>%
    as.data.frame -> cc
  
  cc[is.na(cc)] <- 0
  oa <- a <- ev
  names(a) <- c('id', 'oid')
  names(oa) <- c('oid', 'id')
  a <- rbind(a, oa)
  bb <- cc
  names(bb) <- paste0('o', names(bb))
  
  ndata <- merge(a, cc)
  ndata <- merge(ndata, bb)
  
  ndata$expDiff <- ndata$exp - ndata$oexp
  ndata$clDiff <- ndata$cl - ndata$ocl
  ndata$betDiff <- ndata$bet - ndata$obet
  ndata$eigDiff <- ndata$eig - ndata$oeig
  ndata$hubDiff <- ndata$hub - ndata$ohub
  ndata$authDiff <- ndata$auth - ndata$oauth
  ndata$pageDiff <- ndata$page - ndata$opage
  
  
  ndata$p <- as.numeric(predict(mod, newdata = ndata[, -c(1:2)], type = 'response'))
  
  w <- l <- ndata[, c('id', 'p')]
  names(w) <- c('winner', 'wp')
  names(l) <- c('loser', 'lp')
  nev <- merge(ev, w)
  merge(nev, l)
}



past <- c('http://espn.go.com/mma/fightcenter?eventId=400603491',
          'http://espn.go.com/mma/fightcenter?eventId=400601611',
          'http://espn.go.com/mma/fightcenter?eventId=400592231',
          'http://espn.go.com/mma/fightcenter?eventId=400582487',
          'http://espn.go.com/mma/fightcenter?eventId=400582010',
          'http://espn.go.com/mma/fightcenter?eventId=400561337')



new_pred(past[1])
new_pred(past[2])
new_pred(past[3])
new_pred(past[4])
new_pred(past[5])
new_pred(past[6])




new_pred2 <- function(event) {
  
  ev <- new_event(event)
  fig <- merge(data.frame(id = c(ev$a, ev$b)), f2, all.x = T)
  
  fig %>% 
    mutate(data = ymd(date)) %>%
    select(id, date, exp, cl, bet, eig, hub, auth, page) %>%
    group_by(id) %>%
    mutate(max = max(date)) %>%
    filter(max == date) %>%
    select(-max, -date) %>%
    as.data.frame -> cc
  
  cc[is.na(cc)] <- 0
  oa <- a <- ev
  names(a) <- c('id', 'oid')
  names(oa) <- c('oid', 'id')
  a <- rbind(a, oa)
  bb <- cc
  names(bb) <- paste0('o', names(bb))
  
  ndata <- merge(a, cc)
  ndata <- merge(ndata, bb)
  
  ndata$expDiff <- ndata$exp - ndata$oexp
  ndata$clDiff <- ndata$cl - ndata$ocl
  ndata$betDiff <- ndata$bet - ndata$obet
  ndata$eigDiff <- ndata$eig - ndata$oeig
  ndata$hubDiff <- ndata$hub - ndata$ohub
  ndata$authDiff <- ndata$auth - ndata$oauth
  ndata$pageDiff <- ndata$page - ndata$opage
  
  
  ndata$p <- as.numeric(predict(mod, newdata = ndata[, -c(1:2)], type = 'response'))
  
  w <- l <- ndata[, c('id', 'p')]
  names(w) <- c('a', 'ap')
  names(l) <- c('b', 'bp')
  nev <- merge(ev, w)
  merge(nev, l)
}




x <- new_pred2('http://espn.go.com/mma/fightcenter?eventId=400555416')

nd <- data[, c('id', 'name')]
names(nd) <- c('a', 'aname')
x <- merge(x, nd)

names(nd) <- c('b', 'bname')
x <- merge(x, nd)
x[, c(5, 3, 4, 6)]
#







