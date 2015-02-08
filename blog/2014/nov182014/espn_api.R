
clean <- function(remove, find) {
  gsub(remove, '', maybe(grep(find, meta, value = T)))
}

create_espn_lookup <- function() {
  'http://espn.go.com/mma/fighters' %>% html() -> html
  
  html %>% html_nodes('.evenrow, a') %>% html_attrs() -> even
  html %>% html_nodes('.oddrow, a') %>% html_attrs() -> odd
  
  inter(odd, even) %>%
    grep('/fighter/', ., value = T) %>%
    sapply(function(x) x[[1]]) %>%
    as.character() %>%
    unique() %>%
    gsub('/mma/fighter/', 'http://espn.go.com/mma/fighter/stats/', .)
}


anything_to_pull <- function(url) {
  gsub('stats/_', '_', url) %>% html() %>% html_nodes('td') %>% 
    html_text() -> text
  !any(text == "No Match Results at this time.")
}

get_espn_metadata <- function(url) {
  #name <- lookup[lookup$url == url, ]$name
  name <- gsub('-', ' ', tail(strsplit(url, '/')[[1]], 1))
  url2 <- gsub('/stats/', '/history/', url)
  
  html <- url %>% html()
  
  bio <- html %>% html_nodes('.general-info li') %>% html_text()
  meta <- html %>% html_nodes('.player-metadata li') %>% html_text()
  meta_rm <- html %>% html_nodes('.player-metadata span') %>% html_text()
  stat <- html %>% html_nodes('div .player-stats td') %>% html_text()
  stat_name <- html %>% html_nodes('div .player-stats th') %>%  html_text()
  
  if(anything_to_pull(url)) {
    history <- espn_history(url2)
    stats <- espn_stats(url)
  } else {
    history <- NULL
    stats <- NULL
  }
  
  list(name = name, url = url, bio = bio, meta = meta, meta_rm = meta_rm, 
       stat = stat, stat_name = stat_name, history = history, stats = stats)
}


espn_stats <- function(url) {
  #if( !tolower(fighter) %in% lookup$name ) return(NA)
  
  url %>% html() %>% html_nodes('div div div td') %>%
    html_text() -> stats
  
  id <- grep('STANDING STATISTICS', stats)
  
  stats <- stats[(id + 1):length(stats)]
  
  stats <- stats[!stats %in% c("GROUND STATISTICS", "CLINCH STATISTICS")]
  
  stats <- matrix(stats, ncol = 14, byrow = T)
  ind <- which(stats == 'DATE')
  
  xx <- data.frame(cbind(stats[1:(ind[2]-1), ],
                         stats[ind[2]:(ind[3]-1), ],
                         stats[ind[3]:nrow(stats), ]))
  
  xx <- xx[, -c(15, 16, 29, 30)]
  names(xx) <- xx[1, ]
  xx[-1, ]
}



espn_history <- function(url) {
  #if( !tolower(fighter) %in% lookup$name ) return(NA)
  
  url %>%
    html() %>% 
    html_nodes('div div div td') %>%
    html_text() -> hist
  
  id <- grep('DATE', hist)
  
  hist <- hist[(id):length(hist)]
  
  hist <- matrix(hist, ncol = 7, byrow = T)
  
  xx <- data.frame(hist)
  
  url %>%
    html() %>% html_nodes('td a') %>% html_attrs() -> opp
  opp <- sapply(opp, '[[', 1)
  opp <- grep('fighter/_/', opp, value = T)
  opp <- gsub('http://espn.go.com/mma/fighter/_/id/', '', opp)
  opp <- sapply(strsplit(opp, '/'), `[`, 1)
  
  names(xx) <- xx[1, ]
  xx <- xx[-1, ]
  xx <- xx[xx$OPPONENT != 'TBD', ]
  cbind(xx, oppID = opp)
}


height <- function(h) {
  feet <- as.numeric(sapply(strsplit(h, "'"), `[`, 1))
  inch <- as.numeric(sapply(strsplit(h, "'"), `[`, 2))
  feet * 12 + inch
}

