options(stringsAsFactors = FALSE)

library(rvest)
library(dplyr)

# These are all of the pages that have jobs on them.
site <- paste0('https://github.com/neo4j/neo4j/commits/master?page=', 1:864)


get_commits <- function(url) {
  # This gets all info needed to parse stuff out.
  site[1] %>% html() %>% html_nodes('.commit div p a') %>% 
    html_attrs -> xx

  yy <- xx[sapply(xx, function(x) 'title' %in% names(x))]

  zz <- sapply(yy, `[[`, 'title')
  yy[grep('^Merge', zz, invert = T, perl = T)] -> aa

  aa <- sapply(aa, `[[`, 'href')

  bb <- grep('commit', aa, value = T)

  paste0('https://github.com', bb)
}

x <- c()
for (i in site) {
  x <- c(x, get_commits(i)) 
  print(i)
}


'https://github.com/neo4j/neo4j/commit/0790c11c70fa180a7cb57a5017e5b718d45e8113' %>%
  html -> html

%>% html_nodes('li') %>% html_text


html %>% html_nodes('li span') %>% html_attrs()
