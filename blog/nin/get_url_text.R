url <- 'http://theincidentaleconomist.com/wordpress/patients-do-better-when-cardiologists-are-away-at-academic-meetings/'

url %>% html %>% html_nodes('body div') %>% html_text %>%
  gsub('\r', '', ., fixed = T) %>%
  gsub('\n', '', ., fixed = T) %>%
  gsub('\t', '', ., fixed = T) %>%
  paste(collapse = ' ')



