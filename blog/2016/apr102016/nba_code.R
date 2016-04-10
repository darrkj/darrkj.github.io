

# This site has inforamation on all players. 
'http://www.databasebasketball.com/players/playerlist.htm?lt=' %>%
  paste0(letters) %>% 
  map_df(~readHTMLTable(.)[[2]]) %>%
  select(name = `Player Name`, span = `Years Played`, 
         pos = Position, college = College) -> plys


```{r}
# Clean (HOF) out of name
plys$name <- gsub(' (HOF)', '', plys$name, fixed = T)

# Need to pull out names from this format.
# 'last, first' -> 'last', 'first'
plys$last <- sapply(strsplit(plys$name, ','), `[[`, 1)
plys$first <- sapply(strsplit(plys$name, ','), `[[`, 2)



# Remove leading space from string split on comma
plys$first <- substr(plys$first, 2, nchar(plys$first))

# create rookie and retire
plys$rookie <- as.numeric(sapply(strsplit(plys$span, '-'), `[[`, 1))
plys$retire <- as.numeric(sapply(strsplit(plys$span, '-'), `[[`, 2))

# Drop fields that are no longer needed.
plys %>% select(-name, -span) -> plys

plys$name <- paste(plys$first, plys$last, sep = ' ')
```
