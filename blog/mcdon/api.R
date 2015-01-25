# Function to add a wrapper around a HTTP GET request to return the 
# text html frm a given URL
get <- function(site) {
  strsplit(as.character(GET(site)), '\n')[[1]]
}


get_state <- function() {
  state <- get("http://www.mystore411.com/store/listing/17/McDonald's-store-locations")
  
  state <- grep('<td><a href="/store/list_state', state, value = T)
  
  state <- gsub('<td><a href=\"', '', state, fixed = T)
  
  ind <- as.integer(gregexpr("McDonald's-store-locations", state)) - 1
  
  state <- substr(state, 1, ind)
  
  state <- gsub(' ', '%20', state, fixed = T)
  
  paste("http://www.mystore411.com", state, "McDonald's-store-locations", sep = '')
}


get_city <- function(state, num = 1) {
  url <- paste(state, '?page=', num, '&', sep = '')  
  
  html <- get(url)
  
  city <- grep("<td><a href='/store/list_city/", html, value = T)
  
  if(length(city) != 0) {
    city <- gsub("    <td><a href='", '', city)
    
    ind <- as.integer(gregexpr("McDonald's-store-locations", city)) - 1
    
    city <- substr(city, 1, ind)
    
    city <- paste("http://www.mystore411.com", city, "McDonald", sep = '')
    
    if (length(city) == 100) {
      print(num)
      city <- c(city, get_city(state, num + 1))
    }
  } else {
    city <- grep('<td class=\"dotrow\"><a href=\"', html, value = T)
    
    city <- gsub('                <td class=\"dotrow\"><a href=\"', '', city)
    
    ind <- as.integer(gregexpr('">Mcdonalds', city)) - 1
    
    city <- substr(city, 1, ind)
    
    city <- paste("http://www.mystore411.com", city, sep = '')
  }
  
  city
}


get_store <- function(site) {
  
  site <- get(site)
  
  site <- grep('<a href=\"/store/view/', site, value = TRUE)
  
  site <- gsub('                 <a href=\"', '', site)
  
  ind <- as.integer(gregexpr('">Mcdonalds', site)) - 1
  
  site <- substr(site, 1, ind)
  
  paste("http://www.mystore411.com", site, sep = '')
}
