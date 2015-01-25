
# Get McDonalds locations

options(stringsAsFactors = FALSE)

library(httr)

source('api.R')

# Get the URL for each state
if(F) {
state <- get_state()


city <- c()

for (i in state) {
  city <- c(city, get_city(i))
  Sys.sleep(10)
  print(i)
}



store <- grep('/store/view/', city, value = T)
city <- grep('/view/', city, value = T, invert = T)

save(store, file = 'store.RData')
save(city, file = 'city.RData')
}

load("~/Desktop/darrkj.github.io/blog/mcdon/city.RData")
load("~/Desktop/darrkj.github.io/blog/mcdon/store.RData")


j <- 1

done <- c()


for(i in city[!city %in% done]) {

  tmp <- get_store(i)
  j <- j + 1
  
  if(j %% 10 == 0) {
    print(j)
    Sys.sleep(7)
  }
  Sys.sleep(5)
  
  store <- c(store, tmp)
  done <- c(done, i)
}

save(store, file = 'store2.RData')

rm(done, i, j, tmp)

##############################


load("~/Desktop/darrkj.github.io/blog/mcdon/store2.RData")

done <- c()
data <- c()
j <- 1

#######

for (i in store[!store %in% done][1:500]) {
  x <- get(i)

  x <- grep('<meta name="Description"', x, value = T)
  
  data <- c(data, i)
  done <- c(done, i)
  if(j %% 10 == 0) {
    print(j)
    Sys.sleep(7)
  }
  
  Sys.sleep(5)
  j <- j + 1
}

save(data, file = 'data.RData')
save(done, file = 'done.RData')


#####


