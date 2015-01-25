
# The data came from the following.
# http://blog.splitwise.com/2013/09/18/the-2010-us-census-population-by-zip-code-totally-free/
# This was becuase I never found what I was looking for on the Census site


cen <- read.csv('2010+Census+Population+By+Zipcode+(ZCTA).csv')

# Total population
sum(cen[, 2]) / 1000000
# This sounds about right
# As a comparison
# https://www.google.com/webhp?sourceid=chrome-instant&ion=1&espv=2&ie=UTF-8#q=us+population


