
library(V8)

data(diamonds, package = "ggplot2")

# Create JavaScript session
ct <- new_context()

# Assign diamonds variable inside the js interpreter. 
ct$assign("diamonds", diamonds)

# Load CrossFilter JavaScript library
ct$source("http://cdnjs.cloudflare.com/ajax/libs/crossfilter/1.3.11/crossfilter.min.js")

ct$console()
# This is V8 version 3.14.5.10. Press ESC or CTRL+C to exit.
# ~


//now we are in javasript :)
var cf = crossfilter(diamonds)
var price = cf.dimension(function(x){return x.price})
var depth = cf.dimension(function(x){return x.depth})
price.filter([2000, 3000])
output = depth.top(10)

JSON.stringify(output)


# Pressing ESC
# Exiting V8 console.
output <- ct$get("output")
print(output)


