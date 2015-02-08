
Queue <- 
  R6Class("Queue",
          public = list(
            data = NA,
            #
            initialize = function(data, ...) {
              if (!missing(data)) {
                self$data <- as.list(c(data, ...))
              } else {
                self$data <- list()
              }
            },
            #
            size = function() length(self$data),
            #
            push = function(item) self$data[[self$size() + 1]] <- item,
            #
            pop = function() {
              if (self$size() == 0) return(NULL) # stop("queue is empty!")
              value <- self$data[[1]]
              self$data[[1]] <- NULL
              value
            }
          )
  )

Stack <- 
  R6Class("Stack",
          public = list(
            data = NA,
            #
            initialize = function(data, ...) {
              if (!missing(data)) {
                self$data <- as.list(c(data, ...))
              } else {
                self$data <- list()
              }
            },
            #
            size = function() length(self$data),
            #
            push = function(item) self$data[[self$size() + 1]] <- item,
            #
            pop = function() {
              if (self$size() == 0) return(NULL)
              value <- self$data[[self$size()]]
              self$data[[self$size()]] <- NULL
              value
            }
          )
  )
