library(R6)

Queue_r6 <- R6Class("Queue",
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


Queue_r6_s3 <- R6Class("Queue",
  class = FALSE,
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


Queue_r6_port <- R6Class("Queue",
                       portable = FALSE,
                       public = list(
                         data = NA,
                         #
                         initialize = function(data, ...) {
                           if (!missing(data)) {
                             data <<- as.list(c(data, ...))
                           } else {
                             data <<- list()
                           }
                         },
                         #
                         size = function() length(data),
                         #
                         push = function(item) data[[size() + 1]] <<- item,
                         #
                         pop = function() {
                           if (size() == 0) return(NULL) # stop("queue is empty!")
                           value <- data[[1]]
                           data[[1]] <<- NULL
                           value
                         }
                       )
)


Queue_r6_port_s3 <- R6Class("Queue",
                            class = FALSE,
                         portable = FALSE,
                         public = list(
                           data = NA,
                           #
                           initialize = function(data, ...) {
                             if (!missing(data)) {
                               data <<- as.list(c(data, ...))
                             } else {
                               data <<- list()
                             }
                           },
                           #
                           size = function() length(data),
                           #
                           push = function(item) data[[size() + 1]] <<- item,
                           #
                           pop = function() {
                             if (size() == 0) return(NULL) # stop("queue is empty!")
                             value <- data[[1]]
                             data[[1]] <<- NULL
                             value
                           }
                         )
)


Queue_r5 <- setRefClass(
  Class = "Queue",
  fields = list(name = "character", data = "list"),
  methods = list(
    size = function() length(data),
    #
    push = function(item) data[[size()+1]] <<- item,
    #
    pop = function() {
      if (size() == 0) stop("queue is empty!")
      value <- data[[1]]
      data[[1]] <<- NULL
      value
    },
    #
    initialize=function(...) {
      callSuper(...)
      .self
    }
  )
)

q_comp <- function(str, num = 100) {
  cont <- TRUE
  eval(parse(text = paste0('x <- ', str)))
  for(i in seq(num)) {
    x$push(i)
  }
  while(cont) {
    x$pop()
    if (x$size() == 1) cont <- FALSE
  }
}


library(rbenchmark)

benchmark(replications = rep(100, 3),
          q_comp('Queue_r6$new()'),
          q_comp('Queue_r6_s3$new()'),
          q_comp('Queue_r6_port$new()'),
          q_comp('Queue_r6_port_s3$new()'),
          q_comp('Queue_r5$new()'),
          columns=c('test', 'elapsed', 'replications', 'relative'))

benchmark(replications = rep(100, 3),
          q_comp('Queue_r6$new()', 1000),
          q_comp('Queue_r6_port_s3$new()', 1000),
          q_comp('Queue_r5$new()', 1000),
          columns=c('test', 'elapsed', 'replications', 'relative'))


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


