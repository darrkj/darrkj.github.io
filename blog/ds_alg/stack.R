Stack <- setRefClass(
  Class = "Stack",
  fields = list(name = "character", data = "list"),
  methods = list(
    size = function() {
      'Returns the number of items in the stack.'
      return(length(data))
    },
    #
    push = function(item) {
      'Inserts element on top of the stack.'
      data[[size()+1]] <<- item
    },
    #
    pop = function() {
      'Removes and returns the top element of the stack (or raises error if stack is empty).'
      if (size() == 0) stop("queue is empty!")
      value <- data[[size()]]
      data[[size()]] <<- NULL
      value
    },
    #
    poll = function() {
      'Removes and returns top of stack (or NULL if stack is empty).'
      if (size() == 0) return(NULL)
      else pop()
    },
    #
    top = function() {
      'Returns (but does not remove) specified positions in stack (or NULL if any one of them is not available).'
      data[[size()]]
    },
    nexttop = function() {
      'Returns (but does not remove) specified positions in stack (or NULL if any one of them is not available).'
      data[[size() - 1]]
    },
    #
    peek = function(pos = c(1)) {
      'Returns (but does not remove) specified positions in stack (or NULL if any one of them is not available).'
      if (size() < max(pos)) return(NULL)
      #
      if (length(pos) == 1) return(data[[pos]])
      else return(data[pos])
    },
    initialize = function(...) {
      callSuper(...)
      .self
    }
  )
)
