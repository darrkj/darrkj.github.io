



s <- .0003
ns <- .9997

person <- c(1, 0)

table <- rbind(c(.9985, .0015, .9997, .0003),
               c(.9994, .0006, .9997, .0003))

for(i in 1:nrow(table)) {
  
  
  nEs  <- table[i, 1]
  Es   <- table[i, 2]
  nEns <- table[i, 3]
  Ens  <- table[i, 4]
  
  event <- table[i, 5]
  
  if(event %in% c(1, 'M')) {
    new_s <- (s * Es) / ((s * Es) + (ns * Ens)) 
    new_ns <- (ns * Ens) / ((s * Es) + (ns * Ens)) 
  } else {
    new_s <- (s * nEs) /  ((s * nEs) + (ns * nEns)) 
    new_ns <- (ns * nEns) / ((s * nEs) + (ns * nEns)) 
  }
  
  s <- new_s
  ns <- new_ns
  
  print(c(s, ns))
}

