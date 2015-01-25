# Finite State Automata

fsa <- function(s) {
  
  state <- 'q1'
  
  for (i in unlist(strsplit(s, ''))) {
    if (i == '1' & state == 'q1') state <- 'q2'
    else if (i == '0' & state == 'q2') state <- 'q3'
    else if (state == 'q3') state <- 'q2'
  }
  
  if (state == 'q2') 'accept' else 'reject'  
}


s <- '010101001010101010010'

fsa(s)
fsa('1101')
