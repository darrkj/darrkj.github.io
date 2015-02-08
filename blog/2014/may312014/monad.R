#https://blog.jcoglan.com/2011/03/06/monad-syntax-for-javascript/
#https://blog.jcoglan.com/2011/03/05/translation-from-haskell-to-javascript-of-selected-portions-of-the-best-introduction-to-monads-ive-ever-read/
  
sine <- function(x) sin(x)

cube <- function(x) x*x*x

sineCubed <- function(x) mcube(msine(x))

compose <- function(f, g) {
  function(x) f(g(x))
}

compose(sine, cube)(3)


sine <- function(x) {
  list(sin(x), 'sine was called')
}


cube <- function(x) {
  list(x*x*x, 'cube was called')
}


bind <- function(f) {
  function(tuple) {
    x <- unlist(tuple[[1]])
    s <- tuple[2]
    fx <- f(x)
    y <- unlist(fx[1])
    t <- fx[2]
    list(y, paste(s, t, '.'))
  }
}


f <- compose(bind(sine), bind(cube))
f(list(3, ''))



unit <- function(x) list(x, '')

f(unit(3))
compose(f, unit)

lift <- function(f) compose(unit, f)

roundDebug <- lift(round)

f <- compose(bind(roundDebug), bind(sine))

f(unit(3))

