library(dplyr)


library(assertive)
# http://www.r-bloggers.com/be-assertive/

library(ensurer)
# https://github.com/smbache/ensurer




library(assertr)
# https://cran.r-project.org/web/packages/assertr/vignettes/assertr.html

mtcars %>%
  verify(mpg >= 15)

mtcars %>%
  assert(within_bounds(15,Inf), mpg) 

mtcars %>% verify(nrow(.) > 10)

# Custom predicate
mtcars$let <- sample(LETTERS, nrow(mtcars), replace = T)
mtcars$let[1] <- ''
not.empty.p <- function(x) if(x=="") return(FALSE)
mtcars %>% assert(not.empty.p, let)

mtcars %>%
  insist(within_n_sds(3), mpg) %>%
  group_by(cyl) %>%
  summarise(avg.mpg=mean(mpg))


library(assertive)
# http://www.r-bloggers.com/be-assertive/

library(ensurer)
# https://github.com/smbache/ensurer




library(assertthat)
# https://github.com/hadley/assertthat

is_odd <- function(x) {
  assert_that(is.numeric(x), length(x) == 1)
  x %% 2 == 1
}
assert_that(is_odd(2))
# Error: is_odd(x = 2) is not TRUE

on_failure(is_odd) <- function(call, env) {
  paste0(deparse(call$x), " is even")
}
assert_that(is_odd(2))
# Error: 2 is even

