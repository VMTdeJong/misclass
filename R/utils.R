# Inverse logit
inv.logit <- function(x) 
  1 / (1 + exp(-x))

# Multiple grep: searches for multiple patterns
mgrep <- function(patterns, x, ignore.case = TRUE) 
  unlist(mapply(grep, patterns, MoreArgs = list(x = x, ignore.case = ignore.case), SIMPLIFY = FALSE))

# The following functions update option lists. They allow for partial matches
# for the arguments.
inits.options <- function(x) {
  out <- list(coef.init.limit = 2,
              tau2.init.min = .0001,
              tau2.init.max = 2)
  
  if (!is.null(x$c))
    out$coef.init.limit <- x$c
  
  if (!is.null(x$tau2.init.min))
    out$tau2.init.min <- x$tau2.init.min
  
  if (!is.null(x$tau2.init.max))
    out$tau2.init.max <- x$tau2.init.max
    
  out  
}

measurement.options <- function(x) {
  out <- list(stratified.differential = FALSE,  
              additive.differential   = FALSE,
              covariate               = TRUE,       
              intercept.random        = FALSE)
  
  if (!is.null(x$s))
    out$stratified.differential <- x$s
  
  if (!is.null(x$a))
    out$additive.differential <- x$a
  
  if (!is.null(x$c))
    out$covariate <- x$c
  
  if (!is.null(x$i))
    out$intercept.random <- x$i
  
  out
}

exposure.options <- function(x) {
  out <- list(covariate        = TRUE,       
              intercept.random = FALSE) 
  
  if (!is.null(x$c))
    out$covariate <- x$c
  
  if (!is.null(x$i))
    out$intercept.random <- x$i
  
  out
}

outcome.options <- function(x) {
  out <- list(covariate        = TRUE,
              intercept.random = FALSE,       
              slope.random     = FALSE)
  
  if (!is.null(x$c))
    out$covariate <- x$c
  
  if (!is.null(x$i))
    out$intercept.random <- x$i
  
  if (!is.null(x$s))
    out$slope.random <- x$s
  
  out
}

priors.options <- function(x) {
  out <- list(coef.precision = .1,
              heterogeneity.precision  = 5)
  
  if (!is.null(x$c))
    out$coef.precision <- x$c
  
  if (!is.null(x$h))
    out$heterogeneity.precision <- x$h
  
  out
}
  
