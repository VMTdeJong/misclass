#' Make parameters for monitoring 
#' 
#' Make parameters for monitoring the JAGS object in misclass. 
#' 
#' @template model_arg
#' @param monitor.re Should the estimated random effects per clusters be
#'   monitored? Defaults to FALSE, as this produces a lot of output when many 
#'   clusters are present.
#' 
#' @return  Returns a List of parameter names
#' 
#' @examples 
#' # Nondifferential model, with no random effects or random intercepts.
#' m <- make.monitors()
#'
#'# Differential misclassification model with random intercepts, random effects,
#'# fixed covariate effects 
#' m <- make.monitors(measurement = list(stratified.differential = TRUE,
#'                                       additive.differential   = FALSE,
#'                                       covariate               = TRUE,
#'                                       intercept.random        = TRUE),
#'                    exposure    = list(covariate               = TRUE,
#'                                       intercept.random        = TRUE),
#'                    outcome     = list(intercept.random        = TRUE,
#'                                       slope.random            = TRUE,
#'                                       covariate               = TRUE))
#'# Random intercepts (abbreviated to i), and covariate effects (which default 
#'# to TRUE) for all three submodel.
#' m <- make.monitors(measurement = list(i = TRUE),
#'                    exposure    = list(i = TRUE),
#'                    outcome     = list(i = TRUE))
#'
#' @export
make.monitors <- function(
  measurement = list(stratified.differential = FALSE,  # May be abbreviated to s
                     additive.differential   = FALSE,  # May be abbreviated to a
                     covariate               = TRUE,   # May be abbreviated to c
                     intercept.random        = FALSE), # May be abbreviated to i
  exposure    = list(covariate               = TRUE,   # May be abbreviated to c
                     intercept.random        = FALSE), # May be abbreviated to i
  outcome     = list(intercept.random        = FALSE,  # May be abbreviated to i
                     slope.random            = FALSE,  # May be abbreviated to s
                     covariate               = TRUE),  # May be abbreviated to c
  monitor.re = FALSE) {
  
  measurement <- measurement.options(measurement)
  exposure <- exposure.options(exposure)
  outcome <- outcome.options(outcome)
  
  out <- c()
  
  # 1. Measurement model parameters
  if (measurement$s) {
    if (measurement$a) 
      warning("additive.differential and stratified.differential were both TRUE. 
              additive.differential was ignored.")
    
    out <- c(out, "eta.0.0", "theta.0.0", "psi.0.0", "omega.0.0")
    if (measurement$c)               out <- c(out, "eta.z.0", "theta.z.0", "psi.z.0", "omega.z.0")
    if (measurement$i)               out <- c(out, "tau.eta.0.j", "tau.theta.0.j", "tau.psi.0.j", "tau.omega.0.j")
    if (measurement$i && monitor.re) out <- c(out, "eta.0.j", "theta.0.j", "psi.0.j", "omega.0.j")
    
  } else {
    out <- c(out, "lambda.0.0", "phi.0.0")
    if (measurement$c)               out <- c(out, "lambda.z.0", "phi.z.0")
    if (measurement$a)      out <- c(out, "lambda.y.0", "phi.y.0")
    if (measurement$i)               out <- c(out, "tau.lambda.0.j", "tau.phi.0.j")
    if (measurement$i && monitor.re) out <- c(out, "lambda.0.j", "phi.0.j")         
  }
  
  # 2. Exposure model
  out <- c(out, "gamma.0.0")
  if (exposure$c)               out <- c(out, "gamma.z.0")
  if (exposure$i)               out <- c(out, "tau.gamma.0.j")
  if (exposure$i && monitor.re) out <- c(out, "gamma.0.j")
  
  # 3. Outcome model
  out <- c(out, "beta.0.0", "beta.x.0")
  if (outcome$c)               out <- c(out, "beta.z.0")
  if (outcome$i)               out <- c(out, "tau.beta.0.j")
  if (outcome$i && monitor.re) out <- c(out, "beta.0.j")
  if (outcome$s)               out <- c(out, "tau.beta.x.j") 
  if (outcome$s && monitor.re) out <- c(out, "beta.x.j") 
  
  class(out) <- c("monitor", class(out))
  out
}

#' @export
print.monitor <- function(x, params = TRUE, models = TRUE, suffix = TRUE, ...) {
  # List of parameters
  if (params) 
    cat(paste0(x, "\n"))
  
  # Greek letters to indicate model parameters
  if (models) {
    m <- if (length(grep("phi", x))) c("lambda", "phi") else c("eta", "theta", "psi", "omega")
    cat(paste0("The measurement model contains these parameters: ", 
               paste0(m, collapse = ", "), ".\n"))
    cat(paste0("The exposure model contains the gamma parameters.\n"))
    cat(paste0("The outcome model contains the beta parameters.\n"))
  }
  
  ## Suffixes
  if (suffix) {
    s1 <- c("The first suffix indicates the variable, where .0 = the intercept, .x = the exposure effect")
    if (length(grep(".z", x)))
      s1 <- c(s1, ", .z = the covariate effect(s)")
    s1 <- paste0(c(s1, ".\n"), collapse = "")
    
    s2 <- c("The second suffix indicates the level of the effect, where .0 = the mean effect")
    if (length(grep(".j", x)))
      s2 <- c(s2, ", .j = random effect per cluster")
    s2 <- paste0(c(s2, ".\n"), collapse = "")
    
    cat(paste0(s1, s2))
  }
}
