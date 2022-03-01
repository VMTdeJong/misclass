#' Misclassification model for clustered binary data
#' 
#' Misclassification model for clustered individual participant data with binary 
#' outcomes (endpoints), continuous or binary covariates and a binary exposure
#' (or intervention) that is possibly misclassified in some studies. 
#' 
#' @param x Gold standard measurement of the binary exposure, possibly missing.
#' @param s Surrogate measurement of the binary exposure. Potentially 
#'   misclassified.
#' @param y Binary outcome y. Possibly missing.
#' @param z Matrix or a single vector of covariates. If a matrix, each column
#'   should be one covariate, each row an observation/participant/patient.
#' @param j Indicator variable for the cluster (country, center, data set) the
#'   observation belongs to. If missing, all samples are assumed to belong to 
#'   the same cluster.
#' @param n.chains Number of MCMC chains
#' @param adapt Number of iterations for the sampler to adapt before running the 
#'   warmup and final analysis.
#' @param warmup Number of iterations for the sampler to find the posterior, 
#'   before running the final analysis (also referred to as burnin). 
#' @param sample Number of iterations for final analysis.
#' @param thin thinning interval. Values > 1 can be used to discard samples to 
#'   reduce autocorrelation. 
#' @param psrf.max maximum value for the psrf and mpsrf, to automatically 
#'   to flag non-convergence quantified by \code{gelman.diag} from the 
#'   \code{coda} package..
#' @param monitor.re Should the estimated random effects per cluster be 
#' monitored?
#' @param monitor Optional vector for setting monitors. Defaults to all fixed 
#'   effects, and standard deviations for random effects/intercepts. Include 
#'   include all random by setting \code{monitor.re} to `TRUE`.
#' @param inits Optional initial values. If left \code{NULL} separate initial 
#'   values are generated for each chain, using \code{inits.options}
#' @param inits.options List of: \code{coef.init.limit} For coefficients, 
#'   initial values are generated between coef.init.limit and coef.init.limit; 
#' \code{tau2.init.min} Minimum for initial values of tau2 heterogeneity 
#'   parameters; and \code{tau2.init.max} Maximum for initial values of tau2 
#'   heterogeneity 
#' @template model_arg
#' @template prior_arg
#' @param model Optional JAGS model. Overrides \code{measurement}, 
#' \code{exposure}, \code{outcome}, and \code{priors},
#' @param ... Optional parameters for \code{run.jags} from the \code{runjags}
#'   package.
#' 
#' @details This function (and the rest of the package) is intended to make 
#'   using a misclassification model easier. Therefore, it is not entirely 
#'   comprehensive.  However, the package is designed to be flexible, so that 
#'   you can use partial outputs (the JAGS model, monitors and inits), if you 
#'   would like to tweak the JAGS model. 
#' 
#'   For the coefficients and random intercepts/effects normal priors are used. 
#'   For the variances of the heterogeneity across clusters, half-normal priors 
#'   are used.
#' @import runjags
#' @import coda
#' @export
misclass <- function(
  
  # Data
  x,
  s,
  y,
  z,
  j,
  
  # MCMC options
  n.chains = 2,
  adapt  = 1000,
  warmup = 5000,
  sample = 10000,
  thin   = 1,
  psrf.max = 1.05,
  monitor.re = FALSE,
  monitor    = NULL,
  
  # Initial values for MCMC
  inits = NULL,
  inits.options = NULL,
  
  # Model parameters
  measurement = NULL,
  exposure    = NULL,
  outcome     = NULL,
  priors      = NULL,
  model       = NULL,

  ...) {
  
  z <- as.matrix(z) 
  n.z <- ncol(z)
  if (n.z < 1) 
    stop(paste0("z must contain at least one variable. z contained ", n.z, " variables."))
  
  if (missing(j)) J <- 0 else J <- length(unique(j))
  N <- length(x)
  
  d <- list(N = length(x),
            s = s,
            x = x,
            y = y,
            z = z,
            V = n.z)
  
  measurement <- measurement.options(measurement)
  exposure <- exposure.options(exposure)
  outcome <- outcome.options(outcome)
  priors <- priors.options(priors)
  
  if (is.null(monitor))
    monitor <- make.monitors(measurement = measurement,
                     exposure = exposure,
                     outcome = outcome,
                     monitor.re = monitor.re)
  
  if (is.null(inits)) {
    inits.options <- inits.options(inits.options)
    inits <- make.inits(J = J, 
                        n.z = n.z,                           
                        n.chains = n.chains, 
                        monitor = monitor,  
                        coef.init.limit = inits.options$coef.init.limit,
                        tau2.init.min = inits.options$tau2.init.min,
                        tau2.init.max = inits.options$tau2.init.max)
  }

  if (is.null(model))
    model <- make.model(measurement = measurement,
                        exposure = exposure,
                        outcome = outcome,
                        priors = priors)
  
  # If any random effect/intercept add j and J
  if (measurement$i || exposure$i || outcome$i || outcome$s)  {
    stoptext <- "At least 2 clusters with observed values for x are necessary for running a 
  random intercepts/effects model (but preferably a lot more). The number of 
  clusters with observed values for x was " 
    J.gold <- length(unique(j[!is.na(x)]))
    if (J.gold < 2) stop(paste0(stoptext, J.gold, "."))
    
    d$j <- j
    d$J <- J
  }

  jags <- run.jags(model, 
                   monitor = monitor, 
                   data = d, 
                   inits = inits, 
                   adapt = adapt,
                   burnin = warmup, 
                   sample = sample,
                   ...)
  
  converged <- convergence(jags, psrf.max)
  
  out <- list(jags = jags,
              measurement = measurement,
              exposure = exposure,
              outcome = outcome,
              priors  = priors,
              converged = converged,
              inits.options = inits.options,
              jags.options = list(adapt = adapt,
                                  warmup = warmup,
                                  sample = sample,
                                  thin = thin,
                                  monitor = monitor))
  
  class(out) <- c("misclass", class(out))
  out
}

#' Extend the mcmc chain of a misclass object
#' 
#' When the \code{misclass} object contains too few samples, this function can 
#'   extend the MCMC chain. By default, it doubles the total amount of samples 
#'   used for inference, but does not perform another warmup or adaptation.
#' 
#' @param object a \code{misclass} object
#' @param sample Number of samples by which the mcmc chain(s) should be extended,
#' defaults to same as original object (i.e. the total of samples is doubled).
#' @param warmup Number of additional warmup iterations for the extension of the 
#' chain, defaults to 0.
#' @param adapt Number of additional adaptation iterations of the mcmc sampler, 
#' defaults to 0.
#' 
#' @details see \code{extend.jags} from the {runjags} package for details.
#' 
#' @export
extend.misclass <- function(object, sample = object$jags.options$sample, warmup = 0, adapt = 0) {
  object$jags <- extend.jags(object$jags, sample = sample, burnin = warmup, adapt = adapt)
  
  object
}

convergence <- function(object, psrf.max = 1.05, ...) {
  if (inherits(object, "misclass")) 
    object <- object$jags
  psrf  <- gelman.diag(object)$psrf["beta.x.0", 1]
  mpsrf <- gelman.diag(object)$mpsrf
  cat(paste0("Beta.x.0 potential scale reduction factor point estimate: ", round(psrf, 3), "\n"))
  cat(paste0("Multivariate potential scale reduction factor point estimate: ", round(mpsrf, 3), "\n"))
  
  if ((psrf > psrf.max || mpsrf > psrf.max)) {
    cat(paste0("It is recommended to extend the mcmc chains, using extend.misclass().\n"))
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' Print a misclass object
#' 
#' @param x A misclass object
#' @param digits Number of digits used for rounding
#' @param vars Names of variables to print. Ignored for models with few variables.
#' Defaults to all variables (which may be undesirable for models with many
#' monitored variables).
#' @param ... Arguments passed to \code{add.summary} from the \code{runjags} 
#'   package, for models with many monitors. 
#' @export
print.misclass <- function(x, digits = 3, vars = NA, ...) {
  j <- x$jags
  
  if (!j$summary.available) j <- add.summary(j, vars, ...)
  
  if (x$measurement$stratified.differential) {
    diff <- "Stratified differential" 
  } else if (x$measurement$additive.differential) {
    diff <- "Differential"
  } else diff <- "Non-differential"
  
  cat(paste0(diff, " misclassification model using JAGS. Summary statistics from ", 
             length(j$mcmc) * j$sample, " samples (chains = ", length(j$mcmc), 
             ", adapt+burnin = ", j$summary$start-1, ").\n\n"))
  print(x$jags.options$monitor, models = FALSE, params = FALSE)
  cat("\n")
  
  s <- summary(x$jags)
  sr <- round(s, digits)
  measurement <- mgrep(c("lambda", "phi", "theta", "eta", "psi", "omega"), rownames(s)) 
  exposure <- grep(c("gamma"), rownames(s))
  outcome <- grep(c("beta"), rownames(s))
  
  # Remove beta and double eta from measurement:
  measurement <- unique(measurement[!measurement %in% outcome]) 
  
  cat(paste0(diff, " measurement submodel:\n"))
  print(sr[measurement, ])
  cat("\n")
  cat("Exposure submodel:\n")
  print(sr[exposure, ])
  cat("\n")
  cat("Outcome submodel:\n")
  print(sr[outcome, ])
  cat("\n")
  
  invisible(x)
}

#' @export
summary.misclass <- function(object, digits = 3, ...) {
  if (object$measurement$stratified.differential) {
    diff <- "Stratified differential" 
  } else   if (object$measurement$additive.differential) {
    diff <- "Differential"
  } else diff <- "Non-differential"
  
  cat(paste0(diff, " misclassification model using JAGS.\n"))
  print(round(summary(object$jags), digits = digits))
  print(object$jags.options$monitor, params = FALSE)
  invisible(object,...)
}

#' Extract model coefficients
#' 
#' Extract model coefficients from a misclass object. 
#' 
#' @param object a misclass object
#' @param parm parameter for which coefficients are required. Defaults to all
#' @param method Method for estimating the center of the posterior distribution.
#'   Defaults to "Median", the alternative is "Mean"
#' @param ... ignored
#' @export
coef.misclass <- function(object, parm, method = "Median", ...) 
  object$jags$summaries[parm, method]

#' Credible intervals 
#' 
#' Credible intervals for a misclass object.
#' 
#' @param object a misclass object
#' @param parm vector for names of parameters for which credibile intervals are
#' requested. Defaults to all.
#' @param level the credible interval required
#' @param ... ignored
#' 
#' @details Unlike the generic function \code{confint}, this function produces
#'   credible intervals, not confidence intervals. The intervals are estimated
#'   using the \code{quantile} of the mcmc samples.
#' 
#' @export
confint.misclass <- function(object, parm, level = 0.95, ...) {
  probs <- c((1-level) / 2, 1 -(1-level) / 2)
  t(apply(Reduce(rbind, object$jags$mcmc)[ , parm, drop = FALSE], 2, function(x) quantile(x, probs = probs)))
}
      


