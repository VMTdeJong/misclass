# The idea of this function is to generate a jags file according to user 
# specifications. This allows all permutations of measurement, exposure and 
# outcome models.

#' Make a misclassification model for JAGS
#' 
#' Make a misclassification model that runjags/JAGS can read. The model can have 
#'   covariate effects and random intercepts for all submodels and also random 
#'   slopes for the outcome submodel. To view the model, save it to a file and 
#'   open it (see examples).
#' 
#' @template model_arg
#' @template prior_arg
#' 
#' @details \code{stratified.differential} Allows for completely 
#'   different submodels for different values of y (and x), and thereby 
#'   overrides \code{additive.differential}.
#'   
#'   The output can be saved as .R or .txt file, making 
#'   it easier to read, and allowing it to be run with rjags as well. See the
#'   examples.
#' 
#' @returns a JAGS model, as a character string that \code{run.jags} from the
#'   \code{runjags} package can read.
#' @examples 
#' # Default: Nondifferential error, covariate effects but no random effects.
#' m <- make.model() 
#' 
#' # Random intercepts. Note, not the whole list needs to be entered, only the 
#' # arguments that are to be changed. 
#' m <- make.model(measurement = list(intercept.random = TRUE), 
#' exposure = list(intercept.random = TRUE), 
#' outcome = list(intercept.random = TRUE))
#' 
#' # Random effects. Note that options of the lists can be abbreviated to the 
#' # first letter.
#' m <- make.model(measurement = list(i = TRUE), 
#' exposure = list(i = TRUE), 
#' outcome = list(i = TRUE, e = TRUE))
#'                
#' # Differential misclassification
#' m <- make.model(measurement = list(s = TRUE))
#' m <- make.model(measurement = list(a = TRUE))
#' 
#' \dontrun{
#' write(m, file = "jags_script.R")
#' file.edit("jags_script.R")}
#' @export
make.model <- function(
  measurement = list(stratified.differential = FALSE, # May be abbreviated to s
                     additive.differential   = FALSE, # May be abbreviated to a
                     covariate               = TRUE,  # May be abbreviated to c
                     intercept.random        = FALSE),# May be abbreviated to i
  exposure    = list(covariate               = TRUE,  # May be abbreviated to c
                     intercept.random        = FALSE),# May be abbreviated to i
  outcome     = list(covariate               = TRUE,  # May be abbreviated to c
                     intercept.random        = FALSE, # May be abbreviated to i
                     effects.random          = FALSE),# May be abbreviated to e
  priors      = list(coef.precision          = .1,    # May be abbreviated to c
                     heterogeneity.precision = 5)){   # May be abbreviated to h
  measurement <- measurement.options(measurement)
  exposure <- exposure.options(exposure)
  outcome <- outcome.options(outcome)
  priors <- priors.options(priors)
  
  # Explanation for user 
  out <- helptext()
  
  # Model
  out <- paste0(out, start.model())
  out <- paste0(out, measurement.model(measurement))
  out <- paste0(out, exposure.model(exposure))
  out <- paste0(out, outcome.model(outcome))

  # Monitors
  out <- paste0(out, monitors())
  
  # Priors
  any.ir <- any(measurement$i, exposure$i, outcome$i)
  out <- paste0(out, hyper.priors(priors, any.ir)) 
  out <- paste0(out, measurement.priors(measurement))
  out <- paste0(out, exposure.priors(exposure))
  out <- paste0(out, outcome.priors(outcome))
  
  # End
  out <- paste0(out, end.model())
  out
}

helptext <- function() {
  "##### Note: this is a BUGS / JAGS file. When working in RStudio, and you
    # would like to save it, save it as .R to get fancy colors, making it easier 
    # to read.

##### Data
   ## Description
    # v = covariate, v = 1,...,V, where 
    # V = is number of covariates z.
    # z = covariate, not missing, no error assumed.
    # i = individual, i = 1,.., n
    # n = sample size
    # y = outcome, binary, no error assumed. May be missing for some i.
    # x = exposure of interest, possibly missing for some i.
    # s = surrogate observation of x, i.e. with error. May be missing for some i.
    # j = study/cluster, j = 1,...,J
    # J = number of studies/clusters.
    # Note that values of x, y, or s may be missing and are automatically imputed.
                  
  ### Expected data format:
    # var N, J, V, x[N], y[N], s[N], j[N], z[N, V];
                  
##### The model
  ### Coefficients for nondifferential models and additive differential models
    # beta   = coefficient in outcome model
    # gamma  = coefficient in exposure model
    # lambda = coefficient in measurement model, if x.ij = 1
    # phi    = coefficient in measurement model, if x.ij = 0
    # tau    = precision of random effect
  
  ### Coefficients for stratified differential models
    # beta   = coefficient in outcome model
    # gamma  = coefficient in exposure model
    # eta    = coefficient in measurement model, if x.ij = 1 and y.ij = 1 
    # theta  = coefficient in measurement model, if x.ij = 1 and y.ij = 0
    # psi    = coefficient in measurement model, if x.ij = 0 and y.ij = 1 
    # omega  = coefficient in measurement model, if x.ij = 0 and y.ij = 0
    # tau    = precision of random effect
                  
  ### First subscript
    # .0 = intercept.
    # .x = coefficient for x.
    # .z = coefficient for z.
                  
  ### Second subscript
    # .0 = summary effect / fixed effect.
    # .j = random effect.
                  
  ### Tau's third subscript:
    # tau has one additional subscript (the first) to indicate the coefficient 
    # it is the variance of.
    # tau is equal to 1/sqrt(prec)
    # T(0,) truncates the distribution at zero, so that it is non-negative
    # The sampler assumes this is a-priori known to be non-negative"
}

start.model <- function() {
"

model {

  ### The model
  for (i in 1:N) {"
}

end.model <- function() {
"
}
  
"
}
  

measurement.model <- function(measurement = list(stratified.differential = F,
                                                 additive.differential   = F,
                                                 covariate               = F,
                                                 intercept.random        = F),
                              ...) {
  out <- "  
  
    ## 1. Measurement model  
    ##    Stratified by x."
  
  if (measurement$s) {
    out <- paste0(out, "
    #     And stratified by y to account for differential error.")
    
    if (measurement$i) {
      eta.0.j   <- " + eta.0.j[j[i]]  "
      theta.0.j <- " + theta.0.j[j[i]]"
      psi.0.j   <- " + psi.0.j[j[i]]  "
      omega.0.j <- " + omega.0.j[j[i]]"
    } else {
      eta.0.j   <- ""
      theta.0.j <- ""
      psi.0.j   <- ""
      omega.0.j <- ""
    }
    
    if (measurement$c) {
      eta.z.0   <- " + inprod(eta.z.0,   z[i, ])"
      theta.z.0 <- " + inprod(theta.z.0, z[i, ])"
      psi.z.0   <- " + inprod(psi.z.0,   z[i, ])"
      omega.z.0 <- " + inprod(omega.z.0, z[i, ])"
    } else {
      eta.z.0   <- ""
      theta.z.0 <- ""
      psi.z.0   <- ""
      omega.z.0 <- "" 
    }
    
    out <- paste0(out, "    
    s[i] ~ dbern(s.p[i])
    logit(s.p[i]) <- ifelse(x[i] == 1,
                            ifelse(y[i] == 1,
                                   eta.0.0  ", eta.0.j,    eta.z.0,   ", ", "
                                   theta.0.0", theta.0.j,  theta.z.0, "), ", "
                            ifelse(y[i] == 1,
                                   psi.0.0  ", psi.0.j,    psi.z.0,   ", ", "
                                   omega.0.0", omega.0.j,  omega.z.0, "))")
  } else {
    
    if (measurement$i) {
      lambda.0.j <- " + lambda.0.j[j[i]]"         
      phi.0.j    <- " + phi.0.j[j[i]]"            
    } else {
      lambda.0.j <- ""
      phi.0.j    <- ""
    }
    
    if (measurement$c) {
      lambda.z.0 <- " + inprod(lambda.z.0, z[i, ])" 
      phi.z.0    <- " + inprod(phi.z.0, z[i, ])" 
    } else {
      lambda.z.0 <- ""
      phi.z.0    <- ""
    }
    
    if (measurement$a) {
      lambda.y.0 <- " + lambda.y.0[j[i]]"
      phi.y.0    <- " + phi.y.0[j[i]]"
    } else {
      lambda.y.0 <- ""
      phi.y.0    <- ""
    }
    
    out <- paste0(out, "    
    s[i] ~ dbern(s.p[i])
    logit(s.p[i]) <- ifelse(x[i] == 1,
                            lambda.0.0", lambda.0.j,  lambda.z.0, lambda.y.0, ", ", "
                            phi.0.0"   , phi.0.j,     phi.z.0,    phi.y.0,    ")")
  }
  
  out
}

exposure.model <- function(exposure = list(covariate        = F,
                                           intercept.random = F),
                                    ...) {
  out <- "
  
    ## 2. Exposure model
    ##    Note that x may be missing, in those cases it is latent and imputed."
  
  gamma.0.j <- if (exposure$i) " + gamma.0.j[j[i]]" else ""
  gamma.z.0 <- if (exposure$c) " + inprod(gamma.z.0, z[i, ])" else ""
    
  out <- paste0(out, "
    x[i] ~ dbern(x.p[i])
    logit(x.p[i]) <- gamma.0.0", gamma.0.j, gamma.z.0)
  
  out
}

outcome.model <- function(outcome = list(intercept.random = F,
                                         effects.random   = F,
                                         covariate        = F),
                                   ...) {
  out <- "
  
    ## 3. Outcome model"
  
  beta.0.j <- if (outcome$i) " + beta.0.j[j[i]]"           else ""
  beta.x.j <- if (outcome$s) " + beta.x.j[j[i]] * x[i]"    else ""
  beta.z.0 <- if (outcome$c) " + inprod(beta.z.0, z[i, ])" else ""
  
  
  out <- paste0(out, "
    y[i] ~ dbern(y.p[i])
    logit(y.p[i]) <- beta.0.0 + beta.x.0 * x[i]", beta.0.j, beta.x.j, beta.z.0,"
  }")
  
  out
}

monitors <- function() {
"

  ### Monitoring
  for (i in 1:N) {
    x.pos[i] <- ifelse(x[i] == 1, s[i], 0)
    x.neg[i] <- ifelse(x[i] == 0, s[i], 0)
    
    y.pos[i] <- ifelse(y[i] == 1, x[i], 0)
    y.neg[i] <- ifelse(y[i] == 0, x[i], 0)
  }
  
  min.sum <- .0001 # if x.sum is zero, we would otherwise be dividing by zero.
  
  x.sum <- sum(x)
  x.sum2 <- ifelse(x.sum < min.sum, min.sum, x.sum)
  sens.x.s <- sum(x.pos) / x.sum2
  spec.x.s <- 1 - sum(x.neg) / (N - x.sum2)
  
  y.sum <- sum(y)
  y.sum2 <- ifelse(y.sum < min.sum, min.sum, y.sum)
  sens.y.x <- sum(y.pos) / y.sum2
  spec.y.x <- 1 - sum(y.neg) / (N - y.sum2)"
}

hyper.priors <- function(priors = list(coef.precision = .1,
                                       heterogeneity.precision = .001),
                         intercept.random) {
  out <- paste0("  
  
  ###   Priors
  ## 0. Parameter values
  # Precision for the coefficients beta, eta, theta, etc
  coef.precision <- ", priors$coef.precision)
  
  if (intercept.random) 
    out <- paste0(out, "
    
  # Precision for the heterogeneity parameter tau^2
  heterogeneity.precision <- ", priors$heterogeneity.precision)
  
  out
}

measurement.priors <- function(measurement = list(additive.differential   = F,
                                                  stratified.differential = F,
                                                  covariate        = F,
                                                  intercept.random = F), 
                                        ...) {
  
  if (measurement$s) {
    out <- "  ## 1. Measurement model
  # Intercepts
  # x = 1, y = 1
  eta.0.0    ~ dnorm(0.0, coef.precision)
  
  # x = 1, y = 0
  theta.0.0  ~ dnorm(0.0, coef.precision) 
    
  # x = 0, y = 1
  psi.0.0    ~ dnorm(0.0, coef.precision) 
    
  # x = 0, y = 0
  omega.0.0  ~ dnorm(0.0, coef.precision)"
    
    if (measurement$i) {
      out <- paste0(out, "

  # Random intercepts      
  # x = 1, y = 1
  for (jj in 1:J) {
    eta.0.j[jj] ~ dnorm(0.0, prec.eta.0.j)
  }
  prec.eta.0.j <- 1/tau2.eta.0.j
  tau.eta.0.j <- sqrt(tau2.eta.0.j)
  tau2.eta.0.j ~ dnorm(0.0, heterogeneity.precision)T(0,)
      
  # x = 0, y = 1
  for (jj in 1:J) {
    theta.0.j[jj] ~ dnorm(0.0, prec.theta.0.j)
  }
  prec.theta.0.j <- 1/tau2.theta.0.j
  tau.theta.0.j <- sqrt(tau2.theta.0.j)
  tau2.theta.0.j ~ dnorm(0.0, heterogeneity.precision)T(0,)
                    
  # x = 1, y = 0
  for (jj in 1:J) {
    psi.0.j[jj] ~ dnorm(0.0, prec.psi.0.j)
  }
  prec.psi.0.j <- 1/tau2.psi.0.j
  tau.psi.0.j <- sqrt(tau2.psi.0.j)
  tau2.psi.0.j ~ dnorm(0.0, heterogeneity.precision)T(0,)
      
  # x = 0, y = 0
  for (jj in 1:J) {
    omega.0.j[jj] ~ dnorm(0.0, prec.omega.0.j)
  }
  prec.omega.0.j <- 1/tau2.omega.0.j
  tau.omega.0.j <- sqrt(tau2.omega.0.j)
  tau2.omega.0.j ~ dnorm(0.0, heterogeneity.precision)T(0,)")
    }
    
    if (measurement$c) {
      out <- paste0(out, "
  
  # Covariate    
  # x = 1, y = 1
  for (v in 1:V) {
    eta.z.0[v] ~ dnorm(0.0, coef.precision)
  }
      
  # x = 1, y = 0
  for (v in 1:V) {
    theta.z.0[v] ~ dnorm(0.0, coef.precision)
  }
  
  # x = 0, y = 1
  for (v in 1:V) {
    psi.z.0[v] ~ dnorm(0.0, coef.precision)
  }
      
  # x = 0, y = 0
  for (v in 1:V) {
    omega.z.0[v] ~ dnorm(0.0, coef.precision)
  }")
    }
    
  } else { # If not stratified
    out <- "  
    
  ## 1. Measurement model
  # Intercepts
  # x = 1
  lambda.0.0  ~ dnorm(0.0, coef.precision)
  
  # x = 0
  phi.0.0  ~ dnorm(0.0, coef.precision) "
    
    if (measurement$i) {
      out <- paste0(out, "

  # Random intercepts      
  # x = 1
  for (jj in 1:J) {
    lambda.0.j[jj] ~ dnorm(0.0, prec.lambda.0.j)
  }
  prec.lambda.0.j <- 1/tau2.lambda.0.j
  tau.lambda.0.j <- sqrt(tau2.lambda.0.j)
  tau2.lambda.0.j ~ dnorm(0.0, heterogeneity.precision)T(0,)
      
  # x = 0
  for (jj in 1:J) {
    phi.0.j[jj] ~ dnorm(0.0, prec.phi.0.j)
  }
  prec.phi.0.j <- 1/tau2.phi.0.j
  tau.phi.0.j <- sqrt(tau2.phi.0.j)
  tau2.phi.0.j ~ dnorm(0.0, heterogeneity.precision)T(0,)")
    }
    
    if (measurement$c) {
      out <- paste0(out, "
  
  # Covariate    
  # x = 1
  for (v in 1:V) {
    lambda.z.0[v] ~ dnorm(0.0, coef.precision)
  }
      
  # x = 0
  for (v in 1:V) {
    phi.z.0[v] ~ dnorm(0.0, coef.precision)
  }")
    }
    
    if (measurement$a) {
      out <- paste0(out, "
  # Differential error terms
  # x = 1
  lambda.y.0 ~ dnorm(0.0, coef.precision)
  
  # x = 0
  phi.y.0 ~ dnorm(0.0, coef.precision)")
    }
  }
  out
}

exposure.priors <- function(exposure = list(covariate        = F,
                                            intercept.random = F), 
                            ...) {
  out <- "  
  
  ## 2. Exposure model
  gamma.0.0  ~ dnorm(0.0, coef.precision)"
  if (exposure$i) {
    out <- paste0(out, "  
    
  # Random intercepts
  for (jj in 1:J) {
    gamma.0.j[jj] ~ dnorm(0.0, prec.gamma.0.j)
  }
  prec.gamma.0.j <- 1/tau2.gamma.0.j
  tau.gamma.0.j <- sqrt(tau2.gamma.0.j)
  tau2.gamma.0.j ~ dnorm(0.0, heterogeneity.precision)T(0,)")
  }
  
  if (exposure$c) {
    out <- paste0(out, "  
    
  # Covariate  
  for (v in 1:V) {
    gamma.z.0[v] ~ dnorm(0.0, coef.precision)
  }")
  }
  
  out
}

outcome.priors <- function(outcome = list(intercept.random = F,
                                          effects.random   = F,
                                          covariate        = F), 
                           ...) {
  out <- "  
  
  ## 3. Outcome model
  beta.0.0  ~ dnorm(0.0, coef.precision)
  beta.x.0  ~ dnorm(0.0, coef.precision)"
  
  if (outcome$i) {
    out <- paste0(out, "    
    
  # Random intercepts
  for (jj in 1:J) {
    beta.0.j[jj] ~ dnorm(0.0, prec.beta.0.j)
  }
  prec.beta.0.j <- 1/tau2.beta.0.j
  tau.beta.0.j <- sqrt(tau2.beta.0.j)
  tau2.beta.0.j ~ dnorm(0.0, heterogeneity.precision)T(0,)")
  }
  
  if (outcome$s) {
    out <- paste0(out, "  
    
  # Random effects
  for (jj in 1:J) {
    beta.x.j[jj] ~ dnorm(0.0, prec.beta.x.j)
  }
  prec.beta.x.j <- 1/tau2.beta.x.j
  tau.beta.x.j <- sqrt(tau2.beta.x.j)
  tau2.beta.x.j ~ dnorm(0.0, heterogeneity.precision)T(0,)")
  }
  
  if (outcome$c) {
    out <- paste0(out, "  
  
  # Covariate
  for (v in 1:V) {
    beta.z.0[v] ~ dnorm(0.0, coef.precision)
  }")
  }

  out
}

