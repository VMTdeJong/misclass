#' Make initial values for multiple chains
#' 
#' Initial values for multiple JAGS chains by the \link{misclass} function.
#' 
#' @param J Number of clusters
#' @param n.z Number of covariates (i.e. excluding the exposure)
#' @param n.chains Number of MCMC chains
#' @param monitor Names of parameters for which to generate initial values
#' @param coef.init.limit For coefficients, initial values are generated
#'   between -coef.init.limit and coef.init.limit.
#' @param tau2.init.min Minimum for initial values of tau2 heterogeneity
#' parameters.
#' @param tau2.init.max Maximum for initial values of tau2 heterogeneity
#' parameters.
#' 
#' @examples
#' make.inits(
#'   J = 5,
#'   n.z = 2,
#'   n.chains = 3,
#'   monitor = c("eta.0.0", "phi.y.0", "tau2.beta.x.j")
#' )
#' 
#' @returns  List of vectors with named initial values.
#' 
#' @export
make.inits <- function(J, 
                       n.z, 
                       n.chains, 
                       monitor, 
                       coef.init.limit = 2,
                       tau2.init.min = .0001,
                       tau2.init.max = 2) {
  mapply(make.inits.single.chain, 
         seq_len(n.chains), 
         MoreArgs = list(
           J = J,
           n.z = n.z,
           monitor = monitor,
           coef.init.limit = coef.init.limit,
           tau2.init.min = tau2.init.min,
           tau2.init.max = tau2.init.max),
         SIMPLIFY = FALSE
  )
}

#' @import stats
make.inits.single.chain <- function(...,
                                    J,
                                    n.z,
                                    monitor,
                                    coef.init.limit = 2,
                                    tau2.init.min = .0001,
                                    tau2.init.max = 2) {
  inits <- list(
    ### Coefficients
    ## Scalar
    eta.0.0    = runif(1, min = -coef.init.limit, max = coef.init.limit),
    theta.0.0  = runif(1, min = -coef.init.limit, max = coef.init.limit),
    psi.0.0    = runif(1, min = -coef.init.limit, max = coef.init.limit),
    omega.0.0  = runif(1, min = -coef.init.limit, max = coef.init.limit),
    lambda.0.0 = runif(1, min = -coef.init.limit, max = coef.init.limit),
    lambda.y.0 = runif(1, min = -coef.init.limit, max = coef.init.limit),
    phi.0.0    = runif(1, min = -coef.init.limit, max = coef.init.limit),
    phi.y.0    = runif(1, min = -coef.init.limit, max = coef.init.limit),
    gamma.0.0  = runif(1, min = -coef.init.limit, max = coef.init.limit),
    beta.0.0   = runif(1, min = -coef.init.limit, max = coef.init.limit),
    beta.x.0   = runif(1, min = -coef.init.limit, max = coef.init.limit),
    gamma.s.0  = runif(1, min = -coef.init.limit, max = coef.init.limit),
    
    ## Vectors
    eta.z.0    = runif(n.z, min = -coef.init.limit, max = coef.init.limit),
    theta.z.0  = runif(n.z, min = -coef.init.limit, max = coef.init.limit),
    psi.z.0    = runif(n.z, min = -coef.init.limit, max = coef.init.limit),
    omega.z.0  = runif(n.z, min = -coef.init.limit, max = coef.init.limit),
    lambda.z.0 = runif(n.z, min = -coef.init.limit, max = coef.init.limit),
    phi.z.0    = runif(n.z, min = -coef.init.limit, max = coef.init.limit),
    gamma.z.0  = runif(n.z, min = -coef.init.limit, max = coef.init.limit),
    beta.z.0   = runif(n.z, min = -coef.init.limit, max = coef.init.limit),
    
    eta.0.j    = runif(J, min = -coef.init.limit, max = coef.init.limit),
    theta.0.j  = runif(J, min = -coef.init.limit, max = coef.init.limit),
    psi.0.j    = runif(J, min = -coef.init.limit, max = coef.init.limit),
    omega.0.j  = runif(J, min = -coef.init.limit, max = coef.init.limit),
    lambda.0.j = runif(J, min = -coef.init.limit, max = coef.init.limit),
    phi.0.j    = runif(J, min = -coef.init.limit, max = coef.init.limit),
    gamma.0.j  = runif(J, min = -coef.init.limit, max = coef.init.limit),
    beta.0.j   = runif(J, min = -coef.init.limit, max = coef.init.limit),
    beta.x.j   = runif(J, min = -coef.init.limit, max = coef.init.limit),
    
    ### Variances
    ## Scalar
    tau2.gamma.0.j  = runif(1, min = tau2.init.min, max = tau2.init.max),
    tau2.beta.0.j   = runif(1, min = tau2.init.min, max = tau2.init.max),
    tau2.beta.x.j   = runif(1, min = tau2.init.min, max = tau2.init.max),
    
    tau2.eta.0.j    = runif(1, min = tau2.init.min, max = tau2.init.max),
    tau2.theta.0.j  = runif(1, min = tau2.init.min, max = tau2.init.max),
    tau2.psi.0.j    = runif(1, min = tau2.init.min, max = tau2.init.max),
    tau2.omega.0.j  = runif(1, min = tau2.init.min, max = tau2.init.max),
    
    tau2.lambda.0.j = runif(1, min = tau2.init.min, max = tau2.init.max),
    tau2.phi.0.j    = runif(1, min = tau2.init.min, max = tau2.init.max)
  )
  
  inits[names(inits) %in% monitor]
}


