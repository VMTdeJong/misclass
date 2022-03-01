#' Sample clustered binary data with misclassified exposure
#' 
#' Sample clustered binary data with misclassified exposure, correctly 
#'   classified exposure, covariates and outcome. Random intercepts for outcome, 
#'   measurementand exposure model. Random effects for outcome model.
#' 
#' @param n sample size per cluster
#' @param J number of clusters
#' @param beta.x.0 coefficient for x in the outcome model
#' @param tau.beta.x.j variance of random effect for x in the outcome model
#' @param beta.z.0 log odds ratios for covariates.
#' @param beta.0.0 intercept in the outcome model. if NULL, the incidence is 
#'   fixed so that mean(y) converges to 1/2.
#' @param tau.beta.0.j variance of the random intercept in the outcome model
#' @param tau.z.mu SD of the mean of the z covariates across studies
#' @param cov Covariance for continuous covariates
#' @param gamma.z.0 coefficients for z in the exposure model
#' @param gamma.0.0 intercept in the exposure model
#' @param tau.gamma.0.j variance of the random intercept in the exposure model
#' @param lambda.0.0 intercept in the measurement model, if x = 1
#' @param tau.lambda.0.j variance of the random intercept in the measurement 
#'   model, if x = 1
#' @param lambda.z.0 coefficients in the measurement model, if x = 1
#' @param phi.0.0 intercept in the measurement model, if x = 0
#' @param tau.phi.0.j variance of the random intercept in the measurement model, 
#'   if x = 0
#' @param phi.z.0 coefficients in the measurement model, if x = 0
#' @param colnames optional column names
#' @param center.z should covariates be centered? (post hoc)
#' 
#' @importFrom stats rnorm
#' 
#' @returns A data.frame, including the mismeasured and correctly measured 
#'   variables.
#' @examples
#' d <- sample.misclass(gamma.z.0 = c(1/2, 1/2),
#' lambda.z.0 = c(-1, -1),
#' phi.z.0 = c(1, 1),
#' beta.z.0 = c(1, 1))
#' @export
sample.misclass <- function(n = 50, J = 1,
                            beta.0.0 = NULL, tau.beta.0.j = 0,
                            beta.x.0 = log(2), tau.beta.x.j = 0, beta.z.0 = log(2), 
                            tau.z.mu = 1/2, cov = 1/4,
                            gamma.0.0 = 0, tau.gamma.0.j = 0, gamma.z.0 = 1/2, 
                            lambda.0.0 = 3, tau.lambda.0.j = 1, lambda.z.0 = -2,
                            phi.0.0 = -3, tau.phi.0.j = 1, phi.z.0 = 2,
                            colnames = NULL, center.z = TRUE) {
  ### Dimensions
  N <- J * n
  lengths <- sapply(list(gamma.z.0, lambda.z.0, phi.z.0, beta.z.0), length)
  if (!all(outer(lengths, lengths, Vectorize(identical))))
    stop(paste0("The number of coefficients for z should be the same for each submodel. 
    The numbers were: ", paste0(lengths, collapse = " "), " for gamma.z.0, lambda.z.0, phi.z.0, beta.z.0 
    respectively."))

  # Sample parameters for exposure model
  z.mean.j <- rnorm(J, mean = 0, sd = tau.z.mu)
  gamma.0.j <- rnorm(J, mean = gamma.0.0, sd = tau.gamma.0.j)
  
  # Sample exposure and covariate
  dat <- list()
  
  for (Jj in seq_len(J))
    dat[[Jj]] <- sample.x.z.cont.to.bin(n = n, gamma.0.0 = gamma.0.j[Jj], gamma.z.0 = gamma.z.0, z.mean = z.mean.j[Jj], 
                                        cov = cov, j = Jj, outcome = NA)
  
  dat <- Reduce(rbind, dat)
  
  x <- dat[ , "x"]
  z <- dat[ , grep("z", colnames(dat))]
  j <- dat[ , "j"]
  
  ### Outcome model
  # Optionally set mean(y) = .5
  if (is.null(beta.0.0)) 
    beta.0.0 <- -log(sqrt(prod(exp(c(beta.x.0))))) 
  
  # Generate random intercepts and coefficients.
  beta.0.j <- rnorm(J, beta.0.0, tau.beta.0.j)[j]
  beta.x.j <- rnorm(J, beta.x.0, tau.beta.x.j)[j]
  coefficients <- cbind(matrix(beta.0.j, nrow = N, ncol = 1), 
                        matrix(beta.x.j, nrow = N, ncol = 1), 
                        matrix(rep(beta.z.0, each = N), ncol = length(beta.z.0), nrow = N))
  
  # Draw outcome
  y.p <- inv.logit(rowSums(coefficients * cbind(1, x, z)))
  y   <- rbinom(N, size = 1, prob = y.p)
  
  ### Measurement model
  s <- rep(0, N)
  
  lambda.0.j <- rnorm(J, lambda.0.0, tau.lambda.0.j)[j]
  s.x1.p <- inv.logit(lambda.0.j + lambda.z.0 * z) 
  s[x == 1] <- rbinom(N, 1, s.x1.p)[x == 1]
  
  phi.0.j <- rnorm(J, phi.0.0, tau.phi.0.j)[j]
  s.x0.p <- inv.logit(phi.0.j + phi.z.0 * z) 
  s[x == 0] <- rbinom(N, 1, s.x0.p)[x == 0]
  
  # Return everything except the intercept
  out <- data.frame(y, j, s, x, z)
  if (!is.null(colnames))
    colnames(out) <- colnames
  
  if (center.z)
    out <- center.covariates(out, y.name = c("y", "x", "s"), cluster.name = "j")
  out
}

sample.x.z.cont.to.bin <- function(n, gamma.0.0, gamma.z.0, z.mean, cov, j = NA, outcome = NA) {
  z <- sample.z.cont(n = n, z.n = length(gamma.z.0), z.mean = z.mean, cov = cov)
  x.p <- inv.logit(gamma.0.0 + z %*% gamma.z.0)
  x <- stats::rbinom(n, 1, x.p)  
  cbind(x = x, z = z, j = rep(j, n), outcome = rep(outcome, n))
}

sample.z.cont <- function(n, z.n, z.mean, cov) {
  sigma <- matrix(cov, ncol = z.n, nrow = z.n)
  diag(sigma) <- 1
  out <- mvtnorm::rmvnorm(n = n, mean = rep(z.mean, z.n), sigma = sigma)
  colnames(out) <- paste("z", seq(from = 1, to = z.n), sep = "")
  out
}


# Remove data from object
# 
# data data set.
# cluster name of cluster variable
add.missing <- function(data, J.gold = 5) {
  data$x.miss <- data$x
  data$x.miss[data$j > J.gold] <- NA
  data
}

center.covariates <- function (data, y.name, cluster.name) {
  to.center <- which((!(colnames(data) %in% cluster.name | 
                          colnames(data) %in% y.name)) & sapply(data, is.numeric))
  cluster.vec <- data[, cluster.name]
  for (col in to.center) data[, col] <- center(data[, col], 
                                               center.in = cluster.vec)
  data
}

center <- function (x, center.in) {
  if (length(center.in) != length(x)) 
    stop("length(center.in) should match length(x).")
  for (trial in sort(unique(center.in))) {
    selection.id <- center.in == trial
    selection <- x[selection.id]
    x[selection.id] <- selection - mean(selection, na.rm = T)
  }
  x
}
