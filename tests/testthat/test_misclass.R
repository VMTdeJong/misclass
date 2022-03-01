context("Misclassification model")

### Data
set.seed(1111)
d <- misclass:::sample.misclass(n = 10, J = 5, beta.z.0 = c(2))
d_miss <- misclass:::add.missing(d, J.gold = 4)

library(testthat)
mcmc.n <- 100

### Tests
test_that("The misclassification model runs", {
  invisible(capture.output(
    m <- misclass::misclass(x = d_miss$x.miss,
                            s = d_miss$s,
                            y = d_miss$y,
                            z = d_miss$z,
                            j = d_miss$j,
                            adapt = mcmc.n,
                            warmup = mcmc.n,
                            sample = mcmc.n)
  ))
  expect_is(m, "misclass")
  
  invisible(capture.output(
    m <- extend.misclass(m, sample = mcmc.n)
  ))
  
  expect_equivalent(m$jags$sample, 2*mcmc.n)
  
  invisible(capture.output(
    summary(m)
  ))
  
  invisible(capture.output(
    print(m)
  ))
})


test_that("Random effects models can be estimated", {
  invisible(capture.output(
    m2 <- misclass::misclass(x = d_miss$x.miss,
                             s = d_miss$s,
                             y = d_miss$y,
                             z = d_miss$z,
                             j = d_miss$j,
                             adapt = mcmc.n,
                             warmup = mcmc.n,
                             sample = mcmc.n,
                             monitor.re = T,
                             measurement = list(i = T),
                             exposure = list(i = T),
                             outcome = list(i = T, 
                                            s = T)) # add test to see whether model contains right variables
  ))
  expect_is(m2, "misclass")
  expect_true("tau.beta.0.j" %in% names(coef(m2)))
  expect_true("gamma.0.j[5]" %in% names(coef(m2)))
  
  expect_equal(length(coef(m2)), 39)
  expect_equal(dim(confint(m2)), c(39, 2))
})


test_that("Differential stratified models can be estimated", {
  invisible(capture.output(
    m3 <- misclass::misclass(x = d_miss$x.miss,
                             s = d_miss$s,
                             y = d_miss$y,
                             z = d_miss$z,
                             j = d_miss$j,
                             adapt = mcmc.n,
                             warmup = mcmc.n,
                             sample = mcmc.n,
                             measurement = list(stratified.differential = T,
                                                intercept.random = T),
                             exposure = list(intercept.random = T),
                             outcome = list(intercept.random = T, 
                                            slope.random = T))
  ))
  expect_is(m3, "misclass")
})


### Data
set.seed(1111)


test_that("Input is checked", {
  d <- misclass:::sample.misclass(n = 10, J = 2, beta.z.0 = c(2))
  d_miss <- misclass:::add.missing(d, J.gold = 1) # J.gold should be >1 for random effects/ intercepts
  
  expect_error(m4 <- misclass::misclass(x = d_miss$x.miss,
                                        s = d_miss$s,
                                        y = d_miss$y,
                                        z = d_miss$z,
                                        j = d_miss$j,
                                        adapt = mcmc.n,
                                        warmup = mcmc.n,
                                        sample = mcmc.n,
                                        monitor.re = T,
                                        measurement = list(intercept.random = T),
                                        exposure = list(intercept.random = T),
                                        outcome = list(intercept.random = T, 
                                                       slope.random = T)))
})

test_that("The true values are estimated", {
  skip_on_cran() # "Takes to long for CRAN: 23 minutes"
  print("Finished all CRAN tests")
  set.seed(3333)
  d <- misclass:::sample.misclass(n = 200, 
                                  J = 20, 
                                  beta.0.0 = 0, 
                                  beta.z.0 = 1, 
                                  tau.beta.0.j = 1, 
                                  tau.beta.x.j = 1)
  
  d_miss <- misclass:::add.missing(d, J.gold = 15)
  mcmc.n <- 1000
  invisible(capture.output(
    time <- system.time(m5 <- misclass::misclass(x = d_miss$x.miss,
                                                 s = d_miss$s,
                                                 y = d_miss$y,
                                                 z = d_miss$z,
                                                 j = d_miss$j,
                                                 adapt = mcmc.n,
                                                 warmup = mcmc.n,
                                                 sample = mcmc.n * 2,
                                                 monitor.re = F,
                                                 measurement = list(intercept.random = T),
                                                 exposure = list(intercept.random = T),
                                                 outcome = list(intercept.random = T, 
                                                                slope.random = T)))
  ))
  s5 <- summary(m5$jags)
  
  # Measurement 
  expect_true(s5["lambda.0.0", "Lower95"] < 3)
  expect_true(s5["lambda.0.0", "Upper95"] > 3)
  
  expect_true(s5["lambda.z.0", "Lower95"] < -2)
  expect_true(s5["lambda.z.0", "Upper95"] > -2)
  
  expect_true(s5["phi.0.0", "Lower95"] < -3)
  expect_true(s5["phi.0.0", "Upper95"] > -3)
  
  expect_true(s5["phi.z.0", "Lower95"] < 2)
  expect_true(s5["phi.z.0", "Upper95"] > 2)
  
  expect_true(s5["tau.lambda.0.j", "Lower95"] < 1)
  expect_true(s5["tau.lambda.0.j", "Upper95"] > 1)
  
  expect_true(s5["tau.phi.0.j", "Lower95"] < 1)
  expect_true(s5["tau.phi.0.j", "Upper95"] > 1)
  
  # Exposure
  expect_true(s5["gamma.0.0", "Lower95"] < 0)
  expect_true(s5["gamma.0.0", "Upper95"] > 0)
  
  expect_true(s5["gamma.z.0", "Lower95"] < 1/2)
  expect_true(s5["gamma.z.0", "Upper95"] > 1/2)
  
  expect_true(s5["tau.gamma.0.j", "Lower95"] < 0.25) # True value is actually 0
  expect_true(s5["tau.gamma.0.j", "Upper95"] < 1)
  
  # Outcome
  expect_true(s5["beta.0.0", "Lower95"] < 0)
  expect_true(s5["beta.0.0", "Upper95"] > 0)
  
  expect_true(s5["beta.x.0", "Lower95"] < log(2))
  expect_true(s5["beta.x.0", "Upper95"] > log(2))
  
  expect_true(s5["beta.z.0", "Lower95"] < 1)
  expect_true(s5["beta.z.0", "Upper95"] > 1)
  
  expect_true(s5["tau.beta.0.j", "Lower95"] < 1)
  expect_true(s5["tau.beta.0.j", "Upper95"] > 1)
  
  expect_true(s5["tau.beta.x.j", "Lower95"] < 1)
  expect_true(s5["tau.beta.x.j", "Upper95"] > 1)
})


