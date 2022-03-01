#' @param measurement List for measurement submodel, containing: 
#'   \code{stratified.differential} to define a stratified differential 
#'   misclassification model (i.e. the measurement submodel is stratified by
#'    values of y);
#'   \code{additive.differential} to define a differential misclassification
#'   model with additive terms for y in the measurement submodel (i.e. linear
#'   on the log-odds scale);
#'   \code{covariate} to allow for covariate effects (for z); 
#'   and \code{intercept.random} to allow for random intercepts in the 
#'   measurement submodel. All may be abbreviated to the first letter.
#' @param exposure List for exposure submodel, containing:
#' \code{covariate} to allow for covariate effects (for z) in the exposure
#'   submodel; 
#'   and \code{intercept.random} to allow for random intercepts in the exposure
#'   submodel. All may be abbreviated to the first letter.
#' @param outcome List for outcome submodel, containing: 
#'   \code{covariate} to allow for covariate effects (for z) in the outcome
#'   submodel; 
#'   \code{intercept.random} to allow for random intercepts in the outcome
#'   submodel;
#'   and \code{effects.random} to allow for random effects (random slopes) in 
#'   the outcome submodel. All may be abbreviated to the first letter.
