# Load necessary libraries
library(foreach)
library(doParallel)
library(data.table)
library(SparkR)
library(forecast)
library(imputeTS)
library(zoo)

#' Function working as a testbench for comparison of imputation models
#'
#' @param dataIn input data, can be a numeric vector (univariate) or a data frame/matrix (multivariate) for testing
#' @param smps chr string indicating sampling type for generating missing data, see details
#' @param methods chr vector of imputation methods to use, one to many. A user-supplied function can be included if \code{methodPath} is used.
#' @param methodPath chr string of location of script containing one or more functions for the proposed imputation method(s)
#' @param errorParameter chr string indicating which error type to use, acceptable values are \code{"rmse"} (default), \code{"mae"}, or \code{"mape"}.
#' @param errorPath chr string of location of script containing one or more error functions for evaluating imputations
#' @param blck numeric indicating block sizes as a percentage of the sample size for the missing data, applies only if \code{smps = 'mar'}
#' @param blckper logical indicating if the value passed to \code{blck} is a percentage of the sample size for missing data, otherwise \code{blck} indicates the number of observations
#' @param missPercentFrom numeric from which percent of missing values to be considered
#' @param missPercentTo numeric for up to what percent missing values are to be considered
#' @param interval numeric for interval between consecutive missPercent values
#' @param repetition numeric for repetitions to be done for each missPercent value
#' @param addl_arg arguments passed to other imputation methods as a list of lists, see details.
#' @param use_parallel logical, if TRUE enables parallel processing
#' @param n_cores integer, number of cores to use for parallel processing (defaults to available cores minus 1)
#'
#' @details
#' The function now supports multivariate time series data and parallel processing for faster computation. 
#' The user can specify custom imputation and error methods by providing paths to scripts via \code{methodPath} and \code{errorPath}.
#'
#' @import data.table
#' @importFrom foreach %dopar%
#'
#' @export
#'
impute_errors <- function(dataIn, smps = 'mcar', methods = c("na.approx", "na.interp", "na.interpolation", "na.locf", "na.mean"),  
                          methodPath = NULL, errorParameter = 'rmse', errorPath = NULL, blck = 50, blckper = TRUE, 
                          missPercentFrom = 10, missPercentTo = 90, interval = 10, repetition = 10, 
                          addl_arg = NULL, use_parallel = TRUE, n_cores = detectCores() - 1) {
  
  # Load custom methods if provided
  if (!is.null(methodPath)) source(methodPath)
  if (!is.null(errorPath)) source(errorPath)
  
  # Validate the input
  if (!is.data.frame(dataIn) && !is.matrix(dataIn)) dataIn <- as.data.frame(dataIn)
  
  # Register parallel backend
  if (use_parallel) {
    cl <- makeCluster(n_cores)
    registerDoParallel(cl)
  }
  
  # Initialize the output list
  percs <- seq(missPercentFrom, missPercentTo, interval)
  errall <- vector('list', length = length(methods))
  names(errall) <- methods
  
  # Parallelized imputation and error evaluation
  foreach(meth = methods, .combine = 'c', .packages = c('data.table', 'forecast', 'zoo', 'imputeTS', 'SparkR')) %dopar% {
    args <- list(addl_arg[[meth]])
    for (perc in percs) {
      imputed_errors <- foreach(rep = 1:repetition, .combine = 'c') %dopar% {
        # Generate missing data
        sampled_data <- sample_dat(dataIn, smps = smps, b = perc, blck = blck, blckper = blckper, plot = FALSE)
        
        # Apply the imputation method
        filled <- do.call(meth, args = c(list(sampled_data), args))
        
        # Calculate the error
        err <- do.call(errorParameter, args = list(dataIn, filled))
        return(err)
      }
      errall[[meth]][[as.character(perc)]] <- mean(imputed_errors)
    }
  }
  
  # Stop the cluster
  if (use_parallel) stopCluster(cl)
  
  # Organize output as an errprof object
  out <- list(Parameter = errorParameter, MissingPercent = percs, Errors = errall)
  class(out) <- c('errprof', 'list')
  
  return(out)
}
