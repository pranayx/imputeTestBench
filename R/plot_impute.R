#' Plot imputations
#'
#' Plot imputations for data from multiple methods and support multivariate time series.
#'
#' @param dataIn input data, can be a numeric vector (univariate) or a data frame/matrix (multivariate) for testing
#' @param smps chr string indicating sampling type for generating missing data, see details
#' @param methods chr vector of imputation methods to use, one to many. A user-supplied function can be included if \code{methodPath} is used.
#' @param methodPath chr string of location of script containing one or more functions for the proposed imputation method(s)
#' @param blck numeric indicating block sizes as a percentage of the sample size for the missing data, applies only if \code{smps = 'mar'}
#' @param blckper logical indicating if the value passed to \code{blck} is a percentage of the sample size for missing data, otherwise \code{blck} indicates the number of observations
#' @param missPercent numeric for percent of missing values to be considered
#' @param showmiss logical if removed values missing from the complete dataset are plotted
#' @param multivariate logical indicating whether to plot imputed values for multivariate data separately
#' @param addl_arg arguments passed to other imputation methods as a list of lists, see details.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object showing the imputed data for each method. Red points are labelled as 'imputed' and blue points are labelled as 'retained' from the original dataset. Missing data that were removed can be added to the plot as open circles if \code{showmiss = TRUE}.
#'
#' @import ggplot2
#' @import data.table
#' @importFrom tidyr gather
#' @import zoo
#'
#' @export
#'
plot_impute <- function(dataIn, smps = 'mcar', methods = c("na.approx", "na.interp", "na.interpolation", "na.locf", "na.mean"),  
                        methodPath = NULL, blck = 50, blckper = TRUE, missPercent = 50, showmiss = FALSE, 
                        multivariate = FALSE, addl_arg = NULL) {
  
  # Load custom methods if provided
  if (!is.null(methodPath)) source(methodPath)
  
  # Validate the input
  if (!is.data.frame(dataIn) && !is.matrix(dataIn)) dataIn <- as.data.frame(dataIn)
  
  # Initialize imputation list
  imps <- vector('list', length = length(methods))
  names(imps) <- methods
  
  # Perform imputations
  for (method in methods) {
    arg <- list(addl_arg[[method]])
    sampled_data <- sample_dat(dataIn, smps = smps, b = missPercent, blck = blck, blckper = blckper, plot = FALSE)
    filled <- do.call(method, args = c(list(sampled_data[[1]]), arg))
    imps[[method]] <- filled
  }
  
  # Prepare data for plotting
  toplo <- data.table::rbindlist(lapply(imps, function(x) data.table(x)))
  toplo$Filled <- 'Retained'
  toplo$Filled[is.na(sampled_data[[1]])] <- 'Imputed'
  toplo$Filled <- factor(toplo$Filled)
  toplo$Actual <- dataIn
  toplo$Actual[toplo$Filled %in% 'Retained'] <- NA
  toplo$Time <- 1:nrow(toplo)
  
  if (multivariate) {
    toplo <- tidyr::gather(toplo, 'Method', 'Value', -Time, -Filled, -Actual)
    p <- ggplot(toplo, aes(x = Time, y = Value)) +
      geom_point(aes(colour = Filled), alpha = 0.75, na.rm = TRUE) +
      facet_wrap(~Method, scales = "free", ncol = 1) +
      theme_bw() +
      theme(legend.position = 'top', legend.key = element_blank(), legend.title = element_blank())
  } else {
    toplo <- tidyr::gather(toplo, 'Method', 'Value', -Time, -Filled)
    p <- ggplot(toplo, aes(x = Time, y = Value)) +
      geom_point(aes(colour = Filled), alpha = 0.75, na.rm = TRUE) +
      facet_wrap(~Method, ncol = 1) +
      theme_bw() +
      theme(legend.position = 'top', legend.key = element_blank(), legend.title = element_blank())
  }
  
  # Add actual missing values if showmiss = TRUE
  if (showmiss) {
    p <- p + geom_point(aes(y = Actual, shape = 'Removed'), fill = NA, alpha = 0.75, na.rm = TRUE) +
      scale_shape_manual(values = 21)
  }
  
  return(p)
}
