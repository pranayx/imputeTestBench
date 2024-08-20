#' Sample time series data
#'
#' Sample time series using completely at random (MCAR) or at random (MAR).
#'
#' @param datin input numeric vector (univariate) or data frame/matrix (multivariate)
#' @param smps chr string of sampling type to use, options are \code{"mcar"} or \code{"mar"}
#' @param repetition numeric for repetitions to be done for each missPercent value
#' @param b numeric indicating the total amount of missing data as a percentage to remove from the complete time series
#' @param blck numeric indicating block sizes as a proportion of the sample size for the missing data
#' @param blckper logical indicating if the value passed to \code{blck} is a proportion of \code{missper}, i.e., blocks are to be sized as a percentage of the total size of the missing data
#' @param plot logical indicating if a plot is returned showing the sampled data, plots only the first repetition
#'
#' @return Input data with \code{NA} values for the sampled observations if \code{plot = FALSE}, otherwise a plot showing the missing observations over the complete dataset.
#'
#' @import dplyr ggplot2 data.table
#'
#' @export
#'
#' @examples
#' a <- rnorm(1000)
#' sample_dat(a) # Univariate example
#' b <- data.frame(a = rnorm(1000), b = rnorm(1000))
#' sample_dat(b) # Multivariate example
#'
sample_dat <- function(datin, smps = 'mcar', repetition = 10, b = 10, blck = 50, blckper = TRUE, plot = FALSE) {
  
  # Convert input to data.table for efficient processing
  datin <- as.data.table(datin)
  
  # Sanity check for sampling type
  if (!smps %in% c('mcar', 'mar')) stop('smps must be "mcar" or "mar"')
  
  # Initialize output list
  out <- vector('list', length = repetition)
  
  # Sampling completely at random (MCAR)
  if (smps == 'mcar') {
    for (i in seq_len(repetition)) {
      datsmp <- copy(datin)
      for (col in names(datsmp)) {
        miss_count <- round(nrow(datsmp) * b / 100)
        missing_indices <- sample(2:(nrow(datsmp) - 1), miss_count, replace = FALSE)
        datsmp[missing_indices, (col) := NA]
      }
      out[[i]] <- datsmp
    }
  }
  
  # Sampling at random (MAR)
  if (smps == 'mar') {
    for (i in seq_len(repetition)) {
      datsmp <- copy(datin)
      for (col in names(datsmp)) {
        miss_count <- round(nrow(datsmp) * b / 100)
        blck_size <- if (blckper) pmax(1, round(miss_count * blck / 100)) else pmin(miss_count, blck)
        total_blocks <- floor(miss_count / blck_size)
        missing_indices <- integer(0)
        while (length(missing_indices) < miss_count) {
          start_point <- sample(2:(nrow(datsmp) - blck_size), 1)
          block_indices <- start_point:(start_point + blck_size - 1)
          missing_indices <- unique(c(missing_indices, block_indices[block_indices <= nrow(datsmp)]))
        }
        datsmp[missing_indices[1:miss_count], (col) := NA]
      }
      out[[i]] <- datsmp
    }
  }
  
  # Return plot if required
  if (plot) {
    toplo <- melt(out[[1]], variable.name = 'Series', value.name = 'Value')
    toplo$Index <- rep(1:nrow(out[[1]]), ncol(out[[1]]))
    toplo$Missing <- is.na(toplo$Value)
    p <- ggplot(toplo, aes(x = Index, y = Value, color = Missing)) +
      geom_point(alpha = 0.7) +
      facet_wrap(~Series, scales = "free") +
      theme_bw() +
      theme(legend.position = 'top', legend.key = element_blank(), legend.title = element_blank())
    return(p)
  }
  
  return(out)
}
