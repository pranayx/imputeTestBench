#' Function to plot the Error Comparison
#'
#' @param dataIn an errprof object returned from \code{\link{impute_errors}}
#' @param plotType chr string indicating plot type, accepted values are \code{"boxplot"}, \code{"bar"}, or \code{"line"}
#' @param multivariate logical indicating whether to plot multivariate errors separately
#'
#' @return A ggplot object that can be further modified. If \code{plotType = "boxplot"}, the entire range of errors are shown; otherwise, averages are shown if \code{plotType = "bar"} or \code{"line"}.
#'
#' @importFrom reshape2 melt
#' @import ggplot2
#' @import data.table
#'
#' @export
#'
#' @examples
#' aa <- impute_errors(dataIn = nottem)
#' plot_errors(aa)
#'
#' @method plot_errors errprof
plot_errors <- function(dataIn, plotType = c('boxplot'), multivariate = FALSE) {
  plotType <- match.arg(plotType)
  
  if (!plotType %in% c('boxplot', 'bar', 'line')) stop('plotType must be "boxplot", "bar", or "line"')
  
  # Convert to data.table for efficient processing
  if (multivariate) {
    toplo <- data.table::rbindlist(lapply(dataIn$Errors, function(x) data.table::melt(x, id.vars = 'MissingPercent')))
    setnames(toplo, c("Percent of missing observations", "Methods", "Error value"))
  } else {
    toplo <- data.table::melt(dataIn$Errors, id.vars = 'MissingPercent')
    setnames(toplo, c("Percent of missing observations", "Methods", "Error value"))
  }
  
  # Boxplot
  if (plotType == 'boxplot') {
    p <- ggplot(toplo, aes(x = `Percent of missing observations`, y = `Error value`)) +
      ggtitle(dataIn$Parameter) +
      geom_boxplot(aes(fill = Methods)) +
      facet_wrap(~variable, scales = "free") +
      theme_bw()
  }
  
  # Barplot
  if (plotType == 'bar') {
    p <- ggplot(toplo, aes(x = `Percent of missing observations`, y = `Error value`)) +
      ggtitle(dataIn$Parameter) +
      geom_bar(aes(fill = Methods), stat = 'identity', position = 'dodge') +
      theme_bw()
  }
  
  # Line plot
  if (plotType == 'line') {
    p <- ggplot(toplo, aes(x = `Percent of missing observations`, y = `Error value`, group = Methods)) +
      ggtitle(dataIn$Parameter) +
      geom_line() +
      geom_point(aes(fill = Methods), shape = 21, size = 5, alpha = 0.75) +
      theme_bw()
  }
  
  return(p)
}
