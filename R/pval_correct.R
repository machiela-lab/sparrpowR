#' Calculate p-value corrections for multiple testing
#'
#' Internal function to calculate various p-value corrections for use within the \code{\link{spatial_power}} and \code{\link{jitter_power}} functions.
#'
#' @param input An object of class 'rrs' from the \code{\link{spatial_power}} or \code{\link{jitter_power}} function.
#' @param type Character string specifying which correction for multiple comparisons. Options include a False Discovery Rate \code{p_correct = "FDR"}, a Sidak correction \code{p_correct = "Sidak"}, and a Bonferroni correction \code{p_correct = "Bonferroni"}.
#' @param alpha Numeric. The alpha level for significance threshold (default in \code{\link{spatial_power}} and \code{\link{jitter_power}} functions is 0.05).
#' 
#' @details This function provides functionality for multiple testing correction in five ways:
#' 
#' \enumerate{
#' \item Computes a False Discovery Rate by Benjamini and Hochberg \doi{10.1111/j.2517-6161.1995.tb02031.x} (\code{p_correct = "FDR"}) by: 1) sorting the p-values (p_i) of each knot in ascending order (p_1 <= p_2 <= ... <= p_m), 2) starting from p_m find the first p_i for which p_i <= (i/m) * alpha.
#' \item Computes a Sidak correction \doi{10.2307/2283989} (\code{p_correct = "Sidak"}) by 1 - (1 - \code{alpha}) ^ (1 / total number of gridded knots across the estimated surface). The default in the \code{\link[sparr]{risk}} function is a resolution of 128 x 128 or n = 16,384 knots and a custom resolution can be specified using the \code{resolution} argument within the \code{\link[sparr]{risk}} function.
#' \item Computes a Bonferroni correction (\code{p_correct = "Bonferroni"}) by \code{alpha} / total number of gridded knots across the estimated surface. The default in the \code{\link[sparr]{risk}} function is a resolution of 128 x 128 or n = 16,384 knots and a custom resolution can be specified using the \code{resolution} argument within the \code{\link[sparr]{risk}} function.
#' }
#' 
#' @return An object of class 'numeric' with the corrected alpha level.
#'
#' @export
#'
#' @keywords internal
#' 
pval_correct <- function(input,
                         type = c("FDR", "Sidak", "Bonferroni"),
                         alpha = 0.05,
                         nbc = NULL) {
  
  # False Discovery Rate (Benjamini & Hochberg)
  if (type == "FDR") {
    sort_pvals <- sort(as.vector(input$P$v))
    out_alpha <- fdr(sort_pvals, alpha)
    return(out_alpha)
  }
  
  # Sidak correction
  if (type == "Sidak") {
    out_alpha <- 1 - (1 - alpha) ^ (1 / prod(input$P$dim) )
    return(out_alpha)
  }
  
  # Bonferroni correction
  if (type == "Bonferroni") {
    out_alpha <- alpha / prod(input$P$dim)
    return(out_alpha)
  }
}
