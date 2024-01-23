# The combine function used in foreach
comb <- function(x, ...) {
  lapply(seq_along(x),
         function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
}

# Progress bar used in foreach
progBar <- function(kk, N, per = 1) {
  if (kk %in% seq(1, N, per)) {
    x <- round(kk * 100 / N)
    message("[ ", 
            paste(rep("=", x), collapse = ""),
            paste(rep("-", 100 - x), collapse = ""), 
            " ] ", x, "%", "\r",
            appendLF = FALSE)
    if (kk == N) cat("\r")
  }
}

# False Discovery Rate (Benjamini & Hochberg)
fdr <- function(pvals, alpha) {
  pcrit <- NULL
  m <- length(pvals)
  for (i in 1:m) {
    if (pvals[i] <= (i/m) * alpha) { 
      pcrit <- pvals[i]
    }
  }
  return(max(pcrit, pvals[1]))
}
