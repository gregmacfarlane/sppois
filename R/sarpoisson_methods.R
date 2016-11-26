#' Summarizing SAR Poisson Models
#'
#' `summary` method for class "`sarpoisson`"
#'
#' @param object An object of class `sarpoisson`, usually a result of a call to
#'   \link{sarpoisson}
#' @method summary sarpoisson
#'
summary.sarpoisson <- function(object){

  df.r <- object$df.residual
  aliased <- is.na(stats::coef(object))
  p <- object$rank
  if (p > 0) {
    p1 <- 1L:p
    coef.p <- object$coefficients
    covmat.unscaled <- object$information.matrix
    dimnames(covmat.unscaled) <- list(names(coef.p), names(coef.p))
    covmat <- 1 * covmat.unscaled
    var.cf <- diag(covmat)
    s.err <- sqrt(var.cf)
    tvalue <- coef.p/s.err
    dn <- c("Estimate", "Std. Error")

    if (df.r > 0) {
      pvalue <- 2 * stats::pt(-abs(tvalue), df.r)
      coef.table <- cbind(coef.p, s.err, tvalue, pvalue)
      dimnames(coef.table) <- list(names(coef.p), c(dn, "t value", "Pr(>|t|)"))
    } else {
      coef.table <- cbind(coef.p, NaN, NaN, NaN)
      dimnames(coef.table) <- list(names(coef.p), c(dn, "t value", "Pr(>|t|)"))
    }
  } else {
    coef.table <- matrix( 0L, 4L)
    dimnames(coef.table) <- list(NULL, c("Estimate", "Std. Error",
                                         "t value", "Pr(>|t|)"))
    covmat.unscaled <- covmat <- matrix( 0L, 0L)
    df.f <- length(aliased)
  }
  keep <- match(c("call", "terms", "family", "deviance", "aic",
                  "contrasts", "df.residual", "null.deviance", "df.null",
                  "iter", "na.action", "logLik"), names(object), 0L)
  ans <- c(object[keep],  list(
    deviance.resid = stats::residuals(object, type = "deviance"),
    coefficients = coef.table, aliased = aliased,
    df = c(object$rank, df.r),
    cov.unscaled = covmat.unscaled, cov.scaled = covmat))

  dd <- sqrt(diag(covmat.unscaled))
  ans$correlation <- covmat.unscaled/outer(dd, dd)
  ans$object <- object

  class(ans) <- "summary.sarpoisson"

  return(ans)

}

print.summary.sarpoisson <- function(x, digits = 3){

  # funtion call
  cat("\nCall:", deparse(x$call), sep = "", fill = TRUE)

  # optimization warning
  if(x$object$nlm_results$code > 2){
    warning("nlm did not fully succeed, warning code ", x$object$nlm_results$code)
  }

  # residuals
  cat("\nResiduals:\n")
  resid <- x$object$residuals
  nam <- c("Min", "1Q", "Median", "3Q", "Max")
  print(structure(stats::quantile(resid), names = nam))

  cat("\nCoefficients:", x$coeftitle, "\n")
  # coefficient table
  stats::printCoefmat(x$coefficients, signif.stars = TRUE,
                      digits = digits, na.print = "NA")

  cat("\nLog likelihood:", x$logLik, "for", x$object$method, "model\n")

  cat("\n")

}

print.sarpoisson <- function(x){
  # funtion call
  cat("\nCall:\n")
  cat(deparse(x$call))

  cat("\n\nCoefficients:\n")
  print(x$coefficients)

}
