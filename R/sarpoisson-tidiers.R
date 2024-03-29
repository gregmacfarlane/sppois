#' Tidying methods for spatially autoregressive poisson models
#'
#' These methods tidy the coefficients of spatial autoregression
#' models generated by functions in the `spatialreg` package.
#'
#' @param x An object returned from [sppois::sarpoisson()]
#'
#'
#' @examples
#' \dontrun{
#' library(sppois)
#' crime_sppois <- sarpoisson(crime_i ~ income + home_value, data = columbus_crime,
#'                            listw = columbus_neighbors, method = "fiml")
#'
#' tidy(crime_sppois)
#' tidy(crime_sppois, conf.int = TRUE, conf.level = 0.99)
#' glance(crime_sppois)
#'
#' }
#' @aliases sppois_tidiers
#' @export
#'
tidy.sarpoisson <- function(x, conf.int = FALSE, conf.level = 0.95, ...){

  s <- summary(x)

  result <- s$coefficients %>%
    tibble::as_tibble(rownames = "term") %>%
    dplyr::rename(estimate = Estimate,
                  std.error = `Std. Error`,
                  statistic = `t value`,
                  p.value = `Pr(>|t|)`)

  # Calculate confidence interval
  if (conf.int) {
    ci <- broom:::broom_confint_terms(x, level = conf.level)
    result <- dplyr::left_join(result, ci, by = "term")
  }

  result
}

#' @export
glance.sarpoisson <- function(x, ...){
  with(
    summary(x),
    tibble::tibble(
      logLik = as.numeric(x$logLik),
      df.residual = x$df.residual,
      nobs = length(x$residuals)
    )
  )
}




#' @importFrom generics glance
#' @export
generics::glance

#' @importFrom generics tidy
#' @export
generics::tidy

#' @importFrom generics augment
#' @export
generics::augment
