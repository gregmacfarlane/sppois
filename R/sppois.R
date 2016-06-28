#' Spatial Autoregressive Poisson Model
#'
#' This function implements a limited-information maximum likelihood estimator
#' for Poisson regression models. The estimator was described by
#' \href{http://dx.doi.org/10.1016/j.regsciurbeco.2010.04.001}{Lambert,
#' Brown, and Florax (2010)}.
#'
#' @param formula A symbolic description of the model to be fit. The details of
#'   model specification are given for \code{\link[stats]{lm}}
#' @param dat An optional data frame containing the variables in the model.  By
#'   default the variables are taken from the environment which the function  is
#'   called.
#' @param listw A \code{\link[spdep]{listw}} object created for example by
#'   \code{\link[spdep]{nb2listw}}
#' @param type Default "lag", may be set to "mixed" for a spatial Durbin model;
#'   when "mixed", the lagged intercept is dropped for spatial weights style
#'   "W", that is row-standardised weights, but otherwise included. \em{mixed not
#'   currently supported.}
#'
#'
#' @example R/examples/builddata_example.R
#' @examples
#'   lagsarpois(counts ~ outcome + treatment, data = dd, listw = W)
#'
#' @seealso \code{\link[spdep]{lagsarlm}}
#'
#' @export
#'
lagsarpois <- function(formula, data = list(), listw, type = c("lag", "mixed")){

  # Input management ------

  # set default type to lag if not given
  if (is.null(type)) {
    type <- "lag"
  } else if (type == "mixed") {
    message("mixed not supported, using lag")
    type <- "lag"
  }

  X <- model.matrix(formula, data = data)  # covariate matrix

  # non-linear maximization
  model <- nlm(
    f = lagsarpois.filoglik,  # fiml
    p = c(1, rep(0, ncol(X))),  # initial parameter values are zero for all elements
    y = y,
    X = X,
    W = as(listw, "CsparseMatrix"),
    hessian = TRUE
  )

  return(model)


}

#' Full-information maximum likelihood function for spatial poisson
#'
#' @param theta Parameter vector consisting of rho, and any betas.
#' @param y n-length vector of dependent values
#' @param X $n\times p$ matrix of independent covariates, created by model.matrix
#' @param W A CsparseMatrix of the weights objects.
#'
#' @return A numeric calculation of the model likelihood.
#'
#' @importFrom Matrix Diagonal
#'
lagsarpois.filoglik <- function(theta, y, X, W){
  rho <- theta[1]
  beta <- theta[-1]

  A <- Matrix::Diagonal(nrow(X)) - rho * W   # (I - rho W)
  A1 <- solve(A)
  mu <- exp( A1 %*% X %*% beta)
  lnL <- y %*% A1 %*% X %*% beta - sum(mu)

  return(-1 * as.numeric(lnL))

}
