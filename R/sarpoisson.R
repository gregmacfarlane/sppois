#' Spatial Autoregressive Poisson Model
#'
#'
#' This function implements a limited-information maximum likelihood estimator
#' for Poisson regression models. The estimator was described by
#' \href{http://dx.doi.org/10.1016/j.regsciurbeco.2010.04.001}{Lambert,
#' Brown, and Florax (2010)}.
#'
#' @param formula A symbolic description of the model to be fit. The details of
#'   model specification are given for \code{\link[stats]{lm}}
#' @param data An optional data frame containing the variables in the model.  By
#'   default the variables are taken from the environment which the function  is
#'   called.
#' @param listw A listw object created for example by \code{\link[spdep]{nb2listw}}
#' @param method The method to be used for fitting the regression equation.
#'   Defaults to "liml", a limited-information maximum likelihood. Other
#'   options are "fiml" (full-information maximum likelihood), "model.matrix" to
#'   return a model matrix, and "non-spatial", which will execute a non-spatial
#'   Poisson regression (identical to \code{\link[stats]{glm}}).
#' @param ... Further arguments passed to \link[stats]{nlm}.
#'
#' @return A list of class `sarpoisson` containing the following components:
#' \describe{
#'   \item{coefficients}{The estimated coefficient values.}
#'   \item{fitted.values}{The estimated mean of the poisson distribution.}
#'   \item{residuals}{Difference between estimated mean and observed value.}
#'   \item{df.residuals}{Degrees of freedom remaining in residuals.}
#'   \item{df.null}{Degrees of freedom in the null model.}
#'   \item{logLik}{Numerical log likelihood.}
#'   \item{rank}{Rank of the model.}
#'   \item{call}{Model estimation call.}
#'   \item{nlm_results}{Complete results from \link[stats]{nlm}.}
#'   \item{information.matrix}{Fischer information matrix, obtained as inverse
#'   of Hessian.}
#'   \item{method}{The method used in maximum likelihood estimation.}
#'   \item{xlevels}{}
#'   \item{terms}{Terms of the model frame}
#' }
#'
#' @examples
#'   sarpoisson(crime_i ~ income + home_value, data = columbus_crime,
#'           method = "non-spatial")
#'   sarpoisson(crime_i ~ income + home_value, data = columbus_crime,
#'           listw = columbus_neighbors, method = "fiml")
#'
#' @seealso \code{\link[spdep]{lagsarlm}}
#'
#' @importFrom spdep listw2mat
#' @importFrom stats .getXlevels contrasts model.matrix model.response nlm
#'
#' @export
#'
sarpoisson <- function(formula, data = list(), listw = NULL,  method = "liml",
                       ...){

  # Input management ------
  cl <- match.call()
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "weights", "na.action",
               "offset"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  if (method == "model.frame") return(mf)

  mt <- attr(mf, "terms")
  y <- stats::model.response(mf, "numeric")
  X <- stats::model.matrix(mt, mf)


  # Estimate model coefficients -----------
  if (method == "fiml"){  # full-information maximum-likelihood
    W <- spdep::listw2mat(listw)
    if(det(W) == 0) # non-invertible matrices have determinant of 0
      warning("Matrix listw is near-singular, results may not be reliable.")

    nlm_results <- stats::nlm(
      f = sarpois.filoglik,
      p = c(0, rep(0, ncol(X))),
      y = y, X = X, W = W,
      hessian = TRUE, ...
    )
    names(nlm_results$estimate) <- c("rho", attr(X, "dimnames")[2][[1]])

    A <- Matrix::Diagonal(nrow(X)) - nlm_results$estimate[1] * W   # (I - rho W)
    A1 <- solve(A)
    axb <- A1 %*% X %*% nlm_results$estimate[-1]
    nlm_results$fitted.values = exp(axb)
    nlm_results$residuals = y - nlm_results$fitted.values

  } else if (method == "non-spatial"){
    nlm_results <- stats::nlm(
      f = pois.loglik,
      p = rep(0, ncol(X)),
      y = y, X = X,
      hessian = TRUE, ...
    )
    names(nlm_results$estimate) <- attr(X, "dimnames")[2][[1]]
    nlm_results$fitted.values = exp(X %*% nlm_results$estimate)
    nlm_results$residuals = y - nlm_results$fitted.values

  }

  # Assemble output class and return ---------
  z <- set_sarpoisson(nlm_results, mf, mt)

  z$method <- method
  z$call <- cl
  z$terms <- mt
  return(z)
}

#' Build a glm class object from an NLM poisson fit
#'
#' @param object An object containing the estimation results from an NLM
#'   optimization routine.
#' @param mf The model frame.
#' @param mt The model terms.
#'
#' @return An object of class `c("glm", "lm")`
#'
set_sarpoisson <- function(object, mf, mt){

  y <- model.response(mf, "numeric")
  X <- model.matrix(mt, mf)

  me <- list(
    coefficients = object$estimate,
    fitted.values = object$fitted.values,
    residuals = object$residuals,
    df.residual = length(y) - length(object$estimate),
    df.null = length(y) - 1,
    logLik = -1 * object$minimum,
    rank = length(object$estimate),
    call = mf,
    contrasts = attr(X, "contrasts"),
    levels = stats::.getXlevels(mt, mf),
    nlm_results = object,
    na.action = attr(mf, "na.action"),
    information.matrix = solve(object$hessian)
  )
  # Collect output
  class(me) <- append(class(me), "sarpoisson")

  return(me)
}



#' Full-information maximum likelihood function for spatial poisson
#'
#' @param p Parameter vector consisting of rho, and any betas.
#' @param y n-length vector of dependent values
#' @param X \eqn{n \times p} matrix of independent covariates, created by
#'   model.matrix
#' @param W A CsparseMatrix representation of the weights objects.
#'
#' @return A numeric calculation of the model likelihood.
#'
#' @importFrom Matrix Diagonal
#'
sarpois.filoglik <- function(p, y, X, W){
  rho <- p[1]
  beta <- p[-1]

  # Spatial information matrix
  A <- Matrix::Diagonal(nrow(X)) - rho * W   # (I - rho W)
  A1 <- solve(A)
  axb <- A1 %*% X %*% beta
  mu <- exp(axb)

  lnL <- y %*% axb - sum(mu) - sum(log(factorial(y)))

  return(-1 * as.numeric(lnL))
}

#' Non-spatial poisson likelihood
#'
#' @param p Parameter vector consisting of betas.
#' @param y n-length vector of dependent values
#' @param X \eqn{n \times p} matrix of independent covariates, created by model.matrix
#'
#' @return A numeric calculation of the model likelihood.
#'
pois.loglik <- function(p, y, X){
  xb <- X %*% p
  mu <- exp(xb)
  lnL <- y %*% xb - sum(mu) - sum(log(factorial(y)))

  return(-1 * as.numeric(lnL))
}
