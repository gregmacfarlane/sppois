#' Spatial Autoregressive Poisson Model
#'
#'
#' This function implements a limited-information maximum likelihood estimator
#' for Poisson regression models. The estimator was described by
#' \href{http://dx.doi.org/10.1016/j.regsciurbeco.2010.04.001}{Lambert,
#' Brown, and Florax (2010)}.
#'
#' @param  A symbolic description of the model to be fit. The details of
#'   model specification are given for \code{\link[stats]{lm}}
#' @param data An optional data frame containing the variables in the model.  By
#'   default the variables are taken from the environment which the function  is
#'   called.
#' @param listw A \code{\link[spdep]{listw}} object created for example by
#'   \code{\link[spdep]{nb2listw}}
#' @param method The method to be used for fitting the regression equation.
#'   Defaults to "liml", a limited-information maximum likelihood. Other
#'   options are "fiml" (full-information maximum likelihood), "model.matrix" to
#'   return a model matrix, and "non-spatial", which will execute a non-spatial
#'   Poisson regression (identical to \code{\link[stats](glm)}).
#'
#' @return An object of class `sarpoisson`.
#' @example R/examples/builddata_example.R
#' @examples
#'   sarpois(counts ~ outcome + treatment, data = dd, listw = W)
#'
#' @seealso \code{\link[spdep]{lagsarlm}}
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
  y <- model.response(mf, "numeric")
  X <- model.matrix(mt, mf, contrasts)


  # Estimate model coefficients -----------
  if (method == "fiml"){  # full-information maximum-likelihood
    W <- listw2mat(listw)
    if(det(W) == 0) # non-invertible matrices have determinant of 0
      Warning("Matrix listw is near-singular, results may not be reliable.")

    nlm_results <- nlm(
      f = lagsarpois.filoglik,
      p = c(0, rep(0, ncol(X))),
      y = y, X = X, W = W,
      hessian = TRUE, ...
    )
    names(nlm_results$estimate) <- c("rho", attr(X, "dimnames")[2][[1]])

  } else if (method == "non-spatial"){
    nlm_results <- nlm(
      f = pois.loglik,
      p = rep(0, ncol(X)),
      y = y, X = X,
      hessian = TRUE, ...
    )
    names(nlm_results$estimate) <- attr(X, "dimnames")[2][[1]]

  }

  # Assemble output class and return ---------
  z <- set_sarpoisson(nlm_results, mf)

  z$method <- method
  z$contrasts <- attr(X, "contrasts")
  z$xlevels <- .getXlevels(mt, mf)
  z$call <- cl
  z$terms <- mt
  return(z)
}

#' Build a glm class object from an NLM poisson fit
#'
#' @param object An object containing the estimation results from an NLM
#'   optimization routine.
#' @param mf The model frame.
#'
#'
#'
#' @return An object of class `c("glm", "lm")`
#'
set_sarpoisson <- function(object, mf){

  y <- model.response(mf, "numeric")
  X <- model.matrix(mt, mf, contrasts)

  me <- list(
    coefficients = object$estimate,
    fitted.values = exp(X %*% object$estimate),
    residuals = y - exp(X %*% object$estimate),
    df.residual = length(y) - length(object$estimate),
    df.null = length(y) - 1,
    logLik = -1 * object$minimum,
    rank = length(object$estimate),
    call = mf,
    nlm_results = object,
    na.action = attr(mf, "na.action"),
    information.matrix = solve(object$hessian)
  )
  # Collect output
  class(me) <- append(class(me), "lm")
  class(me) <- append(class(me), "sarpoisson")

  return(me)
}



#' Full-information maximum likelihood function for spatial poisson
#'
#' @param p Parameter vector consisting of rho, and any betas.
#' @param y n-length vector of dependent values
#' @param X $n\times p$ matrix of independent covariates, created by model.matrix
#' @param W A CsparseMatrix of the weights objects.
#'
#' @return A numeric calculation of the model likelihood.
#'
#' @importFrom Matrix Diagonal
#'
lagsarpois.filoglik <- function(p, y, X, W){
  rho <- p[1]
  beta <- p[-1]

  A <- Matrix::Diagonal(nrow(X)) - rho * W   # (I - rho W)
  A1 <- solve(A)
  axb <- A1 %*% X %*% beta
  mu <- exp(axb)
  lnL <- y %*% axb - sum(mu)

  return(-1 * as.numeric(lnL))
}

#' Non-spatial poisson likelihood
#'
#' @param p Parameter vector consisting of betas.
#' @param y n-length vector of dependent values
#' @param X $n\times p$ matrix of independent covariates, created by model.matrix
#'
#' @return A numeric calculation of the model likelihood.
#'
pois.loglik <- function(p, y, X){
  xb <- X %*% p
  mu <- exp(xb)
  lnL <- y %*% xb - sum(mu)

  return(-1 * as.numeric(lnL))
}
