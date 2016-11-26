#' County-level firm births in the United States
#'
#' A dataset containing information on firm birth in multiple sectors by
#' counties in the United States
#'
#' @format A data frame with 3078 rows and 39 variables:
#' \describe{
#'   \item{County}{}
#'   \item{State}{}
#'   \item{JPB_ID}{}
#'   \item{FIPS}{}
#'   \item{X_COORD}{}
#'   \item{Y_COORD}{}
#'   \item{tbirths}{}
#'   \item{subirths}{}
#'   \item{mubirths}{}
#'   \item{texpan}{}
#'   \item{suexpan}{}
#'   \item{muexpan}{}
#'   \item{mestock}{}
#'   \item{msemp}{}
#'   \item{tfdense}{}
#'   \item{pel10emp}{}
#'   \item{pem100emp}{}
#'   \item{mhhi}{}
#'   \item{pci}{}
#'   \item{pop}{}
#'   \item{cclass}{}
#'   \item{uer}{}
#'   \item{pedbs}{}
#'   \item{pedms}{}
#'   \item{pedsc}{}
#'   \item{pedas}{}
#'   \item{pedhs}{}
#'   \item{avg_wage}{}
#'   \item{netflow_emp}{}
#'   \item{proad}{}
#'   \item{interst}{}
#'   \item{avland}{}
#'   \item{taxrev_pc}{}
#'   \item{bci}{}
#'   \item{educ_pc}{}
#'   \item{hwy_pc}{}
#'   \item{metro}{}
#'   \item{micro}{}
#'   \item{noncore}{}
#' }
#'
#' @source \url{http://dx.doi.org/10.1016/j.regsciurbeco.2010.04.001}
#'
"firmbirth"


#' Queen-style county adjacency matrix
#'
#' A \code{CsparseMatrix} representation of county contiguity in the United States.
#'
#'
"wqueen"