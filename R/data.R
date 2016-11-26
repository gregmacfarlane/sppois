#' Crime Rates in Columbus
#'
#' A datset containing information on crime rates, home values, and median
#' incomes for neighborhoods in Columbus, Ohio.
#'
#' @format A data frame with 49 rows and 3 variables:
#' \describe{
#'   \item{crime_i}{Indexed crime rate.}
#'   \item{income}{household income in thousands.}
#'   \item{home_value}{Home values in thousands.}
#' }
#'
#' @note This data is obtained using \link[spdep]{oldcol}
#'
#'
#' @source Anselin, Luc. 1988. Spatial econometrics: methods and models.
#'   Dordrecht: Kluwer Academic, Table 12.1 p. 189.
#'
"columbus_crime"


#' Neighbor relationships for zones in Columbus
#'
#' A listw containing spatial weights for the observations in \link{columbus_crime}.
#'
#' @note This data is obtained using \link[spdep]{oldcol}

#' @source Anselin, Luc. 1988. Spatial econometrics: methods and models.
#'   Dordrecht: Kluwer Academic, Table 12.1 p. 189.
#'
"columbus_neighbors"


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
