#' @title ATA Package Example Item Metadata
#' @description Sample data based on data from Parshall et al. (2002) used for the demonstration of the Weighted (positive) Deviations Method (WDM).
#' @format A data frame with 10 rows and 10 variables:
#' \describe{
#'   \item{Item}{Unique item identifier, integer.}
#'   \item{Content}{Content label, as factor identifying content "A" and "B".}
#'   \item{Content_A}{Dummy code for content "A", 0 and 1 indicators.}
#'   \item{Content_B}{Dummy code for content "B", 0 and 1 indicators.}
#'   \item{p}{Item proportion correct responding, rounded decimal.}
#'   \item{rpbis}{Item-total point biserial correlation, rounded decimal correlation in range -1.00 to 1.00.}
#'   \item{iSx}{Item contribution to total composite standard deviation, double precision numeric.}
#'   \item{Time}{Expected item response time, in minutes.}
#'   \item{Parent0}{Item set ID--initial, unique item set name.}
#'   \item{Parent1}{Item set ID--modified, unique item set name.}
#' }
#' @references Parshall, C. G., et al. (2002). Automated test assembly for online administration. In C. G. Parshall, J. A. Spray, J. C. Kalohn, & T. Davey, Practical considerations in computer based testing (pp.106-125). New York, NY: Springer-Verlag New York, Inc.
#' 
"metadata_example"
