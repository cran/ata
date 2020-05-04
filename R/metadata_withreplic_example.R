#' @title ATA Package Example Item Metadata with Item Replications
#' @description Sample data based on data from Parshall et al. (2002) used for the demonstration of the Weighted (positive) Deviations Method (WDM).
#' @format A data frame with 14 rows and 10 variables:
#' \describe{
#'   \item{Item}{Unique item identifier, integer.}
#'   \item{Item2}{Unique item identifier, character letter.}
#'   \item{Content}{Content label, as factor identifying content "A" and "B".}
#'   \item{Content_A}{Dummy code for content "A", 0 and 1 indicators.}
#'   \item{Content_B}{Dummy code for content "B", 0 and 1 indicators.}
#'   \item{p}{Item proportion correct responding, rounded decimal.}
#'   \item{rpbis}{Item-total point biserial correlation, rounded decimal correlation in range -1.00 to 1.00.}
#'   \item{iSx}{Item contribution to total composite standard deviation, double precision numeric.}
#'   \item{Time}{Expected item response time, in minutes.}
#'   \item{Orig_Item}{Original item copy, corresponding "Item" column ID.}
#' }
#' @references Parhshall, C. G., et al. (2002). Automated test assembly for online administration. In C. G. Parhsall, J. A. Spray, J. C. Kalohn, & T. Davey, Practical considerations in computer based testing (pp.106-125). New York, NY: Springer-Verlag New York, Inc.
#' 
"metadata_withreplic_example"
