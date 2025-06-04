#' IPAR_15_DEPTH
#'
#' This function estimates the depth where PAR is <= 15
#'  according to https://doi.org/10.1364/OE.26.024734
#'
#' @param PRES_CTD vector of pressure values at temperature pressure level
#' @param PAR  vector of PAR
#'
#' @return a value of pressure
#' @export
#'
IPAR_15_DEPTH <- function ( PRES_CTD, PAR ) {

PAR_LIMIT=15

i_par=min(which(PAR<PAR_LIMIT))

ipar_15_depth=PRES_CTD[i_par]

return (ipar_15_depth)

}

