#' CHLA_ERROR_ESTIMATION
#'
#' This function estimates chlorophyll a error according to
#' 	https://doi.org/10.13155/35385
#' @param filenc An object of class ncdf4 of argo bfile
#' @param CHLA_ADJUSTED vector of chlorophyll a adjusted values (numeric)
#' @param ERROR chlorophyll a error values (numeric)
#'
#' @return vector of chlorophyll a error values (numeric)
#' @importFrom ncdf4 ncvar_get
#' @export
#'
CHLA_ERROR_ESTIMATION <- function (filenc,CHLA_ADJUSTED,ERROR) {

#PRES=ncvar_get(filenc,"PRES")

CHLA_ADJUSTED_ERROR=ncdf4::ncvar_get(filenc,"CHLA_ADJUSTED_ERROR")

ERROR_TEMP=abs(ERROR*CHLA_ADJUSTED)+0.02

ERROR_TEMP[which(ERROR_TEMP<0.02)]=0.02

CHLA_ADJUSTED_ERROR=ERROR_TEMP

return(CHLA_ADJUSTED_ERROR)

}
