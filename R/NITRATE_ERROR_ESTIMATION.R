#' DOXY_adj
#'
#' This function estimates nitrate error according to
#' 	https://doi.org/10.13155/84370
#' 
#' @param filenc An object of class ncdf4 of argo bfile
#' @param NITRATE vector of nitrate raw values (numeric)
#' @param NITRATE_ADJUSTED vector of nitrate adjusted values (numeric)
#' @param ERROR nitrate error value
#' @param index_param parameter index in the file matrix
#'
#' @return vector of adjusted nitrate error value
#' @importFrom ncdf4 ncvar_get
#' @export
#' 
NITRATE_ERROR_ESTIMATION <- function (filenc,NITRATE,NITRATE_ADJUSTED,ERROR,index_param) {

PRES=ncdf4::ncvar_get(filenc,"PRES")

DOXY_ADJUSTED_ERROR=ncdf4::ncvar_get(filenc,"DOXY_ADJUSTED_ERROR")

NITRATE_ADJUSTED_ERROR=ncdf4::ncvar_get(filenc,"NITRATE_ADJUSTED_ERROR")

i_prof_param=index_param[,3]

if ( length(which(!is.na(DOXY_ADJUSTED_ERROR)))>1 ) {

	NITRATE_ADJUSTED_ERROR[,i_prof_param]=ERROR+((approx(PRES,DOXY_ADJUSTED_ERROR,PRES[,i_prof_param],rule=1.2)$y)/10.)

} else {

	NITRATE_ADJUSTED_ERROR[,i_prof_param]=ERROR+0.1*abs(NITRATE[,i_prof_param]-NITRATE_ADJUSTED[,i_prof_param])

}

return(NITRATE_ADJUSTED_ERROR)

}
