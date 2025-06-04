#' PH_ERROR_ESTIMATION
#'
#' This function estimates the pH error according
#' 	to https://archimer.ifremer.fr/doc/00866/97828/
#'
#' @param filenc An object of class ncdf4 of argo bfile
#' @param PH_IN_SITU_TOTAL vector of ph total raw values (string)
#' @param PH_IN_SITU_TOTAL_ADJUSTED vector of ph total adjusted values (string)
#' @param ERROR vector pH error values (numeric)
#' @param index_param parameter index in the file matrix
#' @param FLAG_CTD TRUE or FALSE for CTD availability
#'
#' @return vector of pH adjusted error values (numeric)
#' @importFrom ncdf4 ncvar_get
#' @export
#'
PH_ERROR_ESTIMATION <- function (filenc,PH_IN_SITU_TOTAL,PH_IN_SITU_TOTAL_ADJUSTED,ERROR,index_param,FLAG_CTD) {

PRES=ncdf4::ncvar_get(filenc,"PRES")

DOXY_ADJUSTED_ERROR=ncdf4::ncvar_get(filenc,"DOXY_ADJUSTED_ERROR")

PH_IN_SITU_TOTAL_ADJUSTED_ERROR=ncdf4::ncvar_get(filenc,"PH_IN_SITU_TOTAL_ADJUSTED_ERROR")

i_prof_param=index_param[,3]

if (!FLAG_CTD) {

        PH_IN_SITU_TOTAL_ADJUSTED_ERROR=replace(PH_IN_SITU_TOTAL,!is.na(PH_IN_SITU_TOTAL),NA)

} else {

	if ( length(which(!is.na(DOXY_ADJUSTED_ERROR)))>1 ) {

		PH_IN_SITU_TOTAL_ADJUSTED_ERROR[,i_prof_param]=ERROR+((approx(PRES,DOXY_ADJUSTED_ERROR,PRES[,i_prof_param],rule=1.2)$y)*0.0016)

	} else {

		PH_IN_SITU_TOTAL_ADJUSTED_ERROR[,i_prof_param]=ERROR+0.0016*abs(PH_IN_SITU_TOTAL[,i_prof_param]-PH_IN_SITU_TOTAL_ADJUSTED[,i_prof_param])

	}

}


return(PH_IN_SITU_TOTAL_ADJUSTED_ERROR)

}
