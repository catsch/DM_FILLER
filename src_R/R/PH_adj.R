#' PH_adj
#'
#' This function estimates the pH adjustement according
#' 	to https://archimer.ifremer.fr/doc/00866/97828/
#'
#' @param filenc_core An object of class ncdf4 of argo core file
#' @param filenc An object of class ncdf4 of argo bfile
#' @param PARAM_name name of the variable to be adjusted (string)
#' @param OFFSET offset value for the oxygen adjustment (numeric)
#' @param SLOPE  slope value for the oxygen adjustment (numeric)
#' @param DRIFT drift value for the oxygen adjustment (numeric)
#' @param ERROR vector pH error values (numeric)
#'
#' @return list with vector pH adjusted values (numeric)
#' 	and CTD flag availability (TRUE or FALSE)
#' @importFrom ncdf4 ncvar_get
#' @importFRom dm_filler read_CTD
#' @export
#'
PH_adj <- function ( filenc_core, filenc, PARAM_name, OFFSET, SLOPE, DRIFT, ERROR, profile_date_juld ,launch_date_juld, index_param ){

#### READ Core file

CTD=dm_filler::read_CTD(filenc_core)

# we get        : CTD$PRES
#               : CTD$PSAL
#               : CTD$TEMP

#### READ BFILE

PH_IN_SITU_TOTAL=ncdf4::ncvar_get(filenc,as.character(PARAM_name))

PH_ADJUSTED=ncdf4::ncvar_get(filenc,"PH_IN_SITU_TOTAL_ADJUSTED")

# get the pressure
PRES=ncdf4::ncvar_get(filenc,"PRES")

i_prof_param=index_param[,3]

if ( length(which(!is.na(CTD$TEMP)))>1 && length(which(!is.na(CTD$PSAL)))>1 ) {

	if ( max(CTD$PRES,na.rm=TRUE) > 900 ) {

		TREF=CTD$TEMP[which.min(abs(CTD$PRES-900))]

	} else {

		TREF= 2.

	}

	TCOR=(TREF+273.15)/(CTD$TEMP+273.15)

	# We interpolate TCOR to get the values at pH levels
	TCOR_INTERP<- approx(CTD$PRES, TCOR , PRES[,i_prof_param], rule=1.2)$y

	delta_pH=as.numeric(OFFSET+DRIFT*(profile_date_juld-launch_date_juld)/(365*100))

	PH_ADJUSTED[,i_prof_param] = (PH_IN_SITU_TOTAL[,i_prof_param] - delta_pH*TCOR_INTERP)

} else {

	FLAG_CTD=FALSE	

	# DOXY_ADJUSTED=rep(NA,length(DOXY))

	PH_ADJUSTED=replace(PH_IN_SITU_TOTAL,!is.na(PH_IN_SITU_TOTAL),NA)

}

result=(list(PH=PH_ADJUSTED,FLAG_CTD=FLAG_CTD))

return(result)

}
