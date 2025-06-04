#' read_CTD
#'
#' This function read CTD argo file
#'
#' @param filenc_core An object of class ncdf4 of argo core file
#'
#' @return a list of 3 vector: 1. pressure, 2. salinity, 3.temperature
#' @importFrom  ncdf4 ncvar_get
#' @export
#'
read_CTD <- function ( filenc_core ){

##################################################
#### DATA MODE 
##################################################

DATA_MODE=unlist(strsplit(ncvar_get(filenc_core,"DATA_MODE"),split=""))

DATA_MODE_CTD=DATA_MODE[1]

##################################################
#### Get the Best CTD
##################################################

if ( DATA_MODE_CTD == "R" ) {

	PSAL=ncdf4::ncvar_get(filenc_core,"PSAL")
	TEMP=ncdf4::ncvar_get(filenc_core,"TEMP")
	PRES=ncdf4::ncvar_get(filenc_core,"PRES")

} else {
	
	PSAL=ncdf4::ncvar_get(filenc_core,"PSAL_ADJUSTED")
	TEMP=ncdf4::ncvar_get(filenc_core,"TEMP_ADJUSTED")
	PRES=ncdf4::ncvar_get(filenc_core,"PRES_ADJUSTED")

}

#### CTD is always the 1st profile of the nc
if (filenc_core$dim$N_PROF$len == 1 ) {

        PSAL_CTD=PSAL
        TEMP_CTD=TEMP
        PRES_CTD=PRES

} else {

	if( all(is.na(PSAL) & length(PSAL)==2)) {

		PSAL_CTD=NA
		TEMP_CTD=NA
		PRES_CTD=NA

	} else { 

        	PSAL_CTD=PSAL[,1]
        	TEMP_CTD=TEMP[,1]
        	PRES_CTD=PRES[,1]

		if( all(is.na(PSAL[,1])) & all(!is.na(PSAL[,2])) ) {	# Get the discrete CTD profiles to adjust doxy

        		PSAL_CTD=PSAL[,2]
        		TEMP_CTD=TEMP[,2]
        		PRES_CTD=PRES[,2]

		}

	}

}

result=(list(PRES=PRES_CTD,PSAL=PSAL_CTD,TEMP=TEMP_CTD))
#result=(list(PRES=PRES,PSAL=PSAL,TEMP=TEMP))
return(result)

}