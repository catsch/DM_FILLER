DOXY_adj <- function ( filenc_core, filenc, PARAM_name, OFFSET, SLOPE, DRIFT, INCLINE_T, ERROR, profile_date_juld ,launch_date_juld ){

#### READ Core file 

CTD=read_CTD(filenc_core)

# we get        : CTD$PRES
#               : CTD$PSAL
#               : CTD$TEMP

#### READ BFILE

DOXY=ncvar_get(filenc,as.character(PARAM_name))

#### Set Up the error DOXY values where DOXY is different from NA
PPOX_ERROR=replace(DOXY,!is.na(DOXY),ERROR)

# get the pressure
PRES=ncvar_get(filenc,"PRES")

if ( length(which(!is.na(CTD$TEMP)))>1 && length(which(!is.na(CTD$PSAL)))>1 ) {

	# We interpolate CTD DATA to get TEMP and PSAL at all levels
	TEMP_INTERP<- approx(CTD$PRES, CTD$TEMP, PRES, rule=2)$y

	PSAL_INTERP  <- approx(CTD$PRES, CTD$PSAL, PRES, rule=2)$y

	#### Estimate PPOX from DOXY
	# calculate PPOX_DOXY in mbar from DOXY in micromol/kg
	PPOX_DOXY=DOXY_to_PPOX(PRES, TEMP_INTERP, PSAL_INTERP, DOXY)

	PPOX_DOXY_ADJUSTED=as.numeric((1.+DRIFT/100.*(profile_date_juld-launch_date_juld)/365.)+INCLINE_T*TEMP_INTERP)*(SLOPE*PPOX_DOXY)+OFFSET

	# calculate DOXY in micromol/kg from PPOX_DOXY in mbar
	DOXY_ADJUSTED=PPOX_to_DOXY(PRES, TEMP_INTERP, PSAL_INTERP, PPOX_DOXY_ADJUSTED)

	# calculate the error on DOXY from error on PPOX
	ERROR_DOXY=PPOX_to_DOXY(PRES, TEMP_INTERP, PSAL_INTERP, PPOX_ERROR)

} else {

	FLAG_CTD=FALSE	

	# DOXY_ADJUSTED=rep(NA,length(DOXY))

	DOXY_ADJUSTED=replace(DOXY,!is.na(DOXY),NA)

	# ERROR_DOXY=rep(NA,length(DOXY))
	
	ERROR_DOXY=replace(DOXY,!is.na(DOXY),NA)

}

result=(list(DOXY=DOXY_ADJUSTED,DOXY_ERROR=ERROR_DOXY,FLAG_CTD=FLAG_CTD))

return(result)


}
