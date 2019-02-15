DOXY_adj <- function ( filenc_core, filenc, PARAM_name, OFFSET, SLOPE, DRIFT, profile_date_juld ,launch_date_juld ){

#### READ Core file 

CTD=read_CTD(filenc_core)

# we get        : CTD$PRES
#               : CTD$PSAL
#               : CTD$TEMP

#### READ BFILE

DOXY=ncvar_get(filenc,as.character(PARAM_name))

PRES=ncvar_get(filenc,"PRES")

# We interpolate CTD DATA to get TEMP and PSAL at all levels
TEMP_INTERP<- approx(PRES, CTD$TEMP, CTD$PRES, rule=2)$y

PSAL_INTERP  <- approx(PRES, CTD$PSAL, CTD$PRES, rule=2)$y

#### Estimate PPOX from DOXY
# calculate PPOX_DOXY in mbar from DOXY in micromol/kg
PPOX_DOXY=DOXY_to_PPOX(PRES, TEMP_INTERP, PSAL_INTERP, DOXY)

PPOX_DOXY_ADJUSTED=as.numeric((1.+DRIFT/100.*(profile_date_juld-launch_date_juld)/365.))*(SLOPE*PPOX_DOXY+OFFSET)

# calculate DOXY in micromol/kg from PPOX_DOXY in mbar
DOXY_ADJUSTED=PPOX_to_DOXY(PRES, TEMP_INTERP, PSAL_INTERP, PPOX_DOXY_ADJUSTED)

return(DOXY_ADJUSTED)
}
