NITRATE_ERROR_ESTIMATION <- function (filenc,NITRATE,NITRATE_ADJUSTED,ERROR,index_param) {

PRES=ncvar_get(filenc,"PRES")

DOXY_ADJUSTED_ERROR=ncvar_get(filenc,"DOXY_ADJUSTED_ERROR")

NITRATE_ADJUSTED_ERROR=ncvar_get(filenc,"NITRATE_ADJUSTED_ERROR")

i_prof_param=index_param[,3]

if ( length(which(!is.na(DOXY_ADJUSTED_ERROR)))>1 ) {

	NITRATE_ADJUSTED_ERROR[,i_prof_param]=ERROR+((approx(PRES,DOXY_ADJUSTED_ERROR,PRES[,i_prof_param],rule=1.2)$y)/10.)

} else {

	NITRATE_ADJUSTED_ERROR[,i_prof_param]=ERROR+0.1*abs(NITRATE[,i_prof_param]-NITRATE_ADJUSTED[,i_prof_param])

}

return(NITRATE_ADJUSTED_ERROR)

}
