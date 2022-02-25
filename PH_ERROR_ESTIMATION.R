PH_ERROR_ESTIMATION <- function (filenc,PH_IN_SITU_TOTAL,PH_IN_SITU_TOTAL_ADJUSTED,ERROR,index_param,FLAG_CTD) {

PRES=ncvar_get(filenc,"PRES")

DOXY_ADJUSTED_ERROR=ncvar_get(filenc,"DOXY_ADJUSTED_ERROR")

PH_IN_SITU_TOTAL_ADJUSTED_ERROR=ncvar_get(filenc,"PH_IN_SITU_TOTAL_ADJUSTED_ERROR")

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
