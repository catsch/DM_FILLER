CHLA_ERROR_ESTIMATION <- function (filenc,CHLA,CHLA_ADJUSTED,ERROR,index_param) {

PRES=ncvar_get(filenc,"PRES")

CHLA_ADJUSTED_ERROR=ncvar_get(filenc,"CHLA_ADJUSTED_ERROR")

i_prof_param=index_param[,3]

CHLA_ADJUSTED_ERROR[,i_prof_param]=max(0.02,abs(ERROR*CHLA_ADJUSTED[,i_prof_param))

return(CHLA_ADJUSTED_ERROR)

}
