IPAR_15_DEPTH <- function ( PRES_CTD, PAR ) {

PAR_LIMIT=15

i_par=min(which(PAR<PAR_LIMIT))

ipar_15_depth=PRES_CTD[i_par]

return (ipar_15_depth)

}

