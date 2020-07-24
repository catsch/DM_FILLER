MLD <- function ( PRES_CTD, PSAL_CTD , TEMP_CTD ) {

require(oce)

MLD_LIMIT=0.03

THETA=swTheta(PSAL_CTD,TEMP_CTD,PRES_CTD)

POTDENS=swSigmaTheta(PSAL_CTD,TEMP_CTD,PRES_CTD)

FLAG_BAD_POTDENS=rep(FALSE,length(PRES_CTD))

for(i in 2 : length(PRES_CTD)) {

	TEST=POTDENS[i]-POTDENS[i-1]

	if(!is.na(TEST)) {

        if( TEST<=-0.03) FLAG_BAD_POTDENS[i-1]=TRUE

	}
}

POTDENS[which(FLAG_BAD_POTDENS==TRUE)]=NA

PRES_CTD[is.na(POTDENS)]=NA

TEMP_CTD[is.na(POTDENS)]=NA

PSAL_CTD[is.na(POTDENS)]=NA

abs_pres_10=abs(PRES_CTD-10)

POTDENS_10=max(POTDENS[which(abs_pres_10==min(abs_pres_10,na.rm=TRUE))])

# index pour trouver la profondeur de la MLD
# on initialise au max de profondeur au cas ou on ne trouverait pas de MLD

MLD=max(PRES_CTD)

MLD_CALC=(POTDENS-POTDENS_10)

i_MLD=min(which(MLD_CALC>MLD_LIMIT))

MLD=PRES_CTD[i_MLD]

return(MLD)

}

