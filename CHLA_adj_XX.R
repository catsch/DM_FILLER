CHLA_adj_XX <- function ( filenc_core, filenc, PARAM_name, OFFSET, SLOPE, ERROR ){

FLAG_BAD=FALSE

#### READ Core file 

CTD=read_CTD(filenc_core)

# we get        : CTD$PRES
#               : CTD$PSAL
#               : CTD$TEMP

#### READ BFILE

CHLA=ncvar_get(filenc,as.character(PARAM_name))

CHLA_QC=ncvar_get(filenc,"CHLA_QC")

DOWNWELLING_PAR=ncvar_get(filenc,"DOWNWELLING_PAR")

PRES=ncvar_get(filenc,"PRES")

### Are there any good data ?

QC=unlist(strsplit(CHLA_QC,split=""))

if( length(which(QC !="4" & QC != " ")) == 0 ) FLAG_BAD = TRUE

### Correct the CHLA from the Dark Count

CHLA_DC=CHLA-OFFSET

CHLA_ADJUSTED=CHLA_DC

#### Estimation of the MLD 

MLD=MLD(CTD$PRES, CTD$PSAL , CTD$TEMP)

#### Estimation of ipar15

ipar_15_depth=IPAR_15_DEPTH(PRES,DOWNWELLING_PAR)

#### Estimate NPQ inputs 

CHLA_CHLA=CHLA_DC[!is.na(CHLA)]

PRES_CHLA=PRES[!is.na(CHLA)]

# small filter to remove spikes 

MED_CHLA=RunningFilter(2,CHLA_CHLA,na.fill=T, ends.fill=T, Method="Median")

# what are the values of CHLA and Depth in the quenching zone

if (length(which(PRES_CHLA<min(ipar_15_depth,MLD))) != 0 ) {

	CHLA_X12=MED_CHLA[which(PRES_CHLA<min(ipar_15_depth,MLD))]

	DEPTH_X12=PRES_CHLA[which(PRES_CHLA<min(ipar_15_depth,MLD))]

	# Estimate the quenching parameters 

	CHLA_NPQ=unique(max(CHLA_X12))

	DEPTH_NPQ=DEPTH_X12[which.max(CHLA_X12)]

	INDEX_NPQ=max(which(CHLA_X12==CHLA_NPQ))

	if ( is.integer(INDEX_NPQ) && length(INDEX_NPQ) != 0 && FLAG_BAD != TRUE ) FLAG_QUENCHING=TRUE

	# building the CHLA_ADJUSTED profile

	CHLA_ADJUSTED[which(!is.na(CHLA) & PRES<=DEPTH_NPQ)]=CHLA_NPQ

} else {

	FLAG_QUENCHING=FALSE

	INDEX_NPQ=0

}

### Multiply by slope 		

CHLA_ADJUSTED=SLOPE*CHLA_ADJUSTED

# Estimating the Error

ERROR_CHLA=0.25*abs(CHLA_ADJUSTED-CHLA/2.)+ERROR

result=(list(CHLA=CHLA_ADJUSTED,CHLA_ERROR=ERROR_CHLA,CHLA_INDEX_NPQ=INDEX_NPQ,FLAG_QUENCHING=FLAG_QUENCHING))

return(result)


}
