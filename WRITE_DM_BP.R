#######################################################################################
#	This program is designed to modify a BR file into BD file
#       
#       Catherine Schmechtig 2021 February 	
#
#	V1.0 20190503 : Initial version	
#	V1.1 20190517 : robust to CTD
#       V1.2 20190628 : OFFSET for DOXY out of drift 
#	V1.3 20200325 : Correct an issue on QC index
#	V2.0 20200515 : Add Break points in the drift estimation
#	V2.1 20200819 : Add HISTORY PARAMETER
#	V2.2 20210219 : Estimate NITRATE_ADJUSTED_ERROR from DOXY_ADJUSTED_ERROR 	      
#
#	-With an estimation of PARAM_ADJUSTED with a drift, a slope, an offset and some Break points 	
#	-Change the QC
#	-Check DATA_MODE (If not already set to D)
# 	-Change the DATA_STATE_INDICATOR (Table 6 - Argo user Manual )
#	-Change the PARAMETER_DATA_MODE
#	-Calculate the PROFILE_PARAM_QC 
#	-Change the HISTORY section ( + Increase )
#		-HISTORY_INSTITUTION (Table 4)
#		-HISTORY_STEP (Table 12)
#		-HISTORY_DATE
#		-HISTORY_SOFTWARE (if applicable)
#		-HISTORY_SOFTWARE_RELEASE (if applicable)		
#		-HISTORY_ACTION (Table 7) 
#		-HISTORY_PARAMETER
#					
#	-Change the SCIENTIFIC_CALIB_xxx Section
#		-SCIENTIFIC_CALIB_COMMENT
#		-SCIENTIFIC_CALIB_COEFFICIENT
#		-SCIENTIFIC_CALIB_EQUATION
#		-SCIENTIFIC_CALIB_DATE
#		
#	-Inputs : 
#		output of the script ./Fill_DM_BP.sh
#		Bfiles
#	-Output :
#		Bfiles with a change in DM for one parameter 				        
########################################################################################

library(ncdf4)
library(stringr)
require(oce)

source("./READ_CTD.R")
source("./PPOX_to_DOXY.R")
source("./DOXY_to_PPOX.R")
source("./DOXY_adj.R")
source("./NITRATE_ERROR_ESTIMATION.R")

uf=commandArgs()

###################################################################################
# READ the INPUT FILE 
###################################################################################

input_file  <- uf[2]

input=read.table(file=input_file,header=TRUE,sep=";")

# header
# filename;filename_core;metadata_file;param;type;offset;slope;drift;N_CYCLE_BEGIN;param_error;qc;scientific_comment;date_update

# List of the file to process
LIST_nc=input$filename

# List of the associated core file
LIST_nc_core=input$filename_core

# Build the Variable Name 
PARAM_name=input$param

# Get the type of correction (AD, Adjustment - GL, Grey list) 
CORRECTION_TYPE=as.character(input$type)

# Get the correction values
OFFSET=input$offset

SLOPE=input$slope

DRIFT=input$drift 

N_CYCLE_BEGIN=input$N_CYCLE_BEGIN

# Get the error
ERROR=input$param_error

#get the new value of the QC
PARAM_ADJUSTED_QC_value=input$qc

# Get the scientific Comment
scientific_comment=input$scientific_comment

# Get the date of the correction
date_update=as.character(input$date_update)

# Get the launch date to calculate the Drift (between the launch date and the date of the profile) 
metadatafile=as.character(input$metadata_file)

#####################################################################################
# Loop on all the files in the List
#####################################################################################
for (i in seq(1,length(LIST_nc))) {

	print(i)

	FLAG_CTD=TRUE

	IDnc=as.character(LIST_nc[i])

	IDnc_core=as.character(LIST_nc_core[i])

	# Open the B file 
	filenc=nc_open(IDnc,readunlim=FALSE,write=TRUE)

	# Open the metadata file
	filenc_meta=nc_open(metadatafile[i],readunlim=FALSE,write=FALSE)

	# Open the core file
	filenc_core=nc_open(IDnc_core,readunlim=FALSE,write=FALSE) 

###################################################################################
#### Work on variable Name
###################################################################################

	PARAM_ADJUSTED_name=paste(PARAM_name[i],"_ADJUSTED",sep="")

	PARAM_ADJUSTED_QC_name=paste(PARAM_ADJUSTED_name,"_QC",sep="")

	PARAM_QC_name=paste(PARAM_name[i],"_QC",sep="")

	PARAM_ADJUSTED_ERROR_name=paste(PARAM_ADJUSTED_name,"_ERROR",sep="")

	PARAM_STRING=str_pad(PARAM_name[i],64,"right")

	PROFILE_PARAM_QC_name=paste("PROFILE_",PARAM_name[i],"_QC",sep="")

###################################################################################
####    Read the file
###################################################################################

	PARAMETER=ncvar_get(filenc,"PARAMETER")

	index_param=which(PARAMETER == PARAM_STRING , arr.ind=TRUE)

###	Very IMPORTANT
###	Next iteration if the parameter is not in the file 

	if ( length(index_param)==0 ) {
	next
	}


	PARAM=ncvar_get(filenc,as.character(PARAM_name[i]))

	PARAM_ADJUSTED=ncvar_get(filenc,as.character(PARAM_ADJUSTED_name))

	PARAM_ADJUSTED_ERROR=ncvar_get(filenc,as.character(PARAM_ADJUSTED_ERROR_name))

	PROFILE_PARAM_QC=ncvar_get(filenc,PROFILE_PARAM_QC_name)

	PARAMETER_DATA_MODE=ncvar_get(filenc,"PARAMETER_DATA_MODE")


	if ( (PARAM_name[i] == "DOXY") | (CORRECTION_TYPE[i] == "GL") ) {

		PARAM_ADJUSTED_QC=ncvar_get(filenc,PARAM_QC_name)

	} else {

		PARAM_ADJUSTED_QC=ncvar_get(filenc,PARAM_ADJUSTED_QC_name)

	}

####################################################################################
# PARAM_ADJUSTED Estimation
####################################################################################
	# Please Check that we should work on Raw data and not on Adjusted Data but it could be an issue for quenching and factor 2 CATSCHM

	if ( CORRECTION_TYPE[i] == "AD" ) {

###     For the Drift calculation we have to work on date 
###	First some Work on the date

	# get the Launch date in the metadata file 

		launch_date=ncvar_get(filenc_meta,"LAUNCH_DATE")

		launch_date_juld=as.numeric(julian(as.POSIXlt(launch_date,format="%Y%m%d%H%M%S",origin="1950-01-01"),origin="1950-01-01",TZ="GMT"))

	# get the actual date in the profile file 

		profile_date_juld=unique(ncvar_get(filenc,"JULD"))

	# For DOXY we need to go through the adjustment of PPOX_DOXY 

		if ( PARAM_name[i] == "DOXY" ) {


				DOXY_ADJ=DOXY_adj(filenc_core, filenc, PARAM_name[i] ,OFFSET[i], SLOPE[i], DRIFT[i], ERROR[i], profile_date_juld ,launch_date_juld )

				PARAM_ADJUSTED=DOXY_ADJ$DOXY

				PARAM_ADJUSTED_ERROR=DOXY_ADJ$DOXY_ERROR

				FLAG_CTD=DOXY_ADJ$FLAG_CTD

		} else {	

			if ( PARAM_name[i] == "NITRATE" ) {

				PARAM_ADJUSTED=as.numeric((DRIFT[i]/100.*(profile_date_juld-launch_date_juld)/365.))+(SLOPE[i]*PARAM+OFFSET[i])

				PARAM_ADJUSTED_ERROR=NITRATE_ERROR_ESTIMATION(filenc,PARAM,PARAM_ADJUSTED,ERROR[i],index_param)


			} else {

				PARAM_ADJUSTED=as.numeric((DRIFT[i]/100.*(profile_date_juld-launch_date_juld)/365.))+(SLOPE[i]*PARAM+OFFSET[i])

				PARAM_ADJUSTED_ERROR=replace(PARAM,!is.na(PARAM),ERROR[i])
			}
		}

	} else {	

		PARAM_ADJUSTED=rep(NA,length(PARAM_ADJUSTED))

		PARAM_ADJUSTED_ERROR=rep(NA,length(PARAM_ADJUSTED))

	}


####################################################################################
#### Work on index of the correction 
#### We should get information from PARAMETER which is the list of PARAMETER 
#### with CALIBRATION information 
####################################################################################
	
	for ( irow in seq(1,nrow(index_param))) {

		i_param_param =index_param[irow,1]
		
		i_calib_param =index_param[irow,2]	# Check how N-CALIB increases CATSCHM

		i_prof_param = index_param[irow,3]

		index_scientific=append(index_param[irow,],1,after=0) # build the index to write the scientific_calib_comment at the right place 


####################################################################################
# PARAM_ADJUSTED_QC MODIFICATION
####################################################################################
        ## Check how many QC should be set/change

	if ((i_prof_param == 1) && nrow(index_param) == 1 ) { # pourquoi j ai cette condition nrow(index_param)==1 ????

		N_QC= length(which(!is.na(PARAM)))
		
		index_qc=which(!is.na(PARAM))

	} else {
       
		N_QC=length(which(!is.na(PARAM[,i_prof_param])))

		index_qc=which(!is.na(PARAM[,i_prof_param]))

	}

	if ( N_QC > 0 ) { 

	if ( CORRECTION_TYPE[i] == "AD" ) {

		for (i_qc in seq(1,N_QC)) {

			j=index_qc[i_qc]

			QC_test=substr(PARAM_ADJUSTED_QC[i_prof_param], j, j)

			if ( PARAM_name[i] != "NITRATE" ) {

				if ( ( QC_test == "3") || ( QC_test == "2") || (QC_test == " ") || ( QC_test == "1") ) { # To check

					substr(PARAM_ADJUSTED_QC[i_prof_param], j , j)<- as.character(PARAM_ADJUSTED_QC_value[i])
				}

			} else {

                                if ( ( QC_test == "2") || (QC_test == " ") || ( QC_test == "1") ) { # To check

                                        substr(PARAM_ADJUSTED_QC[i_prof_param], j , j)<- as.character(PARAM_ADJUSTED_QC_value[i])
                                }

                        }

			if ( ( QC_test == "4" ) || ( PARAM_ADJUSTED_QC_value[i] == "4" ) ) {  ## Fill value for QC=4 and QC=9

				if (i_prof_param == 1 && nrow(index_param) == 1 ) {

					PARAM_ADJUSTED[j]=NA

					PARAM_ADJUSTED_ERROR[j]=NA
				
				} else {

					PARAM_ADJUSTED[j,i_prof_param]=NA

					PARAM_ADJUSTED_ERROR[j,i_prof_param]=NA
				}

			}

		}

	} else {

		for (i_qc in seq(1,N_QC)) {

			j=index_qc[i_qc]

			substr(PARAM_ADJUSTED_QC[i_prof_param], j , j) <- as.character(PARAM_ADJUSTED_QC_value[i])
		}

	}

	}


####################################################################################
# SCIENTIFIC_CALIB_SECTION
####################################################################################

#	scientific calib comment
	SCIENTIFIC_CALIB_COMMENT=str_pad(scientific_comment[i],256,"right")

	if (!FLAG_CTD) SCIENTIFIC_CALIB_COMMENT=str_pad("no adjustment is performed because of issues in CTD",256,"right")

	ncvar_put(filenc,"SCIENTIFIC_CALIB_COMMENT",SCIENTIFIC_CALIB_COMMENT,start=index_scientific,count=c(256,1,1,1))

# 	scientific calib Coefficient and equation 
	if ( CORRECTION_TYPE[i] == "AD") {

		scientific_coefficient=paste("OFFSET=",OFFSET[i],", SLOPE=",SLOPE[i],", DRIFT=",DRIFT[i],", DRIFT_DAY_BEGIN=",N_CYCLE_BEGIN[i])

		if ( PARAM_name[i] == "DOXY" ) {

			scientific_equation=paste("PPOX_ADJUSTED=OFFSET+(PPOX*SLOPE)*(1+DRIFT/100.*(profile_date_juld-launch_date_juld)/365.)")

			if (!FLAG_CTD) {

				scientific_coefficient="none"

				scientific_equation="none"

			}

		} else {

			scientific_equation=paste(PARAM_ADJUSTED_name,"=(",PARAM_name,"*SLOPE+OFFSET)+(DRIFT/100.*(profile_date_juld-launch_date_juld)/365.)")

		}

	} else {

		scientific_coefficient="none"

		scientific_equation="none"
		
	}

	SCIENTIFIC_CALIB_COEFFICIENT=str_pad(scientific_coefficient,256,"right")

	ncvar_put(filenc,"SCIENTIFIC_CALIB_COEFFICIENT",SCIENTIFIC_CALIB_COEFFICIENT,start=index_scientific,count=c(256,1,1,1))

	SCIENTIFIC_CALIB_EQUATION=str_pad(scientific_equation,256,"right")

	ncvar_put(filenc,"SCIENTIFIC_CALIB_EQUATION",SCIENTIFIC_CALIB_EQUATION,start=index_scientific,count=c(256,1,1,1))

#	Scientific calib date
	ncvar_put(filenc,"SCIENTIFIC_CALIB_DATE",date_update[i],start=index_scientific,count=c(14,1,1,1))


#####################################################################################
# History Section
#####################################################################################
	# N_HISTORY
	N_HISTORY=filenc$dim[['N_HISTORY']]$len
	i_history=N_HISTORY+1

###     HISTORY INSTITUTION
##	We should ask for Data center for table 4 of the argo user's manual (VF for VilleFranche ?) CATSCHM
	HISTORY_INSTITUTION="VF  "
	ncvar_put(filenc,"HISTORY_INSTITUTION",HISTORY_INSTITUTION,start=c(1,i_prof_param,i_history),count=c(4,1,1))

###	HISTORY_STEP	
###	Delayed mode code
	HISTORY_STEP="ARSQ"
	ncvar_put(filenc,"HISTORY_STEP",HISTORY_STEP,start=c(1,i_prof_param,i_history),count=c(4,1,1))

###     HISTORY SOFTWARE Delayed Mode Filler Tool ;-)
	HISTORY_SOFTWARE="DMFT"
	ncvar_put(filenc,"HISTORY_SOFTWARE",HISTORY_SOFTWARE,start=c(1,i_prof_param,i_history),count=c(4,1,1))

###	HISTORY SOFTWARE RELEASE ;-) My first version !!
	HISTORY_SOFTWARE_RELEASE="V2.2"
	ncvar_put(filenc,"HISTORY_SOFTWARE_RELEASE",HISTORY_SOFTWARE_RELEASE,start=c(1,i_prof_param,i_history),count=c(4,1,1))

###     HISTORY_DATE (Same as Date update) 
	ncvar_put(filenc,"HISTORY_DATE",date_update[i],start=c(1,i_prof_param,i_history),count=c(14,1,1))

### 	HISTORY_ACTION (Change Value CV - I don't know If I should also said CF)
	HISTORY_ACTION="CV  "
	ncvar_put(filenc,"HISTORY_ACTION",HISTORY_ACTION,start=c(1,i_prof_param,i_history),count=c(4,1,1))

###     HISTORY_PARAMETER 
        HISTORY_PARAMETER=PARAM_STRING
        ncvar_put(filenc,"HISTORY_PARAMETER",HISTORY_PARAMETER,start=c(1,i_prof_param,i_history),count=c(64,1,1))

#####################################################################################
# PROFILE_PARAM_QC Calculation 
#####################################################################################
#### Definition
# " " -> no QC performed
# "A" -> N=100% All profile levels contain good data
# "B" -> 75% <= N < 100%
# "C" -> 50% <= N < 75%
# "D" -> 25% <= N < 50%
# "E" -> 0% <= N < 25%
# "F" -> N=0%; no profile levels have good data

# Initialisation
	N_good=0
 
# Split the string to count 
	QC=unlist(strsplit(PARAM_ADJUSTED_QC[i_prof_param],split=""))

	N_QC_1=length(which(QC == "1"))
	N_QC_2=length(which(QC == "2"))
	N_QC_5=length(which(QC == "5"))
	N_QC_8=length(which(QC == "8"))


	if ( N_QC != 0) {
 
		N_good=100 * ( N_QC_1 + N_QC_2 + N_QC_5 + N_QC_8 ) / N_QC

	} else {

		N_QC_tot=nchar(PARAM_ADJUSTED_QC[i_prof_param])		
		N_QC_9=length(which(QC == "9"))
		N_QC_B=length(which(QC == " "))
		if ( N_QC_tot == N_QC_9 ) N_good = -99
	 	if ( N_QC_tot == N_QC_9+N_QC_B ) N_good = -99

	}

	
	if ( N_good == -99) substr(PROFILE_PARAM_QC,i_prof_param,i_prof_param) <-" "

	if ( N_good == 0) substr(PROFILE_PARAM_QC,i_prof_param,i_prof_param) <-"F"

	if ( N_good > 0 && N_good < 25 ) substr(PROFILE_PARAM_QC,i_prof_param,i_prof_param) <-"E"

	if ( N_good >= 25 && N_good < 50 ) substr(PROFILE_PARAM_QC,i_prof_param,i_prof_param) <-"D"

	if ( N_good >= 50 && N_good < 75 ) substr(PROFILE_PARAM_QC,i_prof_param,i_prof_param) <-"C"

	if ( N_good >= 75 && N_good < 100 ) substr(PROFILE_PARAM_QC,i_prof_param,i_prof_param) <-"B"

	if ( N_good == 100 ) substr(PROFILE_PARAM_QC,i_prof_param,i_prof_param) <-"A"


#####################################################################################
# DATA_MODE / PARAMETER_DATA_MODE / DATA_STATE_INDICATOR
#####################################################################################
# What I don't like is the fact that DATA_STATE_INDICATOR and DATA_MODE 
# are linked to i_prof_param while we are not doing D for CDOM, BBP700 and CHLA 
# at the same time CATSCHM
#####################################################################################

	# We perform DM QC on the profile i_prof so DATA_MODE should be set to D
	# For the good i_prof 
	ncvar_put(filenc,"DATA_MODE","D",start=c(i_prof_param),count=c(1))

	substr(PARAMETER_DATA_MODE[i_prof_param],i_param_param,i_param_param)<-"D"	

	# We perform DM QC on the profile i_prof but it still can be improved 
	# so we should put DATA_STATE_INDICATOR as 2C for i_prof
	 ncvar_put(filenc,"DATA_STATE_INDICATOR","2C  ",start=c(1,i_prof_param),count=c(4,1))


	} # End loop on irow 

###     Enter the Adjusted _QC in the file 

	ncvar_put(filenc,PARAM_ADJUSTED_QC_name,PARAM_ADJUSTED_QC)

###	Enter the Adjustment in the File (We move this here for isolated FLAG "4" set to NA)
	
	ncvar_put(filenc,PARAM_ADJUSTED_name,PARAM_ADJUSTED)

	ncvar_put(filenc,PARAM_ADJUSTED_ERROR_name,PARAM_ADJUSTED_ERROR)

###    Enter the PROFILE_QC in the file 

	ncvar_put(filenc,PROFILE_PARAM_QC_name,PROFILE_PARAM_QC)

###     ENTER the PARAMETER_DATA_MODE	

	ncvar_put(filenc,"PARAMETER_DATA_MODE",PARAMETER_DATA_MODE)

#####################################################################################
# DATE UPDATE 
#####################################################################################

###     DATE_UPDATE 
	ncvar_put(filenc,"DATE_UPDATE",date_update[i])


######### Close the Files 
	

	nc_close(filenc)

	nc_close(filenc_meta)

	nc_close(filenc_core)

} # end loop on file 
