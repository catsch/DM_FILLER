#!/bin/bash
##################################################################################
## This script is a way to fill DM Argo nc Files
##    
##   It offers the possibility to change parameter into parameter adjusted 
##   taking into account a Slope, a Drift, an Offset 
## 
##   Designed by Catherine Schmechtig, July 2018
##################################################################################
clear
echo "##########################################"
echo "### Welcome in the DM mode filler Tool ###"
echo "##########################################" 

##################################################
### GET the DATE of the CORRECTION
##################################################
DATE_UPDATE=`date +%Y%m%d%H%M%S`

##################################################################################
### WMO and number 
##################################################################################
echo "What WMO nc files you want to fill with DM ?"
read id_WMO

# Empty 
> "DM_list_"$id_WMO

## Header
echo "filename;filename_core;metadata_filename;param;type;offset;slope;drift;N_CYCLE_BEGIN;param_error;qc;scientific_comment;date_update"  >> "DM_list_"$id_WMO


#######################################################################################
### A jouter quelquechose qui recupere l information des meta data  avec le READ_ARGO, 
### on peut afficher quels sont les parametres du fichier et du coup les proposer a la
### modification avec une liste "dynamique"  
#######################################################################################

echo "What is the parameter that interests you ?"
echo " Enter "
echo " 1 for CHLA"
echo " 2 for BBP700"
echo " 3 for NITRATE"
echo " 4 for CDOM"
echo " 5 for DOXY"
echo " 6 for PH_IN_SITU_TOTAL"
read id_PARAM

case "$id_PARAM" in
	"1")
		PARAM="CHLA"
		;;
	"2")
		PARAM="BBP700" 
		;;
	"3")
		PARAM="NITRATE"
		;;
	"4")
		PARAM="CDOM"
		;; 
	"5")
		PARAM="DOXY"
		;; 
	"6")
		PARAM="PH_IN_SITU_TOTAL"
		;;			
esac

################################################################################
### Going to the DATA directory and list the files that need to be filled
################################################################################ 
RT_DIR="../../DATA/RT/"$id_WMO"/profiles"
WORK_DIR="../../DATA/WORK/"$id_WMO"/profiles"
DM_DIR="../../DATA/DM/"$id_WMO"/profiles"

# Test the existence of the DM DIRECTORY
if [ ! -d "$DM_DIR" ] 
then
mkdir "../../DATA/DM/"$id_WMO
mkdir $DM_DIR
fi 

# Test the existence of the WORKING DIRECTORY
if [ ! -d "$WORK_DIR" ] 
then
mkdir "../../DATA/WORK/"$id_WMO 
else
echo "your working directory ../../DATA/WORK/"$id_WMO "already exists, you should remove it before continuing"
exit
fi

# To get some information on the DEPLOYMENT
metadata_file="../../DATA/RT/"$id_WMO"/"$id_WMO"_meta.nc"

# cp the file in a working directory
cp -fr $RT_DIR $WORK_DIR

num_RT=`ls -1 $WORK_DIR/B* | wc -l | awk '{print $1}'`

# Empty list file
>list_file

echo ""
echo "There are "$num_RT" B files to treat for "$id_WMO" float"
echo " How do you want to define the files to treat?"
echo " Enter "
echo " 0  -For All profiles (you have one adjustment for the whole float life)"
echo " 1  -You want to define precisely the profile slot step by step for adjustment or from greylist"
read id_LIST

case "$id_LIST" in
	"0")
		echo " You want to put the same adjustment on all the profiles"
		echo " Enter the Offset"
		read OFFSET
		echo " Enter the slope"
		read SLOPE
		echo " Enter the Drift"
		read DRIFT
		echo "How this correction will improve the QC after the adjustment?"
		echo " Enter"
		echo " 1 -If the ADJUSTED "$PARAM" should be considered as GOOD (QC=1)"
		echo " 2 -If the ADJUSTED "$PARAM" should be considered as PROBABLY GOOD (QC=2)"
		read QC
		echo " Enter the "$PARAM"_ADJUSTED_ERROR"
		read PARAM_ERROR 
		echo " Enter the SCIENTIFIC_CALIB_COMMENT, you want to write in the nc File [max 256 CHAR]"
		read SC_COMMENT

		for i in `ls -1 $WORK_DIR/B*`
		do
			# core file
			icore=`echo $i | sed s/"profiles\/B"/"profiles\/"/`
			if [ ! -e $icore ] 
				then  
				icore=`echo $icore | sed s/"profiles\/D"/"profiles\/R"/`
				if [ ! -e $icore ] 
					then 
					icore=`echo $icore | sed s/"profiles\/R"/"profiles\/D"/`
				fi
			fi
			echo $i";"$icore";"$metadata_file";"$PARAM";AD;"$OFFSET";"$SLOPE";"$DRIFT";1;"$PARAM_ERROR";"$QC";"$SC_COMMENT";"$DATE_UPDATE  >> "DM_list_"$id_WMO
		done  
		;;
	"1")
		echo "Here is the list of the files you can correct" 
		a=1		
		for i in `ls -1 $WORK_DIR/B*`
		do
			echo $a $i  >> list_file 
			((a+=1))
		done  
		column list_file
		echo " "
		echo "Enter your selection (use ; to separate your slot)" 
		echo "for example : 1-6;7;8-10"
		echo "	means that you have one correction for profile 1 to 6, another one for profile 7 and a final one for profile 8 to 10" 	
		read SELECTION

		##### Work on the Entry #####
		# Get the number of slots 
		n_slot=`echo $SELECTION | awk -F ";" '{print NF}'`
		
		#### Loop on every slot ####
		for (( i_slot = 1 ; i_slot <= $n_slot; i_slot +=1 ))
		do
			slot=`echo $SELECTION | awk -F ";" -v var="$i_slot" '{print $var}'`
			test=`echo $slot | grep -v "-"`
			
			if [ -z `echo $slot | grep -v "-"` ]
			then
				cut_car=`expr index "$slot" -`
				ideb=`echo $slot | cut -c -$cut_car | sed s/-//`
				ifin=`echo $slot | cut -c $cut_car- | sed s/-//`
				# echo $ideb $ifin
			else
				ideb=$slot
				ifin=$slot
			fi
			
			echo "Let's enter the correction for the slot "$i_slot": profile "$slot
			echo " Enter"
			echo " 0 -If you want to convert a Greylist to DM ( Fill_value and QC= 4)"
			echo " 1 -If you have an adjustment for the slot"
			read id_TYPE
			case "$id_TYPE" in
			"0")
				TYPE="GL"  # Greylist conversion	
				OFFSET="NA"
				SLOPE="NA"
				DRIFT="NA"
				N_CYCLE_BEGIN="NA"
				QC="4"
				PARAM_ERROR="NA"			
				;;
			"1")
				TYPE="AD"  # Adjustment for the slot	
				echo " Enter the Offset"
				read OFFSET
				echo " Enter the slope"
				read SLOPE
				echo " Enter the Drift"
				read DRIFT
				echo " Enter the cycle number of the beginning of DRIFT"
				read N_CYCLE_BEGIN
				echo "How this correction will improve the QC after the adjustment?"
				echo " Enter"
				echo " 1 -If the ADJUSTED "$PARAM" should be considered as GOOD (QC=1)"
				echo " 2 -If the ADJUSTED "$PARAM" should be considered as PROBABLY GOOD (QC=2)"
				read QC
				echo " Enter the "$PARAM"_ADJUSTED_ERROR"
				read PARAM_ERROR
				;;
			esac # end of GL or AD 			 
			echo " Enter the SCIENTIFIC_CALIB_COMMENT, you want to write in the nc File [max 256 CHAR]"
			read SC_COMMENT
			
			a=1
			for i in `ls -1 $WORK_DIR/B*`
			do
				# core file
				icore=`echo $i | sed s/"profiles\/B"/"profiles\/"/`
				if [ ! -e $icore ]
				 then  
					icore=`echo $icore | sed s/"profiles\/D"/"profiles\/R"/`
					if [ ! -e $icore ] 
						then 
						icore=`echo $icore | sed s/"profiles\/R"/"profiles\/D"/`
					fi
				fi
				if [ $a -ge $ideb ] &&  [ $a -le $ifin ] 
				then 				
				echo $i";"$icore";"$metadata_file";"$PARAM";"$TYPE";"$OFFSET";"$SLOPE";"$DRIFT";"$N_CYCLE_BEGIN";"$PARAM_ERROR";"$QC";"$SC_COMMENT";"$DATE_UPDATE  >> "DM_list_"$id_WMO
				fi
				((a+=1))
			done  # end loop on file to correct
		done  	# end loop on different slots 	
		;;
esac	# end case to specify or not different slots 

###############################################
# Write the launcher for the DM
###############################################
#echo "R DM_list_"$id_WMO "--vanilla < test_NCALIB_BP.R" > lance_DM_BP.sh
echo "R DM_list_"$id_WMO "--vanilla < WRITE_DM_BP.R" 	> lance_DM_BP.sh
echo "for i in \`ls -1 $WORK_DIR/B*\`"  		>> lance_DM_BP.sh
echo "do"						>> lance_DM_BP.sh
echo "j=\`echo \$i | sed s/BR/BD/ | sed s/WORK/DM/\`" 	>> lance_DM_BP.sh
echo  "cp \$i \$j"					>> lance_DM_BP.sh
echo  "done"						>> lance_DM_BP.sh
chmod +x lance_DM_BP.sh

################################################
# Close the input_file creation 
################################################
echo ""
echo "The DM input file is done, you can check it: DM_list_"$id_WMO
echo ""
echo "If it is Ok,"
echo "Enter on the command line" 
echo "./lance_DM_BP.sh"
echo "" 
echo "If it is not, go back ./to Fill_DM_BP.sh"
echo " or contact me :"
echo " schmechtig@obs-vlfr.fr"
echo ""









