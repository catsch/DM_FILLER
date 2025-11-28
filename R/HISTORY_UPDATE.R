#' history_update
#'
#' update the history section in the netcdf file
#'
#' @param filenc An object of class ncdf4 of argo file
#' @param param  argo parameter name (string)
#' @param i_prof_param profile index of the parameter in the nc file
#' @param step  "DM" adjustment report or "QC" Quality controle report
#' @param date_update update text with format yyyymmddHHMMSS
#' @param institution institution code following NERC R04 table
#'
#' @return update history section of ncfile
#'
#' @importFrom  ncdf4 ncvar_put
#' @importFrom  dmfiller software_version
#'
#' @export
#'
history_update <- function(filenc,
                           param,
                           i_prof_param,
                           step,
                           date_update,
                           institution = NULL) {

	# N_HISTORY
	N_HISTORY <- filenc$dim[["N_HISTORY"]]$len
	i_history <- N_HISTORY + 1

	###     HISTORY INSTITUTION
	##	We should ask for Data center for table 4 of the argo user's manual  CATSCHM
  if (is.na(institution) || nchar(institution) > 4) {
    message(paste("Institution size is greater than 4 characters,",
                  "tool doesn't report the mandating institution"))
    inst <- "    "
  }else if (is.null(institution)) {
    inst <- "    "
  }else {
    inst <- paste(c(institution,
                    rep(" ", 4 - nchar(institution))),
                  collapse = "")
  }
	HISTORY_INSTITUTION <- inst
	ncdf4::ncvar_put(filenc,
                   "HISTORY_INSTITUTION",
                   HISTORY_INSTITUTION,
                   start = c(1, i_prof_param, i_history),
                   count = c(4, 1, 1))

	###	HISTORY_STEP
  # following (step) https://vocab.nerc.ac.uk/collection/R12/current/ &
  #           (action) https://vocab.nerc.ac.uk/collection/R07/current/
  if (step == "DM") {
	  ###	Adjustment report
		HISTORY_STEP <- "ARSQ"
    HISTORY_ACTION="CV  "
  } else if (step == "QC") {
    HISTORY_STEP <- "ARSQ"
    HISTORY_ACTION="CF  "
  }
  ncdf4::ncvar_put(filenc,
                   "HISTORY_STEP",
                   HISTORY_STEP,
                   start = c(1, i_prof_param, i_history),
                   count = c(4, 1, 1))


	###     HISTORY SOFTWARE Delayed Mode Filler Tool ;-)
  HISTORY_SOFTWARE <- "DMFT"
	ncdf4::ncvar_put(filenc,
                   "HISTORY_SOFTWARE",
                   HISTORY_SOFTWARE,
                   start = c(1, i_prof_param, i_history),
                   count = c(4, 1, 1))

	###	HISTORY SOFTWARE RELEASE ;-) My first version !!
	HISTORY_SOFTWARE_RELEASE <- dmfiller::software_version()

	ncdf4::ncvar_put(filenc,
                   "HISTORY_SOFTWARE_RELEASE",
                   HISTORY_SOFTWARE_RELEASE,
                   start = c(1, i_prof_param, i_history),
                   count = c(4, 1, 1))

	###     HISTORY_DATE (Same as Date update)
	ncdf4::ncvar_put(filenc,
                   "HISTORY_DATE",
                   date_update,
                   start = c(1, i_prof_param, i_history),
                   count = c(14, 1, 1))

	### 	HISTORY_ACTION (Change Value CV - I don't know If I should also said CF)
	ncdf4::ncvar_put(filenc,
                   "HISTORY_ACTION",
                   HISTORY_ACTION,
                   start = c(1, i_prof_param, i_history),
                   count = c(4, 1, 1))

	###     HISTORY_PARAMETER
	HISTORY_PARAMETER <- param
	ncdf4::ncvar_put(filenc,
                   "HISTORY_PARAMETER",
                   HISTORY_PARAMETER,
                   start = c(1, i_prof_param, i_history),
                   count = c(64, 1, 1))
}