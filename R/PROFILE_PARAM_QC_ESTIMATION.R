#' profile_param_qc_estimation
#'
#' calculate the profile_param_qc after a change
#'
#' @param filenc An object of class ncdf4 of argo file
#' @param param_qc matrix of parameter qc to be changes
#' @param profile_param_qc_name  name of parameter qc to be estimated
#' @param profile_param_qc  matrix of parameter qc to be estimated
#' @param n_qc
#' @param i_prof_param profile index of the parameter in the nc file
#'
#' @return update history section of ncfile
#'
#' @importFrom ncdf4 ncvar_put
#' @export
#'
profile_param_qc_estimation <- function(filenc,
                                        param_qc,
                                        profile_param_qc_name,
                                        profile_param_qc,
                                        n_qc,
                                        i_prof_param) {

  #### Definition
  # " " -> no QC performed
  # "A" -> N=100% All profile levels contain good data
  # "B" -> 75% <= N < 100%
  # "C" -> 50% <= N < 75%
  # "D" -> 25% <= N < 50%
  # "E" -> 0% <= N < 25%
  # "F" -> N=0%; no profile levels have good data

  # Initialisation
  n_good <- 0

  # Split the string to count
  qc <- unlist(strsplit(param_qc[i_prof_param], split = ""))

  n_qc_1 <- length(which(qc == "1"))
  n_qc_2 <- length(which(qc == "2"))
  n_qc_5 <- length(which(qc == "5"))
  n_qc_8 <- length(which(qc == "8"))


  if (n_qc != 0) {
    n_good <- 100 * (n_qc_1 + n_qc_2 + n_qc_5 + n_qc_8) / n_qc
  } else {
    n_qc_tot <- nchar(param_qc[i_prof_param])
    n_qc_9 <- length(which(qc == "9"))
    n_qc_b <- length(which(qc == " "))
    if (n_qc_tot == n_qc_9) n_good <- -99
    if (n_qc_tot == n_qc_9 + n_qc_b) n_good <- -99
  }

  if (n_good == -99) substr(profile_param_qc, i_prof_param, i_prof_param) <- " "

  if (n_good == 0) substr(profile_param_qc, i_prof_param, i_prof_param) <- "F"

  if (n_good > 0 && n_good < 25) substr(profile_param_qc, i_prof_param, i_prof_param) <- "E"

	if (n_good >= 25 && n_good < 50) substr(profile_param_qc, i_prof_param, i_prof_param) <- "D"

	if (n_good >= 50 && n_good < 75) substr(profile_param_qc, i_prof_param, i_prof_param) <- "C"

	if (n_good >= 75 && n_good < 100) substr(profile_param_qc, i_prof_param, i_prof_param) <- "B"

	if (n_good == 100) substr(profile_param_qc, i_prof_param, i_prof_param) <- "A"

	###    Enter the PROFILE_QC in the file
	ncdf4::ncvar_put(filenc, profile_param_qc_name, profile_param_qc)

}