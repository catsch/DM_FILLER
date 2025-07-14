#' REPORT_QC
#'
#' This program is designed to change RAW QC
#'
#' @param input_file text file including all usefull information
#'  for the adjustment separated by ";"
#'   filename             full path of the B file
#'   var_level            name of the level variable
#'   level_value          level value
#'   param                name of the variable for wich _QC should be changed
#'   old_value            old QC param value
#'   new_value            new QC param value
#'   corr_value           corresponding param value
#'   date_update          date update yyyymmddhhmmss
#'   mandating_institution institution that mandates que DMQC
#'
#' @return Bfile with param_qc and profile_param_qc changed
#' @importFrom ncdf4 nc_open ncvar_get ncvar_put nc_close
#' @importFrom stringr str_pad
#' @importFrom dmfiller  history_update profile_param_qc_estimation
#' @export
REPORT_QC <- function(input_file) {

  # default output value
  count <- 0
  msge <- NULL

  # Read input file
  input <- read.table(file = input_file, header = TRUE, sep = ";")

  # List of the file to process
  list_nc <- input$filename

  # Build the Variable Name
  param_name <- input$param

  # old, new qc value and corresponding value
  old_value <- input$old_value
  new_value <- input$new_value
  corr_value <- input$corr_value

  # pressure level
  pres_name <- input$var_level
  pres_value <- input$level_value

  # code mandating institution
  inst <- input$mandating_institution

  # update date
  date_update <- input$date_update
  # Loop on all the files in the List
  for (i in seq(1, length(list_nc))) {
    print(i)

    # initialisation
    report <- 0

    # Open the B file
    idnc <- as.character(list_nc[i])
    if (file.exists(idnc)) {
      filenc <- ncdf4::nc_open(idnc, readunlim = FALSE, write = TRUE)

      # work on variable name
      param_qc_name <- paste(param_name[i], "_QC", sep = "")
      param_string <- stringr::str_pad(param_name[i], 64, "right")
      profile_param_qc_name <- paste("PROFILE_", param_name[i], "_QC", sep = "")

      # read the file
      parameter <- ncdf4::ncvar_get(filenc, "PARAMETER")
      index_param <- which(parameter == param_string, arr.ind = TRUE)

      if (length(index_param) == 0) { # if parameter is not in the file
        add_msge <- paste("Cy.", i, "no report as no", param_string)
        msge <- paste(msge, add_msge, sep = " / ")
        next
      }

      param <- ncdf4::ncvar_get(filenc, as.character(param_name[i]))
      param_qc <- ncdf4::ncvar_get(filenc, as.character(param_qc_name))
      profile_param_qc <- ncdf4::ncvar_get(filenc, profile_param_qc_name)
      pres <- ncdf4::ncvar_get(filenc, as.character(pres_name[i]))

      #loop on index
      nrow <- length(index_param[, 3])
      for (irow in seq(1, nrow)) {

        # profile indexes
        i_prof_param <- index_param[irow, 3]

        # profiles definition
        if (i_prof_param == 1) {
          if (filenc$dim$N_LEVELS$len == 1)  {	# prof one or one level
            v_param <- param[1]
            v_pres <- pres[1]
          } else {
            if (nrow(index_param) == 1) {
              v_param <- param
              v_pres <- pres
            } else {
              # prof one different levels
              v_param <- param[, i_prof_param]
              v_pres <- pres[, i_prof_param]
            }
          }
        }else {
          if (filenc$dim$N_LEVELS$len == 1)  {  # prof one or one level
            v_param <- param[i_prof_param]
            v_pres <- pres[i_prof_param]
          } else {
            # prof one different levels
            v_param <- param[, i_prof_param]
            v_pres <- pres[, i_prof_param]
          }
        }

        # get level where qc to be changed
        dec <- admisc::numdec(pres_value[i])
        index_level <- which(round(v_pres, dec) == pres_value[i])
        if (length(index_level) > 0) {
          # check old value correspondances
          qc <- param_qc[i_prof_param]
          old <- as.numeric(unlist(strsplit(qc, "")))[index_level]
          index_level <- index_level[old == old_value[i]]
          if (length(index_level) > 0) {
            # check the corresponding param value if possible
            if (!is.na(as.numeric(corr_value[i]))) {
              dec <- admisc::numdec(corr_value[i])
              param_value <- round(v_param[index_level], dec)
              index_level_tmp <- index_level[param_value == corr_value[i]]
              if (length(index_level_tmp) > 0) {
                index_level <- index_level_tmp
              }
            }
          }else if (length(index_level) == 0
                    && irow == nrow
                    && report == 0) {
            add_msge <- paste("line", i, "no report as no qc correspondance")
            msge <- paste(msge, add_msge, sep = " / ")
          }
        }else if (length(index_level) == 0
                  && irow == nrow
                  && report == 0) {
          add_msge <- paste("line", i, "no report as no level identification")
          msge <- paste(msge, add_msge, sep = " / ")
        }

        if (length(index_level) > 0) {
          report <- report + 1
          nb <- length(index_level)
          # change qc
          for (j in seq(1, nb)) {
            substr(param_qc[i_prof_param], index_level[j], index_level[j]) <-
              as.character(new_value[i])
            ncdf4::ncvar_put(filenc, param_qc_name, param_qc)

            # PROFILE_PARAM_QC Calculation
            n_qc <- length(which(!is.na(v_param)))
            dmfiller::profile_param_qc_estimation(filenc,
                                                  param_qc,
                                                  profile_param_qc_name,
                                                  profile_param_qc,
                                                  n_qc,
                                                  i_prof_param)
          }

          if (nb > 1) {
            add_msge <- paste("line",
                              i,
                              "prof",
                              j,
                              nb,
                              "reports for the same level")
            msge <- paste(msge, add_msge, sep = " / ")
          }
        }
      }

      if (report > 0) {
        count <- count + 1
      }
      # History Section
      dmfiller::history_update(filenc,
                               param_string, index_param[3],
                              "QC", date_update[i], inst[i])

      # DATE_UPDATE
      ncdf4::ncvar_put(filenc, "DATE_UPDATE", date_update[i])

      # close file
      ncdf4::nc_close(filenc)
    }else {
      add_msge <- paste("line", i, "no report as no Bfiles")
      msge <- paste(msge, add_msge, sep = " / ")
    }
  }
  # output
  final_msge <- paste(count, "reports on ", length(list_nc))
  if (!is.null(msge)) {
    final_msge <- paste(final_msge, "but", msge)
  }

  return(final_msge)
}