#' add_orcid <- function(input_file) {
#'
#' This function add information regarding the dm operator
#'
#' @param input_file file including full path of all Bfile for which
#'  DM operator information will be added in the file
#'
#' @return Bfile including DM operator information
#'
#' @importFrom ncdf4 nc_open ncatt_put nc_close
#'
#' @export
add_orcid <- function(input_file) {

  #### Creating the list of files for which we need to recompute
  liste_to_do <- read.table(input_file, header = TRUE, sep = ";")

  # List of the file to process
  list_nc <- liste_to_do$filename

  # information to be filled
  dm_manager <- liste_to_do$dm_manager
  orcid_manager <- liste_to_do$orcid_manager
  institution_manager <- liste_to_do$institution_manager
  dm_operator <- liste_to_do$dm_operator
  orcid_operator <- liste_to_do$orcid_operator
  institution_operator <- liste_to_do$institution_operator
  variable <- liste_to_do$param

  #### default ouput - status of the report
  return_msge <- NULL
  
  #### working variable
  # variable status : -1 no param
  #                    0 no dm is performed for the variable
  #                    1 dm is made for the variable
  status <- rep(0, length(list_nc))

  # DM position is status = 1
  dm_pos <- rep(0, length(list_nc))

  # max number of dm operation already done
  dm_max <- rep(1, length(list_nc))

  # first loop on file
  # for primary information and getting dm variable information
  for (ifile in seq(1, length(list_nc))) {

    ##########################
    #### Reading the Bfile
    ##########################
    id_nc <- list_nc[ifile]
    filenc_out <- ncdf4::nc_open(id_nc, readunlim = FALSE, write = TRUE)

    att <- ncdf4::ncatt_get(filenc_out, 0)

    ##########################
    #### DM QC information
    ##########################
    #primary
    if (!is.na(dm_manager[ifile])) {
      # availability checking
      is_ok <- lapply(names(att),
                      function(x) grepl("comment_dmqc_operator1", x))
      l_dmqc1 <- which(unlist(is_ok) == TRUE)
      if (any(unlist(is_ok))) {
        # yes it is
        is_ok <- grepl("PRIMARY", toupper(att[l_dmqc1]))
        if (any(unlist(is_ok))) {
          # information checking
          is_ok <- grepl(toupper(dm_manager[ifile]), toupper(att[l_dmqc1]))
          if (!any(unlist(is_ok))) {
            # yes it is
            msge <- paste("comment_dmqc_operator1 in the file is different",
                          "from information return by the user",
                          "- orcid info was not reported in this file")
            return_msge <- c(return_msge, paste(id_nc, msge, sep = " : "))
            status[ifile] <- -1
            next
          }
        }else {
          # no, isn't it
          msge <- paste("comment_dmqc_operator1 must be the PRIMARY contact",
                        "point - orcid info was not reported in this file")
          return_msge <- c(return_msge, paste(id_nc, msge, sep = " : "))
          status[ifile] <- -1
          next
        }
      }else {
        #### adding information
        comment_dmqc_operator1 <- paste("PRIMARY",
                                        orcid_manager[ifile],
                                        dm_manager[ifile],
                                        institution_manager[ifile],
                                        sep = " | ")

        ncdf4::ncatt_put(filenc_out,
                         varid = 0,
                         "comment_dmqc_operator1",
                         comment_dmqc_operator1)
      }
    }

    # variable availability (has dm already been made?)
    is_ok <- lapply(names(att), function(x) grepl("comment_dmqc_operator", x))
    # A faire que comment_dmqc ope est présent dans le fichier ====
    if (any(unlist(is_ok))) {
      message("youhou")
      l_dmqc <- which(unlist(is_ok) == TRUE)
      is_ok <- grepl(toupper(variable[ifile]), toupper(att[l_dmqc]))
      # its dm_pos
      nb <- lapply(names(att)[l_dmqc],
                   function(x) gsub("comment_dmqc_operator", "", x))
      nb <- as.numeric(unlist(nb))

      if (any(unlist(is_ok))) {
        status[ifile] <- 1
        dm_pos[ifile] <- nb[is_ok == TRUE]
      }
      dm_max[ifile] <- max(nb)
    }

    # is the parameter in the file
    param_string <- stringr::str_pad(variable[ifile], 64, "right")
    parameter <- ncdf4::ncvar_get(filenc_out, "PARAMETER")

		index_param <- which(parameter == param_string, arr.ind = TRUE)
    if (length(index_param) == 0) {
      status[ifile] <- -1
    }
    ncdf4::nc_close(filenc_out)
  }

  for (ifile in seq(1, length(list_nc))) {
    msge <- NULL
    if (!is.na(dm_operator[ifile])) {

      if (any(status == 0)) {
        pos <- max(dm_max[status == 0]) + 1
      }else {
        pos <- dm_pos[ifile]
      }

      if (any(status == 0) && any(status == 1) && status[ifile] == 1) {

        # delete old position
        filenc_out <- RNetCDF::open.nc(id_nc, write = TRUE)
        attribute <- paste0("comment_dmqc_operator", dm_pos[ifile])
        RNetCDF::att.delete.nc(filenc_out, "NC_GLOBAL", attribute)
        RNetCDF::close.nc(filenc_out)
        msge <- paste("RM", attribute)
      }

      if (status[ifile] >= 0) {
        # add dmoperator information
        id_nc <- list_nc[ifile]
        filenc_out <- ncdf4::nc_open(id_nc, readunlim = FALSE, write = TRUE)

        comment_dmqc_operator <- paste(toupper(variable[ifile]),
                                       orcid_operator[ifile],
                                       dm_operator[ifile],
                                       institution_operator[ifile],
                                       sep = " | ")

        ncdf4::ncatt_put(filenc_out,
                         varid = 0,
                         paste0("comment_dmqc_operator", pos),
                         comment_dmqc_operator)
        ncdf4::nc_close(filenc_out)
        msge <- paste(msge,
                      paste("ADD", paste0("comment_dmqc_operator", pos)))
      }
      return_msge <- c(return_msge, paste(id_nc, msge, sep = " : "))
    }
  }
  return(return_msge)
}
