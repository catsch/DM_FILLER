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
  LIST_nc <- liste_to_do$filename
  dm_manager <- liste_to_do$dm_manager
  orcid_manager <- liste_to_do$orcid_manager
  institution_manager <- liste_to_do$institution_manager
  dm_operator <- liste_to_do$dm_operator
  orcid_operator <- liste_to_do$orcid_operator
  institution_operator <- liste_to_do$institution_operator
  variable <- liste_to_do$param

  #### default ouput
  return_msge <- NULL
  is_here <- rep(0, length(LIST_nc)) # -1 no param or issue / 0 no dm / 1 dm
  position <- rep(1, length(LIST_nc))

  for (ifile in seq(1, length(LIST_nc))) {

    ##########################
    #### Reading the Bfile
    ##########################
    IDnc <- LIST_nc[ifile]
    filenc_out <- ncdf4::nc_open(IDnc, readunlim = FALSE, write = TRUE)

    att <- ncdf4::ncatt_get(filenc_out, 0)

    ##########################
    #### DM QC information
    ##########################
    #primary
    if (!is.na(dm_manager[ifile])) {
      is_ok <- lapply(names(att),
                      function(x) grepl("comment_dmqc_operator1", x))
      l_dmqc1 <- which(unlist(is_ok) == TRUE)
      if (any(unlist(is_ok))) {
        is_ok <- grepl("PRIMARY", toupper(att[l_dmqc1]))
        if (any(unlist(is_ok))) {
          is_ok <- grepl(toupper(dm_manager[ifile]), toupper(att[l_dmqc1]))
          if (!any(unlist(is_ok))) {
            msge <- paste("comment_dmqc_operator1 in the file is different",
                          "from information return by the user",
                          "- orcid info was not reported in this file")
            return_msge <- c(return_msge, paste(IDnc, msge, sep = " : "))
            is_here[ifile] <- -1
            next
          }
        }else {
          msge <- paste("comment_dmqc_operator1 must be the PRIMARY contact",
                        "point - orcid info was not reported in this file")
          return_msge <- c(return_msge, paste(IDnc, msge, sep = " : "))
          is_here[ifile] <- -1
          next
        }
      }else {
        comment_dmqc_operator1 <- paste("PRIMARY",
                                        orcid_manager[ifile],
                                        dm_manager[ifile],
                                        institution_manager[ifile],
                                        sep = " | ")
        #### adding information
        ncdf4::ncatt_put(filenc_out,
                         varid = 0,
                         "comment_dmqc_operator1",
                         comment_dmqc_operator1)
      }
    }

    # variable
    is_ok <- lapply(names(att), function(x) grepl("comment_dmqc_operator", x))
    l_dmqc <- which(unlist(is_ok) == TRUE)
    # it is already in the file
    is_ok <- grepl(toupper(variable[ifile]), toupper(att[l_dmqc]))
    nb <- lapply(names(att)[l_dmqc],
                 function(x) gsub("comment_dmqc_operator", "", x))
    nb <- as.numeric(unlist(nb))
    if (any(unlist(is_ok))) {
      is_here[ifile] <- 1
      position[ifile] <- nb[is_ok == TRUE]
    }else {
      nb <- max(nb)
      position[ifile] <- nb
    }

    # should be present in the file
    param_string <- stringr::str_pad(variable[ifile], 64, "right")
    parameter <- ncdf4::ncvar_get(filenc_out, "PARAMETER")

		index_param <- which(parameter == param_string, arr.ind = TRUE)
    if (length(index_param) == 0) {
      is_here[ifile] <- -1
    }
    ncdf4::nc_close(filenc_out)
  }

  for (ifile in seq(1, length(LIST_nc))) {
    if (!is.na(dm_operator[ifile])) {
      IDnc <- LIST_nc[ifile]
      filenc_out <- ncdf4::nc_open(IDnc, readunlim = FALSE, write = TRUE)

      if (any(is_here == 0)) {
        pos <- max(position[is_here == 0]) + 1
        message(pos)
      }else {
        pos <- position[ifile]
      }

      if (is_here[ifile] >= 0) {
        comment_dmqc_operator <- paste(toupper(variable[ifile]),
                                      orcid_operator[ifile],
                                      dm_operator[ifile],
                                      institution_operator[ifile],
                                      sep = " | ")

        ncdf4::ncatt_put(filenc_out,
                        varid = 0,
                        paste0("comment_dmqc_operator", pos),
                        comment_dmqc_operator)
        return_msge <- c(return_msge, paste(IDnc, "Done", sep = " : "))
      }
      ncdf4::nc_close(filenc_out)
    }
  }
  return(return_msge)
}
