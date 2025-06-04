#' BBP700_ERROR_ESTIMATION
#'
#' This function estimates BPP700 error according to
#'  https://archimer.ifremer.fr/doc/00491/60262/
#' @param filenc An object of class ncdf4 of argo bfile
#' @param BBP700_ADJUSTED vector of adjusted bbp700 values (numeric)
#' @param ERROR vector of bbp700 error values (numeric)
#' @param scale_BBP700 factory calibration (scale factor) (numeric)
#'
#' @return vector of adjusted bbp700 error values (numeric)
#' @importFrom ncdf4 ncvar_get
#' @export
#'
BBP700_ERROR_ESTIMATION <- function (filenc,
                                     BBP700_ADJUSTED,
                                     ERROR,
                                     scale_BBP700) {

BBP700_ADJUSTED_ERROR=ncdf4::ncvar_get(filenc, "BBP700_ADJUSTED_ERROR")

ERROR_TEMP=abs(ERROR*BBP700_ADJUSTED)

ERROR_TEMP[which(ERROR_TEMP<3*scale_BBP700)]=3*scale_BBP700

BBP700_ADJUSTED_ERROR=ERROR_TEMP

return(BBP700_ADJUSTED_ERROR)

}
