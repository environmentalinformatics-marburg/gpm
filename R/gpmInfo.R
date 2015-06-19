#' Get or access gpm object information used by various functions
#'
#' @description
#' Get information from class GPM
#' 
#' @param gpm GPM object (see \code{\link{gpm}})
#' 
#' @return Objects of respective type (see \code{\link{GPM}})
#'
#' @details The functions are generally self explaining in that sence that
#' \code{get*} returns the respective information and \code{set*} sets the
#' respective information from/in the GPM object.
#'  
#' \code{addGPMLog} adds a log entry to the GPM object
#' 
#' @name gpmInfo
#' 
#' @examples
#' data(abies_alba)
#' meta <- createGPMMeta(dataset = abies_alba, 
#'                       selector = 1, response = c(16:481), meta = c(2: 15))
#' alb <- gpm(abies_alba, meta)
#' 
NULL


################################################################################
# Return (almost) complete sections of the GPM object
################################################################################

# Return GPM data layers -------------------------------------------------------
#' @export getGPMDataLayers
#'
#' @rdname gpmInfo
#'
getGPMDataLayers <- function(GPM){
  return(GPM@data)
}


# Return GMP data layer i ------------------------------------------------
#' @export getGPMDataLayer
#'
#' @rdname gpmInfo
#'
getGPMDataLayer <- function(GPM, lcde){
  return(GPM@data[[getGPMLNBR(GPM, lcde)]])
}


# Return GMP object metadata ---------------------------------------------
#' @export getGPMMeta
#'
#' @rdname gpmInfo
#'
getGPMMeta <- function(GPM, lcde){
  if(missing(lcde)){
    return(GPM@meta)
  } else {
    return(GPM@meta[GPM@meta$lcde == lcde, ])
  }
}


# Return template for GMP object metadata which is based on existing band-
#' @export getGPMMetalcdeTemplate
#'
#' @rdname gpmInfo
#'
getGPMMetalcdeTemplate <- function(GPM, lcde){
  meta_template <- getGPMMeta(GPM, lcde)
  meta_template$DATE <- NULL
  meta_template$LAYER <- NULL
  meta_template$FILE <- NULL
  meta_template$METAFILE <- NULL
  return(meta_template)
}


# Return GMP object log info ---------------------------------------------
#' @export getGPMLog
#'
#' @rdname gpmInfo
#'
getGPMLog <- function(GPM){
  return(GPM@log)
}


################################################################################
# Add entries to the GMP object
################################################################################

# Add additional or overwrite metainformation parameter to GMP object ----
#' @export addGPMMetaParam
#'
#' @rdname gpmInfo
#'
addGPMMetaParam <- function(GPM, meta_param){
  id <- colnames(meta_param)[1]
  name <- colnames(meta_param)[2]
  # Parameter already exists: overwrite, otherwise add
  if(length(which(name == colnames(GPM@meta))) > 0){
    GPM@meta[, which(name == colnames(GPM@meta))] <- NULL
  } 
  GPM@meta <- merge(GPM@meta, meta_param, by = id, all.x = TRUE)
  GPM@meta <- GPM@meta[order(GPM@meta$LNBR),]
  return(GPM)
}


# Add metainformation for an additional layer to GMP object --------------
#' @export addGPMMetaEntry
#'
#' @rdname gpmInfo
#'
addGPMMetaEntry <- function(GPM, meta_param){
  if(missing(meta_param)){
    meta_param <- data.frame(DATE = as.POSIXlt(Sys.Date(), tz = "UTC"))
  }
  
  lnbr_new <- nrow(getGPMMeta(GPM)) + 1
  meta_param$LNBR <- lnbr_new
  
  if("DATE" %in% colnames(meta_param) == FALSE){
    meta_param$DATE <- as.POSIXlt(Sys.Date(), tz = "UTC")
  }
  
  if("LAYER" %in% colnames(meta_param) == FALSE){
    if(length(getGPMDataLayers(GPM)) == lnbr_new){
      meta_param$LAYER <- 
        getGPMLayerfromData(GPM, nbr = length(getGPMDataLayers(GPM)))
    } else {
      meta_param$LAYER <- paste0("Layer number ", lnbr_new)
    }
  }
  
  GPM@meta <- plyr::rbind.fill(getGPMMeta(GPM), meta_param)
  return(GPM)
}


# Add new log entry to GMP object ----------------------------------------
#' @export addGPMLog
#'
#' @rdname gpmInfo
#'
addGPMLog <- function(GPM, info = NA_character_, in_lcde = NA_character_, 
                      out_lcde = NA_character_){
  new_length <- length(getGPMLog(GPM)) + 1
  ps <- sprintf("ps%04d", new_length)
  GPM@log <- append(GPM@log, list(list(time = Sys.time(), info = info, 
                                       in_lcde = in_lcde, out_lcde = out_lcde)))
  names(GPM@log)[new_length] <- ps
  return(GPM)
}


# Add new GMP data layer --------------------------------------------------
#' @export addGPMDataLayer
#'
#' @rdname gpmInfo
#'
addGPMDataLayer <- function(GPM, lcde, data, meta_param, info, in_lcde){
  names(data) <- lcde
  GPM@data[[length(GPM@data) + 1]] <- data
  
  if(missing(meta_param)){
    meta_param = data.frame(lcde = lcde)
  } else {
    meta_param$lcde = lcde 
  }
  
  GPM <- addGPMMetaEntry(GPM, meta_param = meta_param)
  GPM <- addGPMLog(GPM, info = info, in_lcde = in_lcde, 
                   out_lcde = lcde)
  return(GPM)
}


################################################################################
# Return individual entries of the GMP object
################################################################################

# Return parameter (general method implemented by the specific functions below)-
#' @param lcde band code
#' @export getGPMParam
#'
#' @rdname gpmInfo
#' 
getGPMParam <- function(GPM, param, lcde, return_lcde = TRUE){
  if(length(which(param == colnames(getGPMMeta(GPM)))) > 0){
    if(param == "lcde"){
      return(getGPMMeta(GPM)[, which(param == colnames(getGPMMeta(GPM)))])
    } else {
      if(missing(lcde)){
        param <- getGPMMeta(GPM)[, which(param == colnames(getGPMMeta(GPM)))]
        lcde <- as.character(getGPMlcde(GPM))
      } else {
        param <- 
          getGPMMeta(GPM)[, 
                          which(param == colnames(getGPMMeta(GPM)))][match(
                            lcde, getGPMMeta(GPM)$lcde)]
        lcde <- as.character(lcde)
      }
      if(return_lcde == TRUE){
        attr(param, "names") <- lcde
      }
      return(param)
    }
  } else {
    return(NA_character_)  
  }
}


# Return Band code -------------------------------------------------------------
#' 
#' @export getGPMlcde
#'
#' @rdname gpmInfo
#' 
getGPMlcde <- function(GPM){
  getGPMParam(GPM, "lcde", return_lcde = FALSE)
}


# Return Band IDs --------------------------------------------------------------
#' 
#' @export getGPMBID
#'
#' @rdname gpmInfo
#' 
getGPMBID <- function(GPM, lcde){
  getGPMParam(GPM, "BID", lcde, return_lcde = FALSE)
}


# Return sensor ID -------------------------------------------------------------
#' @export getGPMSID
#'
#' @rdname gpmInfo
#' 
getGPMSID <- function(GPM){
  getGPMParam(GPM, "SID", return_lcde = FALSE)[1]
}


# Return sensor ----------------------------------------------------------------
#' @export getGPMSensor
#'
#' @rdname gpmInfo
#' 
getGPMSensor <- function(GPM){
  getGPMParam(GPM, "SENSOR", return_lcde = FALSE)[1]
}


# Return sensor group ----------------------------------------------------------
#' @export getGPMSensorGroup
#'
#' @rdname gpmInfo
#' 
getGPMSensorGroup <- function(GPM){
  getGPMParam(GPM, "SGRP", return_lcde = FALSE)[1]
}


# Return sensor information ----------------------------------------------------
#' @export getGPMSensorInfo
#'
#' @rdname gpmInfo
#' 
getGPMSensorInfo <- function(GPM){
  data.frame(SID = getGPMSID(GPM),
             SENSOR = getGPMSensor(GPM),
             SGRP = getGPMSensorGroup(GPM))
}


# Return spectrum --------------------------------------------------------------
#' @export getGPMSpectrum
#'
#' @rdname gpmInfo
#' 
getGPMSpectrum <- function(GPM, lcde){
  getGPMParam(GPM, "SPECTRUM", lcde)
}


# Return solar band codes ------------------------------------------------------
#' @export getGPMlcdeSolar
#'
#' @rdname gpmInfo
#' 
getGPMlcdeSolar <- function(GPM){
  spectrum <- getGPMSpectrum(GPM)
  return(getGPMlcde(GPM)[grep("solar", spectrum)])
}


# Return thermal band codes ------------------------------------------------------
#' @export getGPMlcdeThermal
#'
#' @rdname gpmInfo
#' 
getGPMlcdeThermal <- function(GPM){
  spectrum <- getGPMSpectrum(GPM)
  return(getGPMlcde(GPM)[grep("thermal", spectrum)])
}


# Return sensor resolution -----------------------------------------------------
#' @export getGPMRes
#'
#' @rdname gpmInfo
#' 
getGPMRes <- function(GPM, lcde){
  getGPMParam(GPM, "SRES", lcde)
}


# Return sensor type -----------------------------------------------------------
#' @export getGPMType
#'
#' @rdname gpmInfo
#' 
getGPMType <- function(GPM, lcde){
  getGPMParam(GPM, "TYPE", lcde)
}


# Return CALIB -----------------------------------------------------------------
#' @export getGPMCalib
#'
#' @rdname gpmInfo
#' 
getGPMCalib <- function(GPM, lcde){
  getGPMParam(GPM, "CALIB", lcde)
}


# Return TYPE band codes matching id ------------------------------------------
#' @export getGPMlcdeType
#'
#' @rdname gpmInfo
#' 
getGPMlcdeType <- function(GPM, lcde, id){
  type <- getGPMType(GPM, lcde)
  result <- getGPMlcde(GPM)[grep(id, type)]
  if(length(result) == 0){
    result = NA_character_
  }
  return(result)
}


# Return SRES band codes matching id ------------------------------------------
#' @export getGPMlcdeSres
#'
#' @rdname gpmInfo
#' 
getGPMlcdeSres <- function(GPM, lcde, id){
  sres <- getGPMRes(GPM, lcde)
  result <- getGPMlcde(GPM)[grep(id, sres)]
  if(length(result) == 0){
    result = NA_character_
  }
  return(result)
}


# Return CALIB band codes matching id ------------------------------------------
#' @export getGPMlcdeCalib
#'
#' @rdname gpmInfo
#' 
getGPMlcdeCalib <- function(GPM, lcde, id){
  calib <- getGPMCalib(GPM, lcde)
  result <- getGPMlcde(GPM)[grep(id, calib)]
  if(length(result) == 0){
    result = NA_character_
  }
  return(result)
}


# Return CALIB band codes machting id and are solare bands ---------------------
#' @export getGPMlcdeSolarCalib
#'
#' @rdname gpmInfo
#' 
getGPMlcdeSolarCalib <- function(GPM, lcde, id){
  calib <- getGPMlcdeCalib(GPM, lcde, id)
  result <- getGPMlcdeSolar(GPM)[getGPMlcdeSolar(GPM) %in% calib]
  if(length(result) == 0){
    result = NA_character_
  }
  return(result)
}


# Return CALIB band codes machting id and are thermal bands --------------------
#' @export getGPMlcdeThermalCalib
#'
#' @rdname gpmInfo
#' 
getGPMlcdeThermalCalib <- function(GPM, lcde, id){
  calib <- getGPMlcdeCalib(GPM, lcde, id)
  return(getGPMlcdeThermal(GPM)[getGPMlcdeThermal(GPM) %in% calib])
}


# Return band information ------------------------------------------------------
#' @export getGPMBandInfo
#'
#' @rdname gpmInfo
#' 
getGPMBandInfo <- function(GPM, lcde, return_calib = TRUE){
  if(return_calib){
    result <- data.frame(BID = getGPMBID(GPM, lcde),
                         SRES = getGPMRes(GPM, lcde),
                         TYPE = getGPMType(GPM, lcde),
                         SPECTRUM = getGPMSpectrum(GPM, lcde),
                         CALIB = getGPMCalib(GPM, lcde))
  } else {
    result <- data.frame(BID = getGPMBID(GPM, lcde),
                         SRES = getGPMRes(GPM, lcde),
                         TYPE = getGPMType(GPM, lcde),
                         SPECTRUM = getGPMSpectrum(GPM, lcde))
  }
  return(result)
}


# Return RAD_MAX ---------------------------------------------------------------
#' @export getGPMRadMax
#'
#' @rdname gpmInfo
#' 
getGPMRadMax <- function(GPM, lcde){
  getGPMParam(GPM, "RADMAX", lcde)
}


# Return RAD_MIN ---------------------------------------------------------------
#' @export getGPMRadMin
#'
#' @rdname gpmInfo
#' 
getGPMRadMin <- function(GPM, lcde){
  getGPMParam(GPM, "RADMIN", lcde)
}


# Return REF_MAX ---------------------------------------------------------------
#' @export getGPMRefMax
#'
#' @rdname gpmInfo
#' 
getGPMRefMax <- function(GPM, lcde){
  getGPMParam(GPM, "REFMAX", lcde)
}


# Return REF_MIN ---------------------------------------------------------------
#' @export getGPMRefMin
#'
#' @rdname gpmInfo
#' 
getGPMRefMin <- function(GPM, lcde){
  getGPMParam(GPM, "REFMIN", lcde)
}


# Return ESD -------------------------------------------------------------------
#' @export getGPMESD
#'
#' @rdname gpmInfo
#' 
getGPMESD <- function(GPM){
  getGPMParam(GPM, "ESD")[1]
}


# Return ESun ------------------------------------------------------------------
#' @export getGPMESUN
#'
#' @rdname gpmInfo
#' 
getGPMESUN <- function(GPM, lcde){
  getGPMParam(GPM, "ESUN", lcde)
}


# Return SZEN ------------------------------------------------------------------
#' @export getGPMSZEN
#'
#' @rdname gpmInfo
#' 
getGPMSZEN <- function(GPM, lcde){
  getGPMParam(GPM, "SZEN", lcde)
}


# Return SAZM ------------------------------------------------------------------
#' @export getGPMSAZM
#'
#' @rdname gpmInfo
#' 
getGPMSAZM <- function(GPM, lcde){
  getGPMParam(GPM, "SAZM", lcde)
}


# Return SELV ------------------------------------------------------------------
#' @export getGPMSELV
#'
#' @rdname gpmInfo
#' 
getGPMSELV <- function(GPM, lcde){
  getGPMParam(GPM, "SELV", lcde)
}

# Return Layer name from metadata ----------------------------------------------
#' @export getGPMMetaLayer
#'
#' @rdname gpmInfo
#' 
getGPMMetaLayer <- function(GPM, lcde){
  getGPMParam(GPM, "LAYER", lcde)
}


# Return Layer name from data layer --------------------------------------------
#' @export getGPMLayerfromData
#'
#' @rdname gpmInfo
#' 
getGPMLayerfromData <- function(GPM, lcde, nbr){
  if(missing(lcde)){
    layers <- sapply(getGPMDataLayers(GPM), function(x){
      names(x)
    })
    if(missing(nbr)){
      return(layers)
    } else {
      return(layers[nbr])
    }
  } else {
    names(getGPMDataLayer(GPM, lcde))
  }
}



# Return LNBR ------------------------------------------------------------------
#' @export getGPMLNBR
#'
#' @rdname gpmInfo
#' 
getGPMLNBR <- function(GPM, lcde){
  getGPMParam(GPM, "LNBR", lcde)
}


# Return LMin ------------------------------------------------------------------
#' @export getGPMLMIN
#'
#' @rdname gpmInfo
#' 
getGPMLMIN <- function(GPM, lcde){
  getGPMParam(GPM, "LMIN", lcde)
}


# Return LMAX ------------------------------------------------------------------
#' @export getGPMLMAX
#'
#' @rdname gpmInfo
#' 
getGPMLMAX <- function(GPM, lcde){
  getGPMParam(GPM, "LMAX", lcde)
}


# Return RADA ------------------------------------------------------------------
#' @export getGPMRADA
#'
#' @rdname gpmInfo
#' 
getGPMRADA <- function(GPM, lcde){
  getGPMParam(GPM, "RADA", lcde)
}


# Return RADM ------------------------------------------------------------------
#' @export getGPMRADM
#'
#' @rdname gpmInfo
#' 
getGPMRADM <- function(GPM, lcde){
  getGPMParam(GPM, "RADM", lcde)
}


# Return REFA ------------------------------------------------------------------
#' @export getGPMREFA
#'
#' @rdname gpmInfo
#' 
getGPMREFA <- function(GPM, lcde){
  getGPMParam(GPM, "REFA", lcde)
}


# Return REFM ------------------------------------------------------------------
#' @export getGPMREFM
#'
#' @rdname gpmInfo
#' 
getGPMREFM <- function(GPM, lcde){
  getGPMParam(GPM, "REFM", lcde)
}


# Return BTK1 ------------------------------------------------------------------
#' @export getGPMBTK1
#'
#' @rdname gpmInfo
#' 
getGPMBTK1 <- function(GPM, lcde){
  getGPMParam(GPM, "BTK1", lcde)
}


# Return BTK2 ------------------------------------------------------------------
#' @export getGPMBTK2
#'
#' @rdname gpmInfo
#' 
getGPMBTK2 <- function(GPM, lcde){
  getGPMParam(GPM, "BTK2", lcde)
}


# Return DATE ------------------------------------------------------------------
#' @export getGPMDATE
#'
#' @rdname gpmInfo
#' 
getGPMDATE <- function(GPM, lcde){
  getGPMParam(GPM, "DATE", lcde)
}


# Return PRAD ------------------------------------------------------------------
#' @export getGPMPRAD
#'
#' @rdname gpmInfo
#' 
getGPMPRAD <- function(GPM, lcde){
  getGPMParam(GPM, "PRAD", lcde)
}