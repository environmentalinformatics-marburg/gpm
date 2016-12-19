#' Create a meta information dataset in gpm format
#'
#' @description
#' Create a meta dataset layer for a gpm object.
#' 
#' @param dataset The name of the data frame variable used for initializing the 
#' gmp object
#' @param type The type of the dataset (e.g. input, output), defaults to "input"
#' @param selector An optional selector variable which can be used to control 
#' the sampling of subsets during model training
#' @param response The column ID of the response, i.e. dependent variable(s) in 
#' the dataset
#' @param independent The column ID of the predictor, i.e. independent 
#' variable(s) in the dataset
#' @param meta The column ID of variables in the dataset which contain only meta
#' information not relevant for the model training
#' 
#' @return A list in the appropriate format to be used as meta data layer in a
#' gpm object.
#'
#' @export createGPMMeta
#' 
#' @details The column ID information is transformed to column names in order to 
#' ensure integrity even if columns are deleted in a later stage.
#' 
#' @references  NONE
#' 
#' @seealso \code{\link{AAAgpmClasses}} for the gpm class.
#' 
#' @examples
#' \dontrun{
#' data(abies_alba)
#' createGPMMeta(dataset = abies_alba, type = "input",
#'               selector = 1, response = c(16:481), meta = c(2: 15))
#' }
#' 
createGPMMeta <- function(dataset, type = "input",
                          selector, response, independent, meta){
  if(!any(colnames(dataset) %in% selector)){
    selector = colnames(dataset)[selector]
  }
  if(!any(colnames(dataset) %in% response)){
    response = colnames(dataset)[response]
  }
  if(!any(colnames(dataset) %in% independent)){
    independent = colnames(dataset)[independent]
  }
  if(!any(colnames(dataset) %in% meta)){
    meta = colnames(dataset)[meta]
  }
  list(TYPE = type,
       SELECTOR = selector,
       RESPONSE = response,
       INDEPENDENT = independent, 
       META = meta)
}