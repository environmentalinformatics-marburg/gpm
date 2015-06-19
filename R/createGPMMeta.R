#' Create meta dataset
#'
#' @description
#' Create a meta dataset layer for a gpm object.
#' 
#' @param NONE
#'
#' @return NONE
#'
#' @export createGPMMeta
#' 
#' @details NONE
#' 
#' @references  NONE
#' 
#' @seealso NONE
#' 
#' @examples
#' data(abies_alba)
#' createGPMMeta(dataset = abies_alba, type = "input",
#'               selector = 1, response = c(16:481), meta = c(2: 15))
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