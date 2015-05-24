#' Classification of remote sensing data
#'
#'
#' @examples
#' not run:
#' calcAtmosCorr(filepath = "Name_of_Landsat_Metadata_File")

classifyData <- function(bands, training_sites){
  
  # Extract training sites and explanatory variables
  samples <- lapply(seq(nrow(training_sites)), function(x){
    data.frame(lc_id = training_sites$lc_id[x],
               lc_name = training_sites$lc_name[x],
               raster::extract(bands, training_sites[x, ]))
  })
  samples <- do.call("rbind", samples)
  
  # Remove highly correlated explanatory variables
  samples_cor <- samples[, c(-1:-2)]
  correlation <- cor(samples_cor)
  #   summary(correlation[upper.tri(correlation)])
  correlation_highly <- caret::findCorrelation(correlation, cutoff = .75)
  samples_cor <- samples_cor[,-correlation_highly]
  correlation2 <- cor(samples_cor)
  #   summary(correlation2[upper.tri(correlation2)])
  samples_final <- cbind(samples[, c(1:2)], samples_cor)
  
  # Split data in training and testing
  samples_model <- samples_final[, -1]
  
  # Adjust bands used for prediction
  bands_model <- bands[[which(names(bands) %in% colnames(samples_model))]]
  bands_values <- getValues(bands_model)
  
  # Validation
  accuracy <- lapply(seq(10), function(x){
    set.seed(x)
    index <- caret::createDataPartition(samples_model$lc_name, p = .8,
                                        list = FALSE,
                                        times = 1)
    samples_model[index, ]
    train <- samples_model[index, ]
    test  <- samples_model[-index, ]
    
    # Train the model
    model <- caret::train(lc_name ~ ., data = train, method = "rf")
    
    # Test the model
    prediction <- predict(model, newdata = test)
    reference <- test$lc_name
    caret::confusionMatrix(prediction, reference)$overall
  })
  accuracy <- do.call("rbind", accuracy)
  
  
  
  # Final model
  model <- caret::train(lc_name ~ ., data = samples_model, method = "rf")
  result <- predict(model, newdata = bands_values)
  result_raster <- setValues(bands[[1]], result)
  names(result_raster) <- "Landcover map"
  plot(result_raster)
  
  return(result_raster)
}
