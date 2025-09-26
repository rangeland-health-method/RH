#' @title Rangeland Health
#' @description This function calculates attributes of rangeland health.
#' @details
#' The function takes standardized data, performs predictions using pre-trained models, and returns the results.
#' @param final_data_st A data frame containing standardized data from the first function.
#' @return The attributes of rangeland health.
#' @examples
#' data(canopy_oc_data)
#' data(trait_data)
#' final_data_st <- Buildup_RH_data(canopy_oc_data, trait_data)
#' evaluation.criteria <- Function_RH(final_data_st)
#' @importFrom stats predict
#' @importFrom randomForest randomForest
#' @name Function_RH
#' @export
Function_RH <- function(final_data_st) {
  model_info <- list(
    list(path = "extdata/Soil_Site_Stability.rds", name = "Soil.Site.Stability"),
    list(path = "extdata/Hydrologic_Function.rds", name = "Hydrologic.Function"),
    list(path = "extdata/Biotic_Integrity.rds", name = "Biotic.Integrity"),
    list(path = "extdata/Rangeland_Health.rds", name = "Rangeland.Health")
  )

  load_model <- function(model_path) {
    file_path <- system.file(model_path, package = "RH")
    model <- readRDS(file_path)
    if (!inherits(model, "randomForest")) {
      stop("Model is not a valid random forest object: ", model_path)
    }
    return(model)
  }

  run_predictions <- function(final_data_st) {
    predictions <- lapply(model_info, function(info) {
      model <- load_model(info$path)
      predict(model, newdata = final_data_st)
    })

    evaluation.criteria <- do.call(cbind, predictions)
    colnames(evaluation.criteria) <- sapply(model_info, function(x) x$name)
    return(evaluation.criteria)
  }

  predictions <- run_predictions(final_data_st)
  return(predictions)
}
