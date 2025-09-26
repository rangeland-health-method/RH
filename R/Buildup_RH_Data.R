#' @title Build up RH Data
#' @description This function prepares input data for the evaluation criteria of rangeland health.
#' @details
#' The function takes canopy cover, soil organic carbon (OC), and plant species trait data, and returns standardized data.
#'
#' **Note:** The first row of the input data matrix (canopy_oc_data) must be the reference sample, and the second column must contain the OC data, and the canopy cover must be entered as a relative value.
#'
#' @param canopy_oc_data A data frame containing canopy cover and soil organic carbon data.
#' @param trait_data A data frame containing plant species trait data.
#' @return A standardized data frame for further analysis using Min-Max Normalization.
#' @md
#' @examples
#' data(canopy_oc_data)
#' data(trait_data)
#' final_data_st <- Buildup_RH_data(canopy_oc_data, trait_data)
#' @importFrom vegan specnumber
#' @importFrom vegan diversity
#' @importFrom FD dbFD
#' @import ade4
#' @import geometry
#' @import lattice
#' @import permute
#' @name Buildup_RH_data
#' @export
Buildup_RH_data <- function(canopy_oc_data, trait_data) {
  canopy.cover.edit <- as.data.frame(canopy_oc_data[, c(-1, -2)])
  trait <- as.data.frame(trait_data)
  OC <- as.numeric(canopy_oc_data[, 2])
  if (!is.data.frame(canopy_oc_data) || !is.data.frame(trait_data)) {
    stop("Error: Both canopy_oc_data and trait_data must be data frames.")
  }

  if (ncol(canopy_oc_data) < 2 || ncol(trait_data) < 2) {
    stop("Error: Data must be a matrix or data frame with at least two dimensions.")
  }

  Taxa_S <- specnumber(canopy.cover.edit)
  Shannon_H <- diversity(canopy.cover.edit, index = "shannon")

  abu <- canopy.cover.edit[, colSums(canopy.cover.edit) >= 0.01]

  # this temporary directory will be for the FD package to leave files in
  newdir <- tempfile("FDbug")
  stopifnot(dir.create(newdir))
  # make sure it disappears after we are done
  on.exit(unlink(newdir, recursive = TRUE), add = TRUE)
  # 'FD' creates files in current directory, so switch it
  olddir <- setwd(newdir)
  # make sure the current directory is restored after we are done
  on.exit(setwd(olddir), add = TRUE)
  # now call the function from the 'FD' package

  rownames(trait) <- trait$sp
  trait.edit <- trait[, c(2:5)]
  FD1 <- dbFD(trait.edit, abu, corr = "cailliez", calc.FRic = TRUE, calc.CWM = FALSE)
  FD2 <- as.data.frame(FD1)
  FD3 <- FD2[, c(3, 8)]

  trait.CWM <- trait[, c(2:6)]
  FD4 <- dbFD(trait.CWM, abu, corr = "cailliez", calc.FRic = TRUE, calc.FDiv = FALSE, CWM.type = "all")
  FD5 <- as.data.frame(FD4)
  FD6 <- FD5[, c(8:16)]

  canopy <- rowSums(canopy.cover.edit) * 100

  input.data <- cbind(Taxa_S, Shannon_H, FD3, FD6, canopy, OC)
  num_rows <- nrow(input.data)
  samples <- list()
  for (i in 2:num_rows) {
    samples[[paste0("sample_", i - 1)]] <- input.data[i, , drop = FALSE]
  }
  reference_sample <- input.data[1, , drop = FALSE]
  pmin_samples <- lapply(samples, function(sample) pmin(sample, reference_sample))
  pmin_mean_sample.total <- rbind(reference_sample, do.call(rbind, pmin_samples))

  standardized_all <- as.data.frame(apply(pmin_mean_sample.total, 2, function(x) (x - min(x)) / (max(x) - min(x))))

  final_data_st <- standardized_all[-1, ]

  return(final_data_st)
}
