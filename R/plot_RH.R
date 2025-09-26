#' @title Create Evaluation Criteria Plots
#' @description This function creates plots to compare evaluation criteria of rangeland health.
#' @details
#' The function takes the output from Function_RH and creates evaluation plots for different criteria.
#' @param evaluation.criteria A data frame containing standardized data from the first function.
#' @param selected_columns A vector of column indices specifying which criteria to plot. If NULL, all columns will be plotted.
#' @param ncol Number of columns for arranging the plots. Default is 4.
#' @param plot_RHC A function that takes standardized data and generates evaluation plots for different criteria of rangeland health.
#' @return A list of evaluation criteria(attributes) plots.
#' @examples
#' data(canopy_oc_data)
#' data(trait_data)
#' final_data_st <- Buildup_RH_data(canopy_oc_data, trait_data)
#' evaluation.criteria <- Function_RH(final_data_st)
#' # Plot all columns
#' plots_all <- plot_RH(evaluation.criteria, ncol = 4)
#' # Plot specific columns
#' selected_columns <- c(1, 3)
#' plots_selected <- plot_RH(evaluation.criteria, selected_columns, ncol = 2)
#' @importFrom ggplot2 ggplot aes geom_bar geom_text labs theme_minimal theme
#' @importFrom ggplot2 element_text element_blank element_line margin
#' @importFrom ggplot2 scale_x_continuous facet_wrap element_rect label_value expand_limits
#' @importFrom dplyr bind_rows %>% case_when group_by mutate ungroup
#' @importFrom grDevices windowsFont windowsFonts
#' @name plot_RH
#' @export
utils::globalVariables(c("Panel", "Mean", "label_value"))

plot_RH <- function(evaluation.criteria, selected_columns = NULL, ncol = 4, font_family = "Arial") {
  if (is.null(selected_columns)) {
    selected_columns <- 1:4
  }

  evaluation.criteria.t <- as.matrix(t(evaluation.criteria))
  variable_names <- colnames(evaluation.criteria.t)

  combined_df <- data.frame()

  for (i in selected_columns) {
    scores <- as.numeric(evaluation.criteria.t[i, ])
    df <- data.frame(
      Variable = variable_names,
      Score = scores,
      Panel = as.character(i)
    )
    combined_df <- bind_rows(combined_df, df)
  }

  combined_df <- combined_df %>%
    mutate(Panel = case_when(
      Panel == "1" ~ "Soil/Site Stability",
      Panel == "2" ~ "Hydrologic Function",
      Panel == "3" ~ "Biotic Integrity",
      Panel == "4" ~ "Rangeland Health"
    ))

  combined_df$Panel <- factor(combined_df$Panel, levels = c(
    "Soil/Site Stability", "Hydrologic Function", "Biotic Integrity", "Rangeland Health"
  ))

  combined_df$Variable <- factor(combined_df$Variable, levels = rev(variable_names))

  if (.Platform$OS.type == "windows") {
    windowsFonts(Arial = windowsFont(font_family))
    font_family <- "Arial"
  }

  g <- ggplot(combined_df, aes(x = Score, y = Variable, fill = Panel)) +
    geom_bar(stat = "identity", width = 0.7, position = "dodge") +
    geom_text(aes(x = 1, label = sprintf("%.2f", Score)),
      hjust = 0, color = "black", size = 3.5, family = font_family
    ) +
    scale_x_continuous(limits = c(0, 1.15), breaks = seq(0, 1, by = 0.25)) +
    facet_wrap(~Panel, ncol = ncol, scales = "fixed", labeller = label_value) +
    labs(x = "", y = "", title = "") +
    theme_minimal() +
    theme(
      axis.text.y = element_text(family = font_family, size = 10, color = "black", hjust = 0),
      axis.text.x = element_blank(),
      axis.title.x = element_text(family = font_family, margin = margin(t = 5), face = "bold"),
      axis.title.y = element_text(family = font_family, face = "bold"),
      panel.grid = element_blank(),
      axis.line = element_line(color = "gray"),
      legend.position = "none",
      strip.text = element_text(family = font_family, face = "bold", size = 10),
      strip.background = element_rect(fill = "white", color = NA)
    )

  return(g)
}
