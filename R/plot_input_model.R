#' @title plot input model
#' @description This function creates graphs for the inputs of the Function_RH model.
#' @details
#' The function takes input data and specific row indices, creates sample charts, and returns the plots.
#' @param final_data_st A data frame containing standardized data from the first function.
#' @param row_indices A vector of row indices specifying which rows to use for creating sample plots.
#' @param plot_title_prefix A prefix for the plot titles (default is "Site").
#' @param ncol Number of columns for arranging the plots. Default is 3.
#' @return A list of sample charts.
#' @examples
#' data(canopy_oc_data)
#' data(trait_data)
#' final_data_st <- Buildup_RH_data(canopy_oc_data, trait_data)
#' row_indices <- 1:17
#' plot_input_model <- plot_input_model(
#'   final_data_st,
#'   row_indices,
#'   plot_title_prefix = "Site",
#'   ncol = 7,
#'   font_family = "Arial"
#' )
#' @importFrom ggplot2 ggplot aes geom_bar geom_text labs theme_minimal theme
#' @importFrom ggplot2 element_text element_blank element_line scale_x_continuous
#' @importFrom ggplot2 facet_wrap element_rect margin label_value
#' @importFrom dplyr bind_rows %>% case_when group_by mutate cur_group_id ungroup
#' @importFrom grDevices windowsFont windowsFonts
#' @name plot_input_model
#' @export
utils::globalVariables(c("Score", "Variable", "Panel", "Mean", "label_value"))

plot_input_model <- function(final_data_st, row_indices, plot_title_prefix = "Site", ncol = 6, font_family = "Arial") {
  ordered_names <- c(
    "S", "H'", "FRic", "RaoQ",
    "CWM LDW", "CWM H", "CWM SLA", "CWM LDMC",
    "CWM AF", "CWM AG", "CWM PF", "CWM PG", "CWM SH",
    "Canopy Cover", "SOM"
  )
  colnames(final_data_st) <- ordered_names

  plot_input_model <- list()
  panel_labels <- letters[1:length(row_indices)]

  combined_df <- data.frame()

  for (i in seq_along(row_indices)) {
    row_index <- row_indices[i]
    sample_data <- as.numeric(final_data_st[row_index, ])
    sample_df <- data.frame(
      Variable = ordered_names,
      Score = sample_data,
      Panel = panel_labels[i]
    )
    sample_df$Mean <- mean(sample_data)
    combined_df <- bind_rows(combined_df, sample_df)
  }

  combined_df$Variable <- factor(combined_df$Variable, levels = rev(unique(combined_df$Variable)))

  combined_df <- combined_df %>%
    group_by(Panel) %>%
    mutate(Panel = paste0(plot_title_prefix, " ", cur_group_id(), "\nmean: ", sprintf("%.2f", unique(Mean)))) %>%
    ungroup()

  combined_df$Panel <- factor(combined_df$Panel, levels = unique(combined_df$Panel))

  if (.Platform$OS.type == "windows") {
    windowsFonts(Arial = windowsFont(font_family))
    font_family <- "Arial"
  }

  g <- ggplot(combined_df, aes(x = Score, y = Variable, fill = Panel)) +
    geom_bar(stat = "identity", width = 0.7, position = "dodge") +
    geom_text(aes(x = 1, label = sprintf("%.2f", Score)),
      hjust = 0, color = "black", size = 4, family = font_family
    ) +
    scale_x_continuous(limits = c(0, 1.35), breaks = seq(0, 1, by = 0.25)) +
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
