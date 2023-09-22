#' One-Way Table Analysis
#'
#' @param data A matrix representing the one-way table.
#' @param alpha The significance level for confidence intervals and chi-squared test.
#'
#' @return A list containing:
#'   \item{CellProportions}{A matrix of cell proportions.}
#'   \item{ConfidenceIntervals}{A matrix of confidence intervals for each cell proportion.}
#'   \item{CellProportionDifferences}{A matrix of differences between cell proportions.}
#'   \item{ConfidenceIntervalsDifferences}{A matrix of confidence intervals for differences between cell proportions.}
#'   \item{ChiSquaredTest}{Results of the chi-squared goodness-of-fit test.}
#'   \item{BarChart}{A bar chart representing the data distribution.}
#'   \item{bar_chart_chq}{A bar chart representing the chi squared test observe and expected data.}
#'   \item{pie_chart}{A pie chart representing the data distribution of each category.}
#'   \item{Summary}{Summary statistics of the one-way table.}
#'
#' @export
#' @import graphics
#' @import stats
#' @import grDevices
#'
#' @examples
#' data <- matrix(c(59, 108, 82, 79), nrow = 1)
#' rownames(data) <- c("Frequency")
#' colnames(data) <- c("Agree Strongly", "Agree Somewhat", "Disagree Somewhat", "Disagree strongly")
#' data <- t(data)
#' rownames(data) <- c("Agree Strongly", "Agree Somewhat", "Disagree Somewhat", "Disagree strongly")
#' result <- one_way_table_analysis(data, alpha = 0.05)
#' print(result)

one_way_table_analysis <- function(data, alpha = 0.05) {
  n_total <- sum(data)
  n_rows <- nrow(data)
  n_cols <- ncol(data)

  # Initialize cis data frame with empty rows
  cis <- data.frame(Row = character(), Col = character(), Lower = numeric(), Upper = numeric(), stringsAsFactors = FALSE)
  cell_proportions <- matrix(NA, n_rows, n_cols)
  for (i in 1:n_rows) {
    for (j in 1:n_cols) {
      # Cell proportion
      p <- data[i, j] / n_total
      cell_proportions[i, j] <- p
      se <- sqrt(p * (1 - p) / n_total)
      z <- qnorm(1 - alpha / 2)
      lower <- p - z * se
      upper <- p + z * se
      # Create a new row in the cis data frame
      cis <- rbind(cis, c(rownames(data)[i], colnames(data)[j], lower, upper))
    }
  }

  # Confidence interval for differences
  cis_diff <- data.frame(Row1 = character(), Row2 = character(), Lower = numeric(), Upper = numeric(), stringsAsFactors = FALSE)
  cell_proportion_diffs <- matrix(NA, n_rows, n_rows)
  for (i in 1:(n_rows - 1)) {
    for (j in (i + 1):n_rows) {
      # Cell proportions for the two rows
      p_i <- sum(data[i, ]) / n_total
      p_j <- sum(data[j, ]) / n_total

      se_diff <- sqrt((p_i * (1 - p_i) / sum(data[i, ])) + (p_j * (1 - p_j) / sum(data[j, ])))

      z <- qnorm(1 - alpha / 2)
      lower_diff <- (p_i - p_j) - z * se_diff
      upper_diff <- (p_i - p_j) + z * se_diff
      # Create a new row in the cis_diff data frame
      cis_diff <- rbind(cis_diff, c(rownames(data)[i], rownames(data)[j], lower_diff, upper_diff))
      cell_proportion_diffs[i, j] <- p_i - p_j
    }
  }

  # Perform the chi-squared goodness-of-fit test
  chi_squared_test <- chisq.test(data)

  # Create a bar chart

  bar_chart <- barplot(data, col = rainbow(length(rownames(data))), beside = TRUE,
                       names.arg = rownames(data), # Use row names as x-axis labels
                       cex.names = 0.7, # Adjust the x-axis label font size
                       main = "Bar Chart of the data")
  #bar_chart <- barplot(data, col=rainbow(2), beside = TRUE, legend.text = rownames(data), args.legend = list(cex = 0.7))
  observed_counts <- chi_squared_test$observed
  expected_counts <- chi_squared_test$expected
  bar_chart_chq <- barplot(rbind(observed_counts, expected_counts),col = rainbow(2), beside = TRUE, names.arg = rownames(data),
          cex.names = 0.7,main = "Chi-Square Test Results")
  legend("topright", legend = c("Observed", "Expected"), fill = rainbow(2))

  # Create a pie chart
  pie_chart <- pie(data, labels = rownames(data), col = rainbow(4), main = "Pie Chart of the data")

  # Create a table summary
  data_summary <- summary(data)

  # Create a list with results
  results <- list(
    "Cell Proportions" = cell_proportions,
    "Confidence Intervals" = cis,
    "Cell Proportion Differences" = cell_proportion_diffs,
    "Confidence Intervals Differences" = cis_diff,
    "Chi Squared Test" = chi_squared_test,
    "Bar Chart" = bar_chart,
    "bar for chart chi square" = bar_chart_chq,
    "Pie Chart" = pie_chart,
    "Summary" = data_summary
  )

  return(results)
}
