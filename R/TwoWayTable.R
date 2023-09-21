#' Two-Way Table Analysis
#'
#' @param data A matrix representing the one-way table.
#' @param alpha The significance level for confidence intervals and chi-squared test.
#'
#' @return A list containing:
#'   \item{ContingencyTable}{The contingency table.}
#'   \item{ExpectedCounts}{The expected cell counts.}
#'   \item{ChiSquaredTest}{Results of the chi-squared test.}
#'   \item{TestStatistic}{The chi-squared test statistic.}
#'   \item{CriticalValue}{The critical value for chi-squared test.}
#'   \item{PValue}{The p-value for chi-squared test.}
#'   \item{RejectRegion}{The rejection region for chi-squared test.}
#'   \item{Conclusion}{The conclusion based on chi-squared test.}
#'   \item{Bar chart}{A bar chart representing the data distribution.}
#'   \item{Mosaicplot}{A Mosaicplot visualize the frequencies in a two way table.}
#'
#'
#' @export
#' @import graphics
#' @import stats
#' @import grDevices
#' @examples
#' conventional_accords <- c(50132 - 5364, 5364)
#' hybrid_accords <- c(1505 - 137, 137)
#' data <- matrix(c(conventional_accords, hybrid_accords), nrow = 2)
#' rownames(data) <- c("Conventional", "Hybrid")
#' colnames(data) <- c("No Injuries", "Injuries")
#' two_way_table(data, alpha = 0.05)
#'

two_way_table <- function(data, alpha = 0.05) {
  # Create a data frame from the matrix
  df <- as.data.frame(data)

  # Create a contingency table using xtabs
  contingency_table <- xtabs(~ ., data = df)

  # Calculate expected counts
  expected_counts <- chisq.test(contingency_table)$expected

  # Perform chi-square test
  chi_squared_result <- chisq.test(contingency_table)

  # Calculate test statistic
  test_statistic <- chi_squared_result$statistic

  # Calculate critical value
  critical_value <- qchisq(1 - alpha, df = chi_squared_result$parameter)

  # Calculate p-value
  p_value <- chi_squared_result$p.value

  # Define rejection region
  reject_region <- c(critical_value, Inf)

  # Determine conclusion
  if (test_statistic > critical_value) {
    conclusion <- "Reject the null hypothesis: There is a significant association between the variables."
  } else {
    conclusion <- "Fail to reject the null hypothesis: No significant association between the variables."
  }
  bar_chart = barplot(data, legend = TRUE,col=rainbow(4), beside = TRUE)
  mosaic = mosaicplot(data,col=rainbow(2))
  # Store the results in a list
  results <- list(
    "ContingencyTable" = contingency_table,
    "ExpectedCounts" = expected_counts,
    "ChiSquaredTest" = chi_squared_result,
    "TestStatistic" = test_statistic,
    "CriticalValue" = critical_value,
    "PValue" = p_value,
    "RejectRegion" = reject_region,
    "Conclusion" = conclusion,
    "bar_chart" = bar_chart,
    "mosaic" = mosaic

  )

  return(results)
}
