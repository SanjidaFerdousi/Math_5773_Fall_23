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
#'   \item{ConfidenceIntervals}{A matrix containing confidence intervals for each cell in the contingency table..}
#'   \item{Bar chart}{A bar chart representing the data distribution.}
#'   \item{Mosaicplot}{A Mosaicplot visualize the frequencies in a two way table.}
#'
#'
#' @export
#' @import graphics
#' @import stats
#' @import grDevices
#' @examples
#'
#' dt <- matrix(c(50132 - 5364,1505 - 137, 5364,137), nrow = 2)
#' rownames(dt) <- c("Conventional", "Hybrid")
#' colnames(dt) <- c("No Injuries", "Injuries")
#' data <- addmargins(dt)
#' two_way_table(data, alpha = 0.05)
#'

two_way_table <- function(data, alpha = 0.05) {

  contingency_table <- data

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

  # Calculating the confidence interval will exclude the last row and column as i will add addmargins later on
  n_rows <- nrow(data)-1
  n_cols <- ncol(data)-1
  lower_bounds <- matrix(NA, n_rows, n_cols)
  upper_bounds <- matrix(NA, n_rows, n_cols)

  for (i in 1:n_rows) {
    for (j in 1:n_cols) {
      p1 <- data[i, j] / sum(data[i, 1:n_cols])
      p2 <- data[i, n_cols + 1] / sum(data[n_rows + 1, 1:n_cols])
      n1 <- sum(data[i, 1:n_cols])
      n2 <- sum(data[n_rows + 1, 1:n_cols])

      se <- sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)
      z <- qnorm(1 - 0.05 / 2)
      margin_error <- z * se
      lower_bound <- (p1 - p2) - margin_error
      upper_bound <- (p1 - p2) + margin_error
      lower_bounds[i, j] <- lower_bound
      upper_bounds[i, j] <- upper_bound
    }
  }

  # Store lower and upper bounds in a list
  confidence_intervals <- list("LowerBounds" = lower_bounds, "UpperBounds" = upper_bounds)



  bar_chart = barplot(data[-nrow(data), -ncol(data)], legend = TRUE,col=rainbow(2), beside = TRUE,main = "Bar plot of the data")
  mosaic = mosaicplot(data[-nrow(data), -ncol(data)],col=rainbow(4),main = "Mosaic Plot of the data")
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
    "ConfidenceIntervals" = confidence_intervals,
    "bar_chart" = bar_chart,
    "mosaic" = mosaic

  )

  return(results)
}
