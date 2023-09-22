#' Fisher's Exact Test Analysis
#'
#' @param observed_table A contingency table (2x2) of observed counts.
#'
#' @return A list containing:
#'   \item{FisherTest}{Results of Fisher's exact test.}
#'   \item{Probability}{The probability of the observed table.}
#' @export
#'
#' @examples
#' observed_table <- matrix(c(2, 1, 8, 5, 0, 3, 0, 1, 0, 0), nrow = 2)
#' rownames(observed_table) <- c("Smartbond", "Composite")
#' colnames(observed_table) <- c("1", "2", "3", "4", "5")
#' fisher_test_with_probability(observed_table)

fisher_test_with_probability <- function(observed_table) {
  # Perform Fisher's exact test
  fisher_result <- fisher.test(observed_table)

  # Calculate the probability using the hypergeometric formula
  total_population <- sum(observed_table)
  row_sums <- rowSums(observed_table)
  col_sums <- colSums(observed_table)

  # Initialize probability to 1
  probability <- 1

  for (i in 1:length(row_sums)) {
    for (j in 1:length(col_sums)) {
      if (observed_table[i, j] > 0) {
        probability <- probability * choose(row_sums[i], observed_table[i, j]) *
          choose((total_population - row_sums[i]), (col_sums[j] - observed_table[i, j])) /
          choose(total_population, col_sums[j])
      }
    }
  }

  # Return results as a list
  results <- list(
    "FisherTest" = fisher_result,
    "Probability" = probability
  )

  return(results)
}
