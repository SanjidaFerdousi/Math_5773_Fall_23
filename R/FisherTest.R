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
#' observed_table <- matrix(c(2, 8, 0, 0, 0, 1, 5, 3, 1, 0), nrow = 2)
#' rownames(observed_table) <- c("Smartbond", "Composite")
#' colnames(observed_table) <- c("1", "2", "3", "4", "5")
#' fisher_test_with_probability(observed_table)
fisher_test_with_probability <- function(observed_table) {
  # Perform Fisher's exact test
  fisher_result <- fisher.test(observed_table)

  # Calculate the probability using the hypergeometric formula
  total_population <- sum(observed_table)
  row_sum <- rowSums(observed_table)
  col_sum <- colSums(observed_table)

  # Calculate hypergeometric probability
  probability <- (choose(row_sum[1], observed_table[1, 1]) *
                    choose(row_sum[2], observed_table[2, 1])) /
    choose(total_population, sum(observed_table[, 1]))

  # Return results as a list
  results <- list(
    "FisherTest" = fisher_result,
    "Probability" = probability
  )

  return(results)
}

