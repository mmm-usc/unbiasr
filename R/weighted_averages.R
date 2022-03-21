# .weighted_average <- function(x, w) {
#   sum((x * w)) / sum(w)
# }

.weighted_average_list <- function(x, weights) {
  if (!is.list(x)) {
    stop("the input is not a list for computing weighted averages.")
  }
  stopifnot(length(x) == length(weights))
  Reduce("+", mapply("*", x, weights, SIMPLIFY = FALSE)) / sum(weights)
}