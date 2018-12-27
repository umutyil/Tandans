#' @title Predicts problematic items for a given list of items
#'
#' @description This package predicts the problematic items of a higher education assesment system
#'
#' @param items
#'
#' @return List
#'
#' @examples problemDetection(items)
#'
#' @export

problemDetection <- function(items) {
  print(items)
  items$z <- with(items, 80 - c)
  items$k <- with(items, a * b * d * z)
  print(items$k)
  zz <- items[items$k>0,][,7]
  print(zz)
  summation <- sum(zz)
  print(summation)
  items$t <- with(items, k / summation)
  print(items)
}
