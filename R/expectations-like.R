expect_like <- function(object, expected, dist = NULL, method = "osa",
                        info = NULL, label = NULL, 
                        expected_label = NULL) {
  if (is.null(label)) {
    label <- find_expr("object")
  }
  
  if (is.null(expected_label)) {
    expected_label <- find_expr("expected")
  }
  
  expect_that(object, is_like(expected, dist = dist, method = method, label = expected_label),
              info = info, label = label)
}

#' @importFrom stringdist stringdist
is_like <- function(expected, dist = NULL, method = "osa", label = NULL) {
  if (is.null(dist)) {
    dist <- round(nchar(expected) * 0.2)
  }
  if (is.null(label)) {
    label <- find_expr("expected")
  }
  
  function(actual) {
    act_dist <- stringdist(expected, actual)
    
    expectation(
      act_dist <= dist,
      paste0("not like ", label, ". ", method ," distance is ", act_dist, " (> ", dist, ")"),
      paste0("is like ", label)
    )
  }
}