test_that("Test whether as_factor works", {


  A <- c(0.7,0.5,3125134, 13)
  B <- factor(c( 0.7, 0.5, 3125134, 13 ))

  expect_equal(as_factor(A), B )

  A <- factor(c(0,7,3,10))
  B <- data.frame(matrix(A, ncol = 2))
  C <- as_factor(B)
  expect_equal(B, C)

})
