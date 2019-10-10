test_that("Test whether as_character ", {

  A <- factor(c(0,7,3,10))
  B <- as_character(A)
  expect_equal(as.character(A),B)

  A <- data.frame(matrix( data = A, nrow = 2))
  C <- apply(A, MARGIN = 2, FUN = as.character)

  D <- as_character(A)
  expect_equal(A,D)


  E <- matrix(B, ncol = 2)
  colnames(E) <- c("X1","X2")
  G <-  as_character(C)
  expect_equal( E, G )


})
