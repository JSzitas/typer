test_that("The typer operator works", {

  A <- c("0,6",16,"fly","0,66","0.4")
  B <- as_numeric(A)
  A_2 <- c(0.6, 16, NA, 0.66, 0.4)
  expect_equal( B, A_2)

  C <- A %t% "numeric"
  D <- A %t% "num"
  expect_equal( C, D )
  expect_equal( C, A_2 )

  encode <- "numeric"
  E <- A %t% encode
  expect_equal( E, A_2)

  A <- factor(c(0,7,3,10))
  B <- as_numeric(A)
  A_2 <- c(0,7,3,10)
  expect_equal( A_2, B )

  C <- A %t% "numeric"
  expect_equal( B, C)

  D <- as_character(A)
  expect_equal(as.character(A),D)

  A_3 <- data.frame(matrix(A, ncol = 2))
  E <- as_factor(A)
  expect_equal( A, E )





})
