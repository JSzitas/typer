test_that("Test whether as_numeric works", {

  A <- c(0.7,0.5,3125134, 13)
  B <- c( 0.7, 0.5, 3125134, 13 )

  # both numeric
  expect_equal(as_numeric(A), B )

  # A character but should work with as.numeric
  A <- c("0.7","0.5","3125134", "13")

  expect_equal(as_numeric(A), B )

  # A character doesnt work with as.numeric
  A <- c("0,7","0.5","3125134", "13")
  expect_equal(as_numeric(A), B )

  # A factor throws something weird
  A <- factor(c(0.7,0.5,3125134, 13))

  expect_equal(as_numeric(A), B)

  # A  matrix isnt even possible with as.numeric (without apply)
  # much less with factors or other weird things
  B <- matrix(B, nrow = 2)
  A <- matrix(factor(c(0.7,0.5,3125134, 13)), nrow = 2)
  expect_equal(as_numeric(A), B)

  # same with data frames
  B <- data.frame(B)
  A <- data.frame(matrix(factor(c(0.7,0.5,3125134, 13)), nrow = 2))
  expect_equal(as_numeric(A), B)


  # same with lists
  B <- list(B)
  A <- list(data.frame(matrix(factor(c(0.7,0.5,3125134, 13)), nrow = 2)))
  expect_equal(as_numeric(A), B)

  # lets see whether it works with NA

  A <- c("0","1","NA","15","2,5","flyer")
  B <- c( 0,  1,  NA , 15 , 2.5 , NA)
  expect_equal(as_numeric(A), B)


  A <- data.frame( R = c("0,1","0.5","0,7","9"),
                   G = c("fly","0,6",154.4,"pupper_12"))
  B <- data.frame( R = c( 0.1,  0.5, 0.7,  9),
                   G = c( NA ,  0.6, 154.4,  NA))
  expect_equal(as_numeric(A), B)


  A <- list( R = c("0,1","0.5","0,7","9"),
             G = c("fly","0,6",154.4,"pupper_12"))
  B <- list( R = c( 0.1,  0.5, 0.7,  9),
             G = c( NA ,  0.6, 154.4,  NA))
  expect_equal(as_numeric(A), B)


})
