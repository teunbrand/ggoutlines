## code to prepare `tetris` dataset goes here

tetris <- data.frame(
  x = c(1:4, 1:3, 3, 1:2, 1:2, 5, 5, 4, 4, 6, 6, 6, 7, 7, 8, 8, 8, 5, 5, 6, 6),
  y = c(rep(1, 4), c(2, 2, 2, 3), 3, 3, 4, 4, 1, 2, 2, 3, 1, 2, 3, 2, 1, 1, 2, 3, 5, 6, 6, 7),
  id = c(rep(1:7, each = 4)),
  type = c(rep(c("line", "L-block", "square", "squiggly", "T-block", "J-block", "reverse squiggly"), each = 4))
)

usethis::use_data(tetris, overwrite = TRUE)
