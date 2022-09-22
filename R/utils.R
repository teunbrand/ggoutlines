


update_gpar <- function(gp, ...) {
  args <- list(...)
  for (i in names(args)) {
    gp[[i]] <- args[[i]]
  }
  gp
}

get_alpha <- function(col) {
  decode_colour(col, alpha = TRUE, na_value = "tranparent")[, "alpha"]
}

protect0 <- function(x) {
  if (length(x) == 0) {
    NULL
  } else x
}

modify_list <- function(old, new) {
  for (i in names(new)) {
    old[[i]] <- new[[i]]
  }
  old
}
