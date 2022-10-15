


update_gpar <- function(gp, ...) {
  args <- list(...)
  for (i in names(args)) {
    gp[[i]] <- args[[i]]
  }
  gp
}

get_alpha <- function(col) {
  decode_colour(col, alpha = TRUE, na_value = "transparent")[, "alpha"]
}

has_alpha <- function(col) {
  any(get_alpha(col) != 1)
}

clear_grob <- function(bg, fg, col, fill = NULL) {
  if (!(has_alpha(col) || has_alpha(fill))) {
    return(bg)
  }
  if (inherits(bg, "list")) {
    if (!inherits(fg, "list")) {
      cli::cli_abort(
        c("Failed to clear grob.", i = "Probably a programming mistake. Sorry!")
      )
    }
    ans <- Map(groupGrob, src = fg, op = "clear", dst = bg)
  } else {
    ans <- groupGrob(src = fg, op = "clear", dst = bg)
  }
  return(ans)
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

split_gp <- function(gp, i = seq_len(max(lengths(gp)))) {
  gp <- unclass(gp)
  constant <- lapply(gp, function(x) {
    if (length(x) == 1) {
      return(x)
    }
    if (vec_unique_count(x) == 1) {
      return(x[1])
    }
    NULL
  })
  constant <- constant[lengths(constant) > 0]

  var <- data_frame0(!!!gp[setdiff(names(gp), names(constant))])

  if (prod(dim(var)) == 0) {
    # Early exit if there are no variable parts
    constant <- do.call(gpar, constant)
    constant <- rep(list(constant), vec_unique_count(i))
    return(constant)
  }

  loc <- vec_group_loc(i)
  var <- vec_chop(var, loc$loc)[order(loc$key)]
  lapply(var, function(x) {
    x <- c(unclass(x), constant)
    do.call(gpar, x)
  })
}

split_arrow <- function(arrow, i = seq_len(max(lengths(unclass(arrow))))) {
  if (length(i) == 1 & all(i == 1)) {
    list(arrow)
  }
  lapply(i, function(j) {
    do_recycle <- lengths(unclass(arrow)) > 1
    copy <- arrow
    copy[do_recycle] <- lapply(unclass(copy)[do_recycle], `[`, j)
    copy
  })
}

standardise_id <- function(id, id.lengths, alt_length) {
  ans <- NULL
  if (!is.null(id)) {
    ans <- id
  } else if (!is.null(id.lengths)) {
    ans <- rep(seq_along(id.lengths), id.lengths)
  }
  ans <- ans %||% rep(1L, alt_length)
  match(ans, unique(ans))
}

unique0 <- function(x, ...) if (is.null(x)) x else vec_unique(x, ...)

data_frame0 <- function(...) data_frame(..., .name_repair = "minimal")
