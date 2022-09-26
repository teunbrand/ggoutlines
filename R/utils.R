


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

split_gp <- function(gp, i = seq_len(max(lengths(gp)))) {
  gp[] <- lapply(gp, rep, length.out = max(i))
  lapply(i, function(j) {
    do_recycle <- lengths(gp) > 1
    copy <- gp
    copy[do_recycle] <- lapply(unclass(copy)[do_recycle], `[`, j)
    copy[lengths(copy) == 0] <- list(NULL)
    copy
  })
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

  var <- vec_chop(var, vec_group_loc(i)$loc)
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
