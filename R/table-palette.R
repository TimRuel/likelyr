# ======================================================================
# table-palette.R
#
# Semantic accessors for table color palettes defined in
# inst/palettes/tables.yml
#
# Tables should NEVER hard-code hex values.
# They should ask for colors by meaning.
# ======================================================================


# ----------------------------------------------------------------------
# Internal: load + cache palette
# ----------------------------------------------------------------------

.table_palette_cache <- NULL

.load_table_palette <- function() {
  path <- system.file("palettes", "tables.yml", package = "likelyr")
  if (path == "") {
    stop("tables.yml not found in installed package.", call. = FALSE)
  }
  yaml::read_yaml(path)
}

.get_table_palette <- function() {
  if (is.null(.table_palette_cache)) {
    .table_palette_cache <<- .load_table_palette()
  }
  .table_palette_cache
}


# ----------------------------------------------------------------------
# Internal helpers
# ----------------------------------------------------------------------

.get_palette_value <- function(path, key, label) {
  pal <- .get_table_palette()
  node <- pal
  for (p in path) node <- node[[p]]

  if (!key %in% names(node)) {
    stop("Unknown table ", label, ": ", key, call. = FALSE)
  }
  node[[key]]
}

.get_ab_value <- function(node, index) {
  if (index %% 2 == 1) node$a else node$b
}


# ----------------------------------------------------------------------
# Text colors — body (semantic data text)
# ----------------------------------------------------------------------

#' Get table body text color by semantic name
#'
#' @param key One of the names in tables.yml$text$body
#' @return Hex color string
#' @export
table_text_body <- function(key) {
  .get_palette_value(
    path  = c("text", "body"),
    key   = key,
    label = "body text color"
  )
}


# ----------------------------------------------------------------------
# Text colors — headers
# ----------------------------------------------------------------------

#' Get table header text color
#'
#' @param key One of "column", "group"
#' @export
table_text_header <- function(key = c("column", "group")) {
  key <- match.arg(key)
  .get_palette_value(
    path  = c("text", "header"),
    key   = key,
    label = "header text color"
  )
}


# ----------------------------------------------------------------------
# Backgrounds: group headers
# ----------------------------------------------------------------------

#' Get group header background color
#'
#' @param group One of "point_estimate", "interval_estimate", "truth"
#' @export
table_group_header_bg <- function(group) {
  .get_palette_value(
    path  = c("background", "group_header"),
    key   = group,
    label = "group header background"
  )
}


# ----------------------------------------------------------------------
# Backgrounds: column headers
# ----------------------------------------------------------------------

#' Get column header background color
#'
#' @param section One of "point_estimate", "interval_estimate", "truth"
#' @export
table_column_header_bg <- function(section) {
  .get_palette_value(
    path  = c("background", "column_header"),
    key   = section,
    label = "column header background"
  )
}


# ----------------------------------------------------------------------
# Backgrounds: body
# ----------------------------------------------------------------------

#' Get default table body background
#'
#' @param section One of "point_estimate", "interval_estimate", "truth"
#' @export
table_body_bg <- function(section) {
  .get_palette_value(
    path  = c("background", "body"),
    key   = section,
    label = "body background"
  )
}


# ----------------------------------------------------------------------
# Backgrounds: column semantics
# ----------------------------------------------------------------------

#' Get semantic column background color
#'
#' @param key One of "point", "truth", "interval", "diagram"
#' @export
table_column_bg <- function(key) {
  .get_palette_value(
    path  = c("background", "column"),
    key   = key,
    label = "column background"
  )
}


# ----------------------------------------------------------------------
# Backgrounds: row blocks (alternating)
# ----------------------------------------------------------------------

#' Get alternating row block background
#'
#' @param index Integer index (1-based)
#' @param type One of "pseudolikelihood", "level"
#' @export
table_row_block_bg <- function(index, type = c("level", "pseudolikelihood")) {
  type <- match.arg(type)
  pal  <- .get_table_palette()

  rows <- pal$background$row[[type]]
  .get_ab_value(rows, index)
}


# ----------------------------------------------------------------------
# Accents / structural signals
# ----------------------------------------------------------------------

#' Get table accent color
#'
#' @param key One of "separator", "header_row", "group_row"
#' @export
table_accent <- function(key) {
  .get_palette_value(
    path  = c("accent"),
    key   = key,
    label = "accent"
  )
}


# ----------------------------------------------------------------------
# PE row backgrounds (Profile / Integrated)
# ----------------------------------------------------------------------

#' Compute per-row PE background colors
#'
#' Returns a vector aligned with rows of a table, alternating by
#' pseudolikelihood (e.g. Profile / Integrated).
#'
#' Designed for use with column_spec(background = ...)
#'
#' @param pseudolikelihood Character vector
#' @param levels Optional explicit ordering
#' @export
table_pe_row_bg <- function(pseudolikelihood, levels = NULL) {

  if (is.null(levels)) {
    levels <- unique(pseudolikelihood)
  }

  idx <- match(pseudolikelihood, levels)

  if (anyNA(idx)) {
    stop("Unknown pseudolikelihood value encountered.", call. = FALSE)
  }

  ifelse(
    idx %% 2L == 1L,
    table_row_block_bg(1, "pseudolikelihood"),
    table_row_block_bg(2, "pseudolikelihood")
  )
}


# ----------------------------------------------------------------------
# Convenience: flatten palette for legacy usage
# ----------------------------------------------------------------------

#' Flatten table palette to a named list
#'
#' Useful for legacy code expecting pal$psi_hat, etc.
#'
#' @export
flatten_table_palette <- function() {
  pal <- .get_table_palette()

  c(
    pal$text$body,
    list(
      header_text_column = pal$text$header$column,
      header_text_group  = pal$text$header$group,

      bg_group_pe      = pal$background$group_header$point_estimate,
      bg_group_ie      = pal$background$group_header$interval_estimate,
      bg_group_truth   = pal$background$group_header$truth,

      bg_head_pe       = pal$background$column_header$point_estimate,
      bg_head_ie       = pal$background$column_header$interval_estimate,
      bg_head_truth    = pal$background$column_header$truth,

      bg_body_pe       = pal$background$body$point_estimate,
      bg_body_ie       = pal$background$body$interval_estimate,
      bg_body_truth    = pal$background$body$truth
    ),
    pal$background$column,
    pal$accent
  )
}
