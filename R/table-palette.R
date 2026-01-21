# ======================================================================
# table-palette.R
#
# Semantic accessors for table color palettes defined in
# inst/styles/tables.yml
#
# Tables should NEVER hard-code hex values.
# They should ask for colors by meaning.
# ======================================================================

# ----------------------------------------------------------------------
# Internal: load palette
# ----------------------------------------------------------------------

#' Load table palette YAML
#'
#' @description
#' Reads \code{inst/styles/tables.yml} from the installed package.
#'
#' @return Named list of table style configuration.
#' @keywords internal
#' @noRd
.load_table_palette <- function() {
  path <- system.file("styles", "tables.yml", package = "likelyr")
  if (path == "") {
    stop("tables.yml not found in installed package.", call. = FALSE)
  }
  yaml::read_yaml(path)
}

# ----------------------------------------------------------------------
# Internal helpers
# ----------------------------------------------------------------------

#' Lookup palette value by semantic path
#'
#' @param path Character vector giving nested node path.
#' @param key Name of value to retrieve.
#' @param label Human-readable label for error messages.
#'
#' @return Requested palette value.
#' @keywords internal
#' @noRd
.get_palette_value <- function(path, key, label) {
  pal <- .load_table_palette()
  node <- pal
  for (p in path) {
    node <- node[[p]]
  }

  if (!key %in% names(node)) {
    stop("Unknown table ", label, ": ", key, call. = FALSE)
  }
  node[[key]]
}

#' Alternate between A/B values
#'
#' @param node Named list with elements \code{a} and \code{b}.
#' @param index Integer index (1-based).
#'
#' @return Either \code{node$a} or \code{node$b}.
#' @keywords internal
#' @noRd
.get_ab_value <- function(node, index) {
  if (index %% 2 == 1) node$a else node$b
}


# ----------------------------------------------------------------------
# Text colors — body (semantic data text)
# ----------------------------------------------------------------------

#' Get table body text color by semantic name
#'
#' @param key One of the names in \code{tables.yml$text$body}.
#'
#' @return Hex color string.
#' @keywords internal
#' @noRd
table_text_body <- function(key) {
  .get_palette_value(
    path = c("text", "body"),
    key = key,
    label = "body text color"
  )
}


# ----------------------------------------------------------------------
# Text colors — headers
# ----------------------------------------------------------------------

#' Get table header text color
#'
#' @param key One of \code{"column"} or \code{"group"}.
#'
#' @return Hex color string.
#' @keywords internal
#' @noRd
table_text_header <- function(key = c("column", "group")) {
  key <- match.arg(key)
  .get_palette_value(
    path = c("text", "header"),
    key = key,
    label = "header text color"
  )
}


# ----------------------------------------------------------------------
# Backgrounds: group headers
# ----------------------------------------------------------------------

#' Get group header background color
#'
#' @param group One of \code{"point_estimate"}, \code{"interval_estimate"},
#'   or \code{"truth"}.
#'
#' @return Hex color string.
#' @keywords internal
#' @noRd
table_group_header_bg <- function(group) {
  .get_palette_value(
    path = c("background", "group_header"),
    key = group,
    label = "group header background"
  )
}


# ----------------------------------------------------------------------
# Backgrounds: column headers
# ----------------------------------------------------------------------

#' Get column header background color
#'
#' @param section One of \code{"point_estimate"}, \code{"interval_estimate"},
#'   or \code{"truth"}.
#'
#' @return Hex color string.
#' @keywords internal
#' @noRd
table_column_header_bg <- function(section) {
  .get_palette_value(
    path = c("background", "column_header"),
    key = section,
    label = "column header background"
  )
}


# ----------------------------------------------------------------------
# Backgrounds: body
# ----------------------------------------------------------------------

#' Get default table body background
#'
#' @param section One of \code{"point_estimate"}, \code{"interval_estimate"},
#'   or \code{"truth"}.
#'
#' @return Hex color string.
#' @keywords internal
#' @noRd
table_body_bg <- function(section) {
  .get_palette_value(
    path = c("background", "body"),
    key = section,
    label = "body background"
  )
}


# ----------------------------------------------------------------------
# Backgrounds: column semantics
# ----------------------------------------------------------------------

#' Get semantic column background color
#'
#' @param key One of \code{"point"}, \code{"truth"},
#'   \code{"interval"}, or \code{"diagram"}.
#'
#' @return Hex color string.
#' @keywords internal
#' @noRd
table_column_bg <- function(key) {
  .get_palette_value(
    path = c("background", "column"),
    key = key,
    label = "column background"
  )
}


# ----------------------------------------------------------------------
# Backgrounds: row blocks (alternating)
# ----------------------------------------------------------------------

#' Get alternating row block background
#'
#' @param index Integer index (1-based).
#' @param type One of \code{"pseudolikelihood"} or \code{"level"}.
#'
#' @return Hex color string.
#' @keywords internal
#' @noRd
table_row_block_bg <- function(index, type = c("level", "pseudolikelihood")) {
  type <- match.arg(type)
  pal <- .load_table_palette()

  rows <- pal$background$row[[type]]
  .get_ab_value(rows, index)
}


# ----------------------------------------------------------------------
# Accents / structural signals
# ----------------------------------------------------------------------

#' Get table accent color
#'
#' @param key One of \code{"separator"}, \code{"header_row"},
#'   or \code{"group_row"}.
#'
#' @return Hex color string.
#' @keywords internal
#' @noRd
table_accent <- function(key) {
  .get_palette_value(
    path = c("accent"),
    key = key,
    label = "accent"
  )
}


# ----------------------------------------------------------------------
# PE row backgrounds (Profile / Integrated)
# ----------------------------------------------------------------------

#' Compute per-row PE background colors
#'
#' @description
#' Returns a vector aligned with table rows, alternating by
#' pseudolikelihood (e.g. Profile / Integrated).
#'
#' Designed for use with \code{column_spec(background = ...)}.
#'
#' @param pseudolikelihood Character vector.
#' @param levels Optional explicit ordering of levels.
#'
#' @return Character vector of hex colors.
#' @keywords internal
#' @noRd
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
#' @description
#' Convenience helper for legacy code expecting a flat palette
#' (e.g. \code{pal$psi_hat}).
#'
#' @return Named list of palette values.
#' @keywords internal
#' @noRd
flatten_table_palette <- function() {
  pal <- .load_table_palette()

  c(
    pal$text$body,
    list(
      header_text_column = pal$text$header$column,
      header_text_group = pal$text$header$group,

      bg_group_pe = pal$background$group_header$point_estimate,
      bg_group_ie = pal$background$group_header$interval_estimate,
      bg_group_truth = pal$background$group_header$truth,

      bg_head_pe = pal$background$column_header$point_estimate,
      bg_head_ie = pal$background$column_header$interval_estimate,
      bg_head_truth = pal$background$column_header$truth,

      bg_body_pe = pal$background$body$point_estimate,
      bg_body_ie = pal$background$body$interval_estimate,
      bg_body_truth = pal$background$body$truth
    ),
    pal$background$column,
    pal$accent
  )
}
