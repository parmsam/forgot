#' Retrieve tibble with function documentation for a specified package
#'
#' @param pkg string with installed R package name
#' @param formatted boolean should it coerce columns to char, true by default
#'
#' @return tibble with function documentation info
#' @export
#' @importFrom purrr map_df
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr %>%
#' @examples
#' forgot::forgot("stringr")
forgot <- function(pkg, formatted = TRUE){
  db <- tools::Rd_db(package = pkg)
  n <- names(db)
  df <- purrr::map_df(db, parse_rd) %>%
    tidyr::pivot_wider(names_from = name, values_from = value)
  if(formatted){
    df <- df %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.list), as.character))
  }
  return(df)
}

# parse_rd is based on Rd2roxygen::parse_file()
# https://github.com/yihui/Rd2roxygen/blob/main/R/rd2roxygen.R
parse_rd <- function(rd) {
  # rd = tools::parse_Rd(path)
  tags = sapply(rd, tag)
  tags = gsub("\\\\", "", tags)
  names(rd) = tags

  # Remove top-level text strings - just line breaks between sections
  rd = rd[tags != "TEXT"]

  out = list()
  # Title, description, value and examples, need to be stitched into a
  # single string.
  out$title = reconstruct(untag(rd$title))
  out$docType = reconstruct(untag(rd$docType))
  out$usage = reconstruct(untag(rd$usage))
  out$desc = gsub("$\n+|\n+^", "", reconstruct(untag(rd$description)))
  out$details = reconstruct(untag(rd$details))
  out$section = paste(reconstruct(untag(rd$section[1])),
                      reconstruct(untag(rd$section[-1])), sep = ': ')
  if (length(out$section) == 0) out$section = NULL
  out$format = reconstruct(untag(rd$format))
  out$value = reconstruct(untag(rd$value))
  out$note = reconstruct(untag(rd$note))
  out$author = gsub('@', '@@', reconstruct(untag(rd$author)))
  out$seealso = reconstruct(untag(rd$seealso))
  out$references = reconstruct(untag(rd$references))
  out$source = reconstruct(untag(rd$source))

  out$examples = reconstruct(untag(rd$examples))

  # Join together aliases and keywords
  out$name = reconstruct(untag(rd$name))
  out$aliases = unname(sapply(rd[names(rd) == "alias"], "[[", 1))
  # If the only alias is the name, then skip it
  if (identical(out$aliases, out$name)) {
    out$aliases = NULL
  }
  out$keywords = unlist(lapply(rd[names(rd) == "keyword"], head, 1))

  # Pull apart arguments
  arguments = rd$arguments
  arguments = arguments[sapply(arguments, tag) != "TEXT"]
  out$params = unlist(sapply(arguments, function(argument) {
    if (tag(argument) != '\\item') return(NULL)
    paste(if (tag(argument[[1]][[1]]) == "\\dots")
      "\\dots" else gsub(' +', '', argument[[1]]),
      reconstruct(argument[[2]]))
  }))

  out %>%
    tibble::enframe() %>%
    dplyr::mutate(function_name = out$name)
}
