#' Retrieve tibble with function documentation for a specified package
#'
#' @param pkg string with installed R package name
#' @param formatted boolean should it coerce columns to char, true by default
#' @param keyword optional string to filter the dataframe on for any mention of
#' @param selected optional string vector of columns to search on, null by default
#' @param interactive optional boolean to get a reactable HTML table
#' @return tibble with function documentation info
#' @export
#' @importFrom tools Rd_db
#' @importFrom purrr map_df
#' @importFrom tidyr pivot_wider
#' @importFrom stringr str_detect
#' @importFrom stringr fixed
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr if_any
#' @examples
#' forgot::forgot("stringr")
#' forgot::forgot("dplyr", "count")
forgot <- function(pkg,
                   keyword = NULL,
                   selected = NULL,
                   formatted = T,
                   interactive = F) {
  db <- tools::Rd_db(package = pkg)
  n <- names(db)
  df <- purrr::map_df(db, parse_rd) %>%
    tidyr::pivot_wider(names_from = name, values_from = value)
  if (formatted) {
    df <- df %>%
      dplyr::mutate(dplyr::across(where(is.list), as.character))
  }
  if (!is.null(selected) & is.character(selected)) {
    if (!("function_name" %in% selected)) {
      selected <- c("function_name", selected)
    }
    df <- df %>%
      dplyr::select(selected)
  }
  if (!is.null(keyword) & is.character(keyword)) {
    df <- df %>%
      dplyr::filter(dplyr::if_any(
        dplyr::everything(),
        ~stringr::str_detect(.,
                             stringr::fixed(keyword, ignore_case = TRUE))
      ))
  }
  if (interactive) {
    df <- df %>% simple_html_table()
  }
  return(df)
}

#' Get forgot tibble with only first two fields populated by default
#'
#' @param pkg string with installed R package name
#' @param keyword optional string to filter the dataframe on for any mention of
#' @param selected optional string vector of columns to add on to search
#'
#' @return tibble with function documentation info
#' @export
#'
#' @examples
#' forgot2("stringr")
#' forgot2("dplyr", "count")
forgot2 <- function(pkg,
                    keyword = NULL,
                    selected = c()
                    ){
  selected_fields <- c("function_name", "title", selected)
  forgot(pkg,
         keyword = keyword,
         selected = selected_fields,
         interactive = F)
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
  if (length(out$section) == 0)
    out$section = NULL
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
  out$keywords = unlist(lapply(rd[names(rd) == "keyword"], utils::head, 1))

  # Pull apart arguments
  arguments = rd$arguments
  arguments = arguments[sapply(arguments, tag) != "TEXT"]
  out$params = unlist(sapply(arguments, function(argument) {
    if (tag(argument) != '\\item')
      return(NULL)
    paste(if (tag(argument[[1]][[1]]) == "\\dots")
      "\\dots"
      else
        gsub(' +', '', argument[[1]]),
      reconstruct(argument[[2]]))
  }))

  out %>%
    tibble::enframe() %>%
    dplyr::mutate(function_name = out$name)
}

#' Get tibble with roxygen2 fields for a specific function of interest
#'
#' @param pkg string installed R package name
#' @param function_name string function name in R package
#' @param field string roxygen2 field of interest, NULL by default
#' @param print logical on if you should print data frame, TRUE by default
#'
#' @return forgot tibble with roxygen2 field info for pkg or function
#' @export
#'
#' @examples
#' forgot_fx("dplyr", "count", "usage", print = FALSE)
#' forgot_fx("dplyr", "count", "usage", print = TRUE)
forgot_fx <- function(pkg, function_name,
                      field = NULL,
                      print = TRUE){
  df <- forgot::forgot(pkg) %>%
      dplyr::filter(function_name == {{function_name}})
  if(length(field)){
    df <- df %>%
      dplyr::select({{field}})
  }
  if(!print){
    invisible(df)
  } else{
    return(df)
  }
}

#' Print usage for an R function of interest in console
#'
#' @param pkg string with installed R package name
#' @param function_name function name in R package
#' @param write logical write to RStudio rmd file, FALSE by default
#'
#' @return NULL
#' @export
#'
#' @examples
#' forgot_usg("dplyr", "count")
forgot_usg <- function(pkg, function_name,
                       write = FALSE){
  f <- forgot_fx(
            pkg = {{pkg}}, {{function_name}},
            field = "usage",
            print = FALSE)[[1]]
  cat(f)
  invisible(f)
}

#' Print examples for an R function of interest in console
#'
#' @param pkg string with installed R package name
#' @param function_name function name in R package
#' @param write logical write to RStudio rmd file, FALSE by default
#'
#' @return NULL
#' @export
#'
#' @examples
#' forgot_exmpls("dplyr", "count")
#' forgot_exmpls("dplyr", "count", write = TRUE)
forgot_exmpls <- function(pkg, function_name,
                          write = FALSE){
  f <- forgot_fx(
    pkg = {{pkg}}, {{function_name}},
    field = "examples",
    print = FALSE)[[1]]
  cat(f)
  if(write){
    new_file_write(f)
  }
  invisible(f)
}

#' Print parameters for an R function of interest in console
#'
#' @param pkg string with installed R package name
#' @param function_name function name in R package
#' @param write logical write to RStudio rmd file, FALSE by default
#'
#' @return NULL
#' @export
#'
#' @examples
#' forgot_params("dplyr", "count")
forgot_params <- function(pkg, function_name,
                          write = FALSE){
  f <- forgot_fx(
    pkg = {{pkg}}, {{function_name}},
    field = "params",
    print = FALSE)[[1]]
  f <- eval(parse(text = f))
  cat(f, sep = "\n------\n")
  if(write){
    new_file_write(f)
  }
  invisible(f)
}

#' Create new RStudio Rmarkdown document with content of interest
#'
#' @param content vector with text to include in file
#' @param type type of file to create in RStudio IDE, RMarkdown by default
#'
new_file_write <- function(content, type = "rmarkdown"){
  rstudioapi::documentNew(text = content, type = type)
}
