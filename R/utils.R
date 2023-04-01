simple_html_table <- function(data) {
  htmltools::browsable(
    htmltools::tagList(
      htmltools::div(
        style = "margin-bottom: 0.75rem",
        htmltools::tags$input(
          type = "text",
          placeholder = "Search for keywords",
          style = "padding: 0.25rem 0.5rem; width: 100%",
          oninput = "Reactable.setSearch('search-table', this.value)"
        )
      ),
      reactable::reactable(
        data,
        bordered = TRUE,
        filterable = TRUE,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(4, 8, 12),
        defaultPageSize = 4,
        highlight = TRUE,
        defaultColDef = reactable::colDef(minWidth = 120),
        elementId = "search-table",
        resizable = TRUE,
        wrap = FALSE
      )
    )
  )
}


# utility functions form Rd2roxygen package
# https://github.com/yihui/Rd2roxygen/blob/main/R/utils.R
## extract tags
tag = function(x)
  attr(x, "Rd_tag")

## replace tags
untag = function(x) {
  if (is.null(x))
    return(NULL)
  attr(x, "Rd_tag") = "TEXT"
  x
}

## construct strings from rd
reconstruct = function(rd) {
  if (is.null(rd))
    return()

  if (is.list(rd)) {
    if (length(tag(rd)) &&
        tag(rd) %in% c('\\item', '\\tabular', '\\eqn', '\\deqn', '\\link')) {
      if (tag(rd) == '\\link')
        return(paste('\\link', sprintf('[%s]', attr(
          rd, 'Rd_option'
        )), '{', rd, '}', sep = ""))
      if (length(rd) == 2) {
        return(paste(
          tag(rd),
          '{',
          rd[[1]],
          '}{',
          paste(sapply(rd[[2]], reconstruct), collapse = ""),
          '}',
          sep = "",
          collapse = ""
        ))
      } else if (length(rd) == 0)
        return(tag(rd))
    }
    special = tag(rd) == toupper(tag(rd))
    singles = tag(rd) %in% c('\\tab', '\\cr')
    prefix = ifelse(special, "",
                    paste(tag(rd), ifelse(singles, "", "{"), sep = ""))
    suffix = ifelse(special, "", ifelse(singles, "", "}"))
    paste(prefix, paste(sapply(rd, reconstruct), collapse = ""), suffix,
          sep = "")
  } else {
    if (tag(rd) == 'TEXT')
      gsub('%', '\\%', rd, fixed = TRUE)
    else
      rd
  }
}
