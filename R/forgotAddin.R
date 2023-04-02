#' Forgot Rstudio Addin to Search Package Documentation
#'
#' Creates the miniUI for the forgot RStudio Addin
#'
#' @param selected_pkg string optional package name to search on
#'
#' @importFrom miniUI miniPage
#'
#' @examples
#' forgotAddin("dplyr")
#'
#' @noRd
forgotAddin <- function(selected_pkg = NULL) {
  installed_pkgs <- as.vector(installed.packages()[, 1])
  doc_fields <- c("function_name","title", "usage",
                  "desc", "value", "author",
                  "examples", "name", "aliases",
                  "params", "keywords", "seealso",
                  "format")
  selected_fields <- c("function_name", "title", "desc")
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar(
      shiny::div(
        "forgot",
        shiny::icon("people-pulling",
                    lib = "font-awesome",
                    verify_fa = FALSE)
      )
    ),
    miniUI::miniContentPanel(
      # Define layout, inputs, outputs
      shiny::selectInput(
        inputId = "pkg_select",
        label = "Select installed package",
        choices = installed_pkgs,
        selected = selected_pkg
      ),
      shiny::textInput(inputId = "keyword",
                       label = "Enter seach keyword (optional)"),
      shiny::selectizeInput(
        inputId = "fields",
        label = "Enter search fields (optional)",
        choices = doc_fields,
        selected = selected_fields,
        multiple = T
      ),
      shiny::actionButton(
        inputId = "search",
        label = "Search",
        icon = shiny::icon("search", verify_fa = FALSE)
      ),
      reactable::reactableOutput("table")
    )
  )

  server <- function(input, output, session) {
    react_tbl <- shiny::reactiveVal(NULL)
    # Define reactive expressions, outputs, etc.
    shiny::observeEvent(input$search, {
      if (input$keyword != "") {
        search_tbl <- forgot(input$pkg_select,
                             keyword = input$keyword,
                             selected = input$fields,
                             interactive = F)
      } else {
        search_tbl <- forgot(input$pkg_select,
                             selected = input$fields,
                             interactive = F)
      }
      react_tbl(search_tbl)
      print(paste("Searched", input$pkg_select))
    })
    output$table <- reactable::renderReactable({
      if(!is.null(react_tbl())) forgot::simple_reactable_table(react_tbl())
    })
    # When the Done button is clicked, return a value
    shiny::observeEvent(input$done, {
      shiny::stopApp(returnValue)
    })
  }

  shiny::runGadget(ui, server)
}
