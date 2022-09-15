#' UI Module for reactable of clicked node
#'
#' @description Module for reactable of clicked node
#'
#' @importFrom shiny NS tagList 
#' 
#' @param id Internal parameters for {shiny}.
#' @return sets of tags.
#' @examples
#' \dontrun{
#' tbClickedUI('tb_clicked')
#' }
#' @export
tbClickedUI <- function(id){
  ns <- NS(id)
  tagList( uiOutput(ns("clicked_node_table")))
}


#' Server Module for reactable of clicked node
#'
#' @description Server Module for reactable of clicked node
#'
#' @importFrom shiny NS tagList 
#' 
#' @param id string. Namespace of the module.
#' @param data dataframe. the collected data for the clicked node.
#' @param height string. A valid CSS unit \(like "100\%", "400px", "auto"\).
#' @param dict.combine dataframe. "id", "term", "category"
#' @param name_weight string. Default "cosine similarity". Name for the column of weight in "data".
#' @return server part for tbClicked UI.
#' @examples
#' \dontrun{
#' tbClickedServer('tb_clicked')
#' }
#' @export
tbClickedServer <- function(id, data, height, dict.combine, name_weight = "cosine similarity"){
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    output$clicked_node_table <- renderUI({
      if(nrow(data) > 0){
        shinycssloaders::withSpinner(
          reactable::reactableOutput(ns("tb_clicked_node"), width = "100%",
                                     height = height), type = 6
        )} else {
          ""
        }
    })
    
    df <- reactive({
      colnames(data) <- c("center_nodes", "connected_nodes", "cosine_similarity")
      df <- left_join(data,
                      dict.combine[, c("id", "term", "category")],
                      by = c("connected_nodes" = "id"))
      df$cosine_similarity <- round(df$cosine_similarity, 3)
      df[order(df$cosine_similarity, decreasing = TRUE), c(5, 2, 4, 3)]
    })
    
    
    output$tb_clicked_node <- reactable::renderReactable(
      reactable::reactable({ df() },
      groupBy = c("category"),
      columns = list(
        cosine_similarity = reactable::colDef(name = name_weight),
        connected_nodes = reactable::colDef(
          minWidth = 250,
          name = "connected_nodes / term",
          # Show species under character names
          cell = function(value, index) {
            term <- df()$term[index]
            term <- if (!is.na(term)) term else "Unknown"
            div(
              div(style = list(fontWeight = 600), value),
              div(style = list(fontSize = 12), term)
            )
          }
        ),
        term = reactable::colDef(show = FALSE)
      ),
      bordered = TRUE,
      defaultExpanded = TRUE,
      pagination = FALSE
      )
    )
  })
}





