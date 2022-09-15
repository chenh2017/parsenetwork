





#' details tab
#'
#' @description show a table or list for more details about the selected node.
#' 
#' @param tname string. name of the details data.
#' @param df dataframe. details data.
#' @param title dataframe. "id", "label", "term", "semantic_type", "group2", "group", "type", "category"
#' @param outdiv list. tagList.
#' @param nodeid string. node id.
#' @param output output of shiny server.
#' @param cui_sy dataframe. Default FALSE. synonyms table.
#' @param helps string. Default "". Description about the data.
#' 
#' @importFrom rlang .data
#' @importFrom dplyr left_join
#' @import visNetwork 
#' 
#' @return a visNetwork plot.
#' @examples
#' \dontrun{
#' plot_network("df_edges", "dict.combine", "attrs", "colors_group")
#' }
#' @export
detailsTab <- function(tname, df, title, outdiv, nodeid, output, cui_sy = NULL, helps = "") {
      print(ncol(df))
      height <- ifelse(nrow(df) > 10, 500, "auto")
      
      addhr <- function(){
        if(length(outdiv)> 0){
          hr()
        }
      }
      
  
      # ## network  ====================================
      # output$ui_details <- renderUI({
        if (ncol(df) > 2) {
          outdiv <- tagList(outdiv,
                            addhr(),
                            h4(paste0(title, " for ", nodeid)) %>% 
                              shinyhelper::helper(type = "inline",
                                                  title = "Description of the data",
                                                  content = helps,
                                                  size = "m"),
                            shinycssloaders::withSpinner(
                              reactable::reactableOutput(paste0(tname, "_tb_details"), width = "100%",
                                              height = ifelse(nrow(df) > 10, "500px", "auto")), type = 6
                            ))
          print("tb_details")
          
        } else {
          outdiv <- tagList(outdiv,
                            addhr(),
                            uiOutput(paste0(tname, "_li_details"))
          )
          output[[paste0(tname, "_li_details")]] <- renderUI({
            box_info(title = paste0(title, " for ", nodeid),
                     info = tags$ul(
                       lapply(df[,2], function(x){ tags$li(x) })
                     ), helps,
                     height = ifelse(nrow(df) > 10, 500, "auto"))
          })
        }
      outdiv
    }

box_info <- function(title, info, helps, height = 500, border_color = "#EEEEEE"){
  div(
    p(tags$b(title, style = "padding-left: 5px;"),
      style = "margin-top: 5px;") %>% 
      shinyhelper::helper(type = "inline",
                          title = "Description of the data",
                          content = helps,
                          size = "m"),
    div(info,
        style = paste0("height: ", ifelse(is.numeric(height), paste0(height - 45, "px;"), "auto"),
                       "overflow: auto;
                        background: white;
                        margin-top: 5px;")
    ), style = paste0("height: ", ifelse(is.numeric(height), paste0(height, "px;"), "auto"),
                      "box-shadow: #868585 0px 0px 5px;
                       background: ", border_color, ";
                       padding: 5px;")
  )
}
