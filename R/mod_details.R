




# detailsUI <- function(id, tabName) {
#   ns <- NS(id)
#   uiOutput("ui_details")
# }

detailsServer <- function(tname, df, title, outdiv, nodeid, cui_sy = NULL, output, helps = "") {
  # moduleServer(
  #   id,
  #   function(input, output, session) {
      # ns <- NS(id)
      
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
          output[[paste0(tname, "_tb_details")]] <- reactable::renderReactable(reactable::reactable({
            df[, -1]
          },
          bordered = TRUE,
          details = function(index) {
            if(!is.null(cui_sy)){
              cui <- df$cui[index]
              print("details for table")
              print(cui)
              if(cui %in% cui_sy$id){
                htmltools::div(
                  "Synonyms of ", df$cui[index], ":",
                  htmltools::tags$pre(paste(sort(cui_sy$synonyms[cui_sy$id == df$cui[index]]), collapse = "\n")),
                  width = "70%"
                )
              }
            }
          },pagination = FALSE)
          )
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
      # outdiv
      # })
      
      # print(head(cui_sy))
      
      
      
      
      
      outdiv
    }
#   )
# }


