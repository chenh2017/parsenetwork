

clickedNodeText <- function(node_id, dict.combine){
  node_group = dict.combine$semantic_type[match(node_id,dict.combine$id)]
  node_type = dict.combine$category[match(node_id,dict.combine$id)]
  HTML(paste0("<b>ID: </b>",node_id, 
              "<br><b>Semantic type: </b>", node_group,
              "<br><b>Category: </b>", node_type))
}


box_info <- function(title, info, height = 500, border_color = "#EEEEEE"){
  div(
    p(tags$b(title, style = "padding-left: 5px;"),
      style = "margin-top: 5px;"),
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


WriteData <- function(data){
  downloadHandler(
    filename = "nodes.xls",
    content = function(path) {
      if(!is.null(data)){
        file = data
      }else{
        file = data.frame("Warning"="Try to click some rows in the 'Possible inputs' box to specify your nodes!")
      }
      readr::write_tsv(file,path)
    }
  )
}


# getDesc <- function(x){
#   dict.combine$Description[match(x, dict.combine$id)]
# }
