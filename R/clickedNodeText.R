
#' clicked node text
#'
#' @description Module for header
#'
#' @importFrom shiny NS tagList 
#' 
#' @param node_id string. clicked node id.
#' @param dict.combine data.frame. "id", "semantic_type", "category"
#' @return sets of tags.
#' @examples
#' \dontrun{
#' clickedNodeText(node_id, dict.combine)
#' }
#' @export
clickedNodeText <- function(node_id, dict.combine){
  node_group = dict.combine$semantic_type[match(node_id,dict.combine$id)]
  node_type = dict.combine$category[match(node_id,dict.combine$id)]
  HTML(paste0("<b>ID: </b>",node_id, 
              ifelse(grepl("\\w", node_group), paste0("<br><b>Semantic type: </b>", node_group), ""),
              "<br><b>Category: </b>", node_type))
}


