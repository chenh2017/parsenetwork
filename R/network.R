
#' plot network
#'
#' @description Plot the network.
#' 
#' @param center_nodes vector. center nodes' id.
#' @param df_edges dataframe. "from", "to", "cos".
#' @param dict.combine dataframe. "id", "label", "term", "semantic_type", "group2", "group", "type", "category"
#' @param attrs list. attr_nodes_type attr_nodes_center attr_edges attr_legend_edge(if directed)
#' @param colors_group dataframe. "group", "color.background".
#' @param shapes vector.
#' @param hide_labels logical. Default TRUE.
#' @param directed logical. Default FALSE.
#' @param node_num_cutoff numeric. Default 500. Most nodes to show.
#' @param layout string. Default "layout_nicely". layout for igraph.
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
plot_network <- function(center_nodes, df_edges, dict.combine, attrs, colors_group, shapes, 
                         hide_labels = TRUE, 
                         directed = FALSE,
                         node_num_cutoff = 500, 
                         layout = "layout_nicely"){
  if(nrow(df_edges) > 0){
    draw.data = dataNetwork(center_nodes, df_edges, dict.combine, attrs, colors_group, shapes, directed)
    df_edges = draw.data[[1]]
    df_nodes = draw.data[[2]]
    legends = draw.data[[3]]
    print(nrow(df_edges))
    
    if(hide_labels){
      df_nodes$label <- "    "
      df_nodes$font.size[df_nodes$iscenter == "center"] <- 50
      df_nodes$font.size[df_nodes$iscenter == "other"] <- 30
      df_nodes$font.background <- NA
    }
    
    addEdges = NULL
    if(directed){
      addEdges = attrs$attr_legend_edge
    } 
    
    p <- visNetwork(df_nodes, df_edges, width = "100%",height = "100%") %>%
      visLegend(addNodes = legends,
                addEdges = addEdges,
                width = 0.1,
                position = "right",
                useGroups = FALSE,
                zoom = TRUE,
                stepX = 150,
                stepY = 70,
                ncol = 1)
    add_attr_network(p, layout)

  } else {
    visNetwork(data.frame(), data.frame(), width = "100%",
               main = paste("Try to click some rows in",tagList(icon("table")),"to specify your nodes"))
  }
}




dataNetwork <- function(center_nodes, df_edges, dict.combine, attrs, 
                        colors_group, shapes, directed){
  
  df_edges$title <- paste(df_edges$from, "<b> &rarr; </b>", df_edges$to, ": ", round(df_edges$cos, 3))
  
  df_edges$ends <- apply(df_edges[, 1:2], 1, function(x){
    paste(sort(x), collapse = "|")
  })
  
  if(!directed){
    
  df_edges <- df_edges %>%
    dplyr::group_by(.data$ends) %>%
    dplyr::summarise(from = gsub("\\|.+", "", .data$ends[1], perl = TRUE),
                     to = gsub(".+\\|", "", .data$ends[1], perl = TRUE),
                     length = mean(abs(.data$cos)^(-1.1)*10),
                     title = .data$title[1])
  
  } else {

    df_edges$direction = 1
    df_edges$direction[(!df_edges$from %in% center_nodes) & (df_edges$to %in% center_nodes)] = -1
    
    df_edges <- df_edges %>%
      dplyr::group_by(.data$ends) %>%
      dplyr::summarise(from = gsub("\\|.+", "", .data$ends[1], perl = TRUE),
                       to = gsub(".+\\|", "", .data$ends[1], perl = TRUE),
                       length = mean(abs(.data$cos)^(-1.1)*10),
                       title = paste(.data$title, collapse = "<br>"), 
                       direction = sum(.data$direction))
    
    df_edges$arrows <- "to"
    df_edges$arrows[df_edges$direction == -1] <- "from"
    df_edges$arrows[df_edges$direction == 0] <- "from;to"
  }
  
  df_edges$edgetype <- "center-other"
  df_edges$edgetype[df_edges$from %in% center_nodes &
                      df_edges$to %in% center_nodes ] <- "center-center"
  
  if(directed){
    df_edges$edgetype[df_edges$direction == -1] <- "other-center"
    df_edges$edgetype[df_edges$direction == 0] <- "two-way"
  }
  
  # attr_edges
  # edgetype      color.color       width
  # center-center #FEF65C           4
  # center-other  rgba(128,128,128) 2
  # other-center  #26FD8D           2
  # two-way       #FF96EB           4
  df_edges <- left_join(df_edges, attrs$attr_edges, by = "edgetype")
  
  df_nodes <- data.frame(id = unique(c(df_edges$from, df_edges$to)))
  df_nodes <- left_join(df_nodes, dict.combine[, c("id", "label", "term", "semantic_type", "group2", "group", "type", "category")], by = c("id"))
  df_nodes$label <- df_nodes$term
  
  df_nodes$iscenter <- "other"
  df_nodes$iscenter[df_nodes$id %in% center_nodes] <- "center"
  
  # attr_nodes_center
  # iscenter
  df_nodes <- left_join(df_nodes, attrs$attr_nodes_center, by = "iscenter")
  
  # colors_group
  # group color.background
  df_nodes <- left_join(df_nodes, colors_group, by = "group")
  df_nodes$color.highlight.background = 
    df_nodes$color.hover.background = 
    df_nodes$color.border = df_nodes$color.background
  
  df_nodes$font.color = df_nodes$color.background
  
  # attr_nodes_type:
  # type      shape
  # NLP       ellipse
  # Codified  box
  # attr_nodes_type:
  # type      shape
  # CUI       dot
  # nonCUI    square
  attrs$attr_nodes_type$shape <- rep(shapes, 2)
  
  df_nodes <- left_join(df_nodes, attrs$attr_nodes_type, by = "type")
  
  df_nodes$title = paste0("<b>ID: </b>", df_nodes$id,
                          "<br><b>Term: </b>", df_nodes$term,
                          "<br><b>Semantic type: </b>", df_nodes$semantic_type,
                          "<br><b>Category: </b>", df_nodes$category)
  
  
  legends <- df_nodes[, c("group", "shape", "color.background")]
  legends <- legends[!duplicated(legends), ]
  colnames(legends) <- c("label", "shape", "color")
  legends$size <- 10
  legends$font.size <- 10
  legends$font.color <- "white"
  legends <- legends[order(legends$shape, legends$label),]
  
  if(length(unique(legends$shape)) > 1){
    for (s in unique(attrs$attr_nodes_type$shape)){
      if(s %in% legends$shape){
        legends <- dplyr::add_row(
          legends,
          data.frame("label" = attrs$attr_nodes_type$type[match(s, attrs$attr_nodes_type$shape)],
                     "shape" = "box",
                     "color" = "rgba(0,0,0,0)",
                     "size" = 10,
                     "font.size" = 20,
                     "font.color" = "white"),
          .before = match(s, legends$shape)
        )
      }
    }
  }
  
  
  return(list(df_edges, df_nodes, legends))
}

add_attr_network <- function(p, layout = "layout_nicely"){
  p %>%
    visNodes(shadow = list(enabled = TRUE, size = 4, x = 3, y = 3)) %>%
    visEdges(physics = FALSE,
             smooth = FALSE,
             hoverWidth = 2.5) %>%
    visOptions(highlightNearest = list(enabled = T,
                                       degree = 1,
                                       hover = FALSE,
                                       hideColor = "rgba(200,200,200,0.2)"),
               collapse = FALSE) %>%
    visInteraction(hover = TRUE) %>%
    visIgraphLayout(layout = layout,
                    physics = FALSE,
                    smooth = FALSE,
                    type = "square") %>%
    visEvents(selectNode = "function(nodes) {
                    Shiny.onInputChange('current_node_id', nodes);
                    ;}") %>%
    visLayout(randomSeed = 10) 
}
