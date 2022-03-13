

dataNetwork <- function(center_nodes, df_edges, dict.combine, attrs){
  
  attr_edges <- attrs$attr_edges
  attr_nodes_type <- attrs$attr_nodes_type
  attr_nodes_cap <- attrs$attr_nodes_cap
  # print(head(df_edges))
  df_edges <- df_edges[df_edges$from != df_edges$to, ]
  
  df_edges$ends <- paste0(df_edges$from, ";",df_edges$to)
  df_edges$ends <- sapply(df_edges$ends, function(x){
    paste(sort(strsplit(x, ";", fixed = T)[[1]]), collapse = ";")
  })
  
  df_edges <- df_edges[!duplicated(df_edges$ends), ]
  df_edges <- df_edges[, -4]
  
  df_edges$length <- abs(df_edges$cos)^(-1.1)*10
  df_edges$title <- paste0(df_edges$from,"<b> &rarr; </b>", df_edges$to, "<br>", df_edges$cos)
  df_edges$edgetype <- "center-other"
  df_edges$edgetype[df_edges$from %in% center_nodes &
                      df_edges$to %in% center_nodes ] <- "center-center"
  
  
  df_edges <- left_join(df_edges, attr_edges, by = "edgetype")
  
  df_nodes <- data.frame(id = unique(c(df_edges$from, df_edges$to)))
  df_nodes <- left_join(df_nodes, dict.combine[, c("id", "label", "term", "semantic_type", "group2", "group", "type", "category")], by = c("id"))
  # colnames(df_nodes) <- c("id", "label", "group", "type", "index01", "index02", "capinfo2", "Category")
  # df_nodes$Cap <- df_nodes$index01
  # df_nodes$Cap[df_nodes$Cap == "Ignore_cui"] <- df_nodes$index02[df_nodes$Cap == "Ignore_cui"]
  
  df_nodes$iscenter <- "other"
  df_nodes$iscenter[df_nodes$id %in% center_nodes] <- "center"
  
  df_nodes <- left_join(df_nodes, attrs$attr_nodes_center, by = "iscenter")
  df_nodes <- left_join(df_nodes, attrs$attr_nodes_cap, by = c("group2" = "Cap"))
  # df_nodes <- left_join(df_nodes, attrs$attr_nodes_type, by = "type")
  df_nodes$shape <- "ellipse"
  df_nodes$shape[df_nodes$type == "NLP"] <- "box"
  
  # write_csv(df_nodes, "df_nodes.csv")
  
  # df_nodes$group[df_nodes$id %in% center_nodes] <- df_nodes$label[df_nodes$id %in% center_nodes]
  
  df_nodes$title = paste0("<b>ID: </b>",df_nodes$id,
                          "<br><b>Term: </b>",df_nodes$term,
                          "<br><b>Semantic type: </b>",df_nodes$semantic_type,
                          # "<br><b>Group: </b>", df_nodes$group,
                          "<br><b>Category: </b>", df_nodes$category)
  
  
  # df_nodes$font.background[is.na(df_nodes$font.background)] <- ""

  
  return(list(df_edges, df_nodes))
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
               # selectedBy = list(`variable` = "Cap_label",
               #                   `multiple` = TRUE,
               #                   `main` = "Select by group"),
               collapse = FALSE) %>%
    visInteraction(hover = TRUE) %>%
    visIgraphLayout(layout = layout,
                    physics = FALSE,
                    smooth = FALSE,
                    type = "square") %>%
    visEvents(selectNode = "function(nodes) {
                    Shiny.onInputChange('current_node_id', nodes);
                    ;}") %>%
    visLayout(randomSeed = 10) # to have always the same network
}



plot_network <- function(df_edges, hide_labels,
                         node_num_cutoff,
                         myconfirmation, slider_text, slider_size,
                         dict.combine, 
                         attrs, 
                         layout = "layout_nicely"){
  print(nrow(df_edges))
  if(nrow(df_edges) > 0){
    center_nodes = unique(df_edges$from)
    draw.data = dataNetwork(center_nodes, df_edges, dict.combine, attrs)
    df_edges = draw.data[[1]]
    df_nodes = draw.data[[2]]
    print(nrow(df_edges))
    
    if(hide_labels){
      df_nodes$label <- "    "
      df_nodes$font.size[df_nodes$iscenter == "center"] <- 50
      df_nodes$font.size[df_nodes$iscenter == "other"] <- 30
      df_nodes$font.background <- NA
      df_nodes$label[df_nodes$shape == "box"] <- "        "
    }
    
    codified <- c(2:5)[attrs$attr_legend_groups$label[2:5] %in% unique(df_nodes$group2[df_nodes$type == "Codified"])]
    nlp <- c(7:16)[attrs$attr_legend_groups$label[7:16] %in% unique(df_nodes$group2[df_nodes$type == "NLP"])]
    legend_to_show <- c(1,codified,6,nlp)
    
      p <- visNetwork(df_nodes, df_edges, width = "100%",height = "100%") %>%
        visLegend(addNodes = attrs$attr_legend_groups[legend_to_show,],
                  width = 0.09,
                  position = "right",
                  useGroups = FALSE,
                  zoom = TRUE,
                  stepX = 150,
                  stepY = 70,
                  ncol=1)
      add_attr_network(p, layout)


  }else{
    visNetwork(data.frame(), data.frame(), width = "100%",
               main = paste("Try to click some rows in",tagList(icon("table")),"to specify your nodes"))
  }
  
  
}


