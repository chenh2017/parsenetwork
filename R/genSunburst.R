
#' @importFrom data.table := .SD .N 

## Generate sunburst plot using plotly =======================================
sunburstDF <- function(DF, valueCol = NULL, root.name = "Root"){
  
  colNamesDF <- names(DF)
  
  if(data.table::is.data.table(DF)){
    DT <- data.table::copy(DF)
  } else {
    DT <- data.table::data.table(DF, stringsAsFactors = FALSE)
  }
  
  DT[, "root" := root.name]
  colNamesDT <- names(DT)
  
  if(is.null(valueCol)){
    data.table::setcolorder(DT, c("root", colNamesDF))
  } else {
    data.table::setnames(DT, valueCol, "values", skip_absent=TRUE)
    data.table::setcolorder(DT, c("root", setdiff(colNamesDF, valueCol), "values"))
  }
  
  hierarchyCols <- setdiff(colNamesDT, "values")
  hierarchyList <- list()
  
  for(i in seq_along(hierarchyCols)){
    currentCols <- colNamesDT[1:i]
    if(is.null(valueCol)){
      currentDT <- unique(DT[, currentCols, with = FALSE][, "values" := .N, by = currentCols], by = currentCols)
    } else {
      currentDT <- DT[, lapply(.SD, sum, na.rm = TRUE), by=currentCols, .SDcols = "values"]
    }
    #currentDT = stats::na.omit(currentDT)
    data.table::setnames(currentDT, length(currentCols), "labels")
    hierarchyList[[i]] <- currentDT
  }
  
  hierarchyDT <- data.table::rbindlist(hierarchyList, use.names = TRUE, fill = TRUE)
  
  parentCols <- setdiff(names(hierarchyDT), c("labels", "values", valueCol))
  hierarchyDT[, "parents" := apply(.SD, 1, function(x){data.table::fifelse(all(is.na(x)), yes = NA_character_, no = paste(x[!is.na(x)], sep = ":", collapse = " - "))}), .SDcols = parentCols]
  hierarchyDT[, "ids" := apply(.SD, 1, function(x){paste(x[!is.na(x)], collapse = " - ")}), .SDcols = c("parents", "labels")]
  hierarchyDT[, c(parentCols) := NULL]
  return(hierarchyDT)
}
sunburstPreData <- function(df, changeline){
  
  df = df[!is.na(df$labels), ]
  # df$labels = stringr::str_replace(df$labels, "^.*_Codified","Codified")
  # df$labels = stringr::str_replace(df$labels, "^.*_NLP","NLP")
  # df$labels = stringr::str_replace(df$labels, "Ignore_cui","Others")
  # df$labels = stringr::str_replace(df$labels, ",...",", ...")
  df$labels = gsub("^.*_Codified", "Codified", df$labels, perl = TRUE)
  df$labels = gsub("^.*_NLP", "NLP", df$labels, perl = TRUE)
  df$labels = gsub("Ignore_cui", "Others", df$labels, fixed = TRUE)
  df$labels = gsub(",...", ", ...", df$labels, fixed = TRUE)
  df$text = df$labels
  # label = df$labels[stringr::str_length(df$labels)>5]
  label = df$labels[nchar(df$labels)>5]
  # label_split = stringr::str_split(label," ")
  label_split = strsplit(label, " ", fixed = TRUE)
  if(changeline != 99){
    label_com = sapply(label_split, function(x){
      y = ""
      i = 1
      k = 0
      while(i <= length(x)){
        y = paste(y, x[i])
        # k = k + stringr::str_length(x[i])
        k = k + nchar(x[i])
        if(k>=changeline & i!=length(x)){
          y = paste0(y,"<br>")
          k = 0
        }
        i = i + 1
      }
      # return(stringr::str_trim(y,side = "both"))
      y <- gsub("^\\s*", "", y, perl = TRUE)
      gsub("\\s*$", "", y, perl = TRUE)
    })
    # df$text[stringr::str_length(df$labels)>5] = label_com
    df$text[nchar(df$labels)>5] = label_com
  }
  return(df)
}

sunburstPlotly <- function(node_now, df_edges, 
                           dict.combine){
  
  node_name = dict.combine$term[match(node_now,dict.combine$id)]
  nodes = df_edges$to
  rhd = dict.combine[match(nodes,dict.combine$id), 
                     c("id","group1","group2","group","level1","level2","level3","level4")]
  rhd$x = df_edges$cos
  
  if(nrow(rhd)>0){
    rhd = rhd[order(rhd$group2,rhd$group,rhd$level1,rhd$level2,
                    rhd$level3,rhd$level4,rhd$x),]
    DF = rhd[,-c(1,2)]
    df = sunburstDF(DF,valueCol = "x",root.name = node_name)
    df = sunburstPreData(df, changeline=10)
    
    m <- list(
      l = 0,r = 0,b = 0,t = 0,pad = 0
    )
    
    plotly::plot_ly(data = df, ids = ~ids, labels= ~labels, parents = ~parents, 
            text = ~text, values= ~values, type='sunburst', branchvalues = 'total',
            hoverinfo = "label", textinfo = "text", textfont = list(color="black"),
            height =  750)%>%
      plotly::layout(autosize = F, margin = m)

  }
}