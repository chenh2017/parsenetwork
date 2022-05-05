

writeDB <- function(df, tname, db, overwrite = FALSE){
  con <- RSQLite::dbConnect(RSQLite::SQLite(), db)
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  on.exit(message("write ", "'", tname, "'", " into ", db, "\n"), add = TRUE)
  if(overwrite){
    RSQLite::dbWriteTable(con, tname, df, overwrite = TRUE)
  } else {
    RSQLite::dbWriteTable(con, tname, df, append = TRUE)
  }
}


edge2db <- function(df, db, cutoff, directed){
  if(class(df)[1]=="matrix"){
    df <- reshape2::melt(df)
    df <- data.frame("from" = as.vector(df$Var1),
                     "to" = as.vector(df$Var2),
                     "weight" = df$value)
  }
  if (class(df)[1]=="dgCMatrix"){
    summ <- Matrix::summary(df)
    df<-data.frame("from" = rownames(df)[summ$i],
                   "to" = colnames(df)[summ$j],
                   "weight" = summ$x)
  }
  
  df <- df[df$weight > cutoff,]
  ids <- unique(c(df$from, df$to))
  df_index <- data.frame("index" = 1:length(ids), "id" = sort(ids))
  df$direction <- 1
  df_reverse <- df
  colnames(df_reverse) <- c("to", "from", "weight", "direction")
  if(directed){
    df_reverse$weight <- -df_reverse$weight
    df_reverse$direction <- -1
  }
  
  df_2 <- rbind(df, df_reverse)
  df_2 <- df_2 %>% dplyr::group_by(.data$from, .data$to, .data$direction) %>% dplyr::summarise(weight = mean(.data$weight))
  # df_2 <- df_2[!duplicated(df_2),]
  
  
  df_2 <- dplyr::left_join(df_2, df_index, by = c("from" = "id"))
  df_2 <- dplyr::left_join(df_2, df_index, by = c("to" = "id"))
  
  df_2 <- df_2 %>%
    dplyr::group_by(.data$from) %>%
    dplyr::summarize(from = unique(.data$index.x),
              to = paste(.data$index.y, collapse = ";"),
              cos = paste(.data$weight, collapse = ";"))
  df_2 <- df_2[order(df_2$from), c("to", "cos")]
  
  writeDB(df_2, "edge", db, overwrite = TRUE)
  writeDB(df_index, "ids", db, overwrite = TRUE)
}


cui2db <- function(dict_cui, db){
  
  ## more info for dict_cui
  ##NLP==================================================================
  
  ## | id | term | semantic_type | group1 | group2 | category |
  
  dict_cui$type = "NLP"
  dict_cui$label = dict_cui$id
  dict_cui$term[dict_cui$term==""] = dict_cui$label[dict_cui$term==""]
  dict_cui$term_s = dict_cui$term
  
  dict_cui$group = dict_cui$group1
  dict_cui$group[dict_cui$group2 != "Ignore_cui"] = paste0(dict_cui$group2[dict_cui$group2 != "Ignore_cui"],"_","NLP")
  
  dict_cui$level1 = dict_cui$semantic_type
  dict_cui$level2 = dict_cui$term
  dict_cui$level3 = dict_cui$level4 = NA
  dict_cui$stype_s = gsub("|", "\n", dict_cui$semantic_type, fixed = TRUE)
  dict_cui <- dict_cui[, c("id", "type", "label", "term", "term_s", "semantic_type", "stype_s", "group1", "group2", "group", "level1", "level2", "level3", "level4", "category")]
  writeDB(dict_cui, "node", db, overwrite = TRUE)
}


codified2db <- function(dict, db){
  ## more info for dict_codified
  ## codified ==================================================================
  
  ## | id | term | semantic_type | group1 | group2 | category | level1 | level2 | level3 | level4 |
  
  dict$type = "Codified"
  dict$label = dict$id
  dict$term[dict$term==""] = dict$label[dict$term==""]
  dict$term_s = dict$term
  dict$group = paste0(dict$group2, "_Codified")
  dict$stype_s = gsub("|", "\n", dict$semantic_type, fixed = TRUE)
  dict <- dict[, c("id", "type", "label", "term", "term_s", "semantic_type", "stype_s", "group1", "group2", "group", "level1", "level2", "level3", "level4", "category")]
  writeDB(dict, "node", db, overwrite = FALSE)
}


details2db <- function(tname, df, db, overwrite, title, note){
  details <- getData("details", db)
  title = ifelse(is.null(title), tname, title)
  if(tname %in% details$tname){
    details$title[details$tname == tname] <- title
    writeDB(details, "details", db, overwrite = TRUE)
  } else {
    writeDB(data.frame(tname = tname, title = title, note = note), "details", db, overwrite = FALSE)
  }
  writeDB(df, tname, db, overwrite = overwrite)
}



readDB <- function(sql, tname, db){
  con <- RSQLite::dbConnect(RSQLite::SQLite(), db)
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  if(tname %in% RSQLite::dbListTables(con)){
    RSQLite::dbGetQuery(conn = con, sql)
  } else {
    print(paste0(tname, " is't in ", db))
    NULL
  }
}


getData <- function(tname, db, n = 0, id = NULL){
  if (n > 0){
    sql <- paste0("SELECT * FROM ", tname, " LIMIT 1 OFFSET ", n-1, ";")
  } else if (!is.null(id)){
    sql <- paste0("SELECT * FROM ", tname, " WHERE id='", id, "';")
  } else {
    sql <- paste0("SELECT * FROM ", tname, ";")
  }
  readDB(sql, tname, db)
}

getCosFromDB <- function(n, ids, db){
  if(!is.numeric(n)){
    n <- ids$index[ids$id == n]
  }
  data <- getData("edge", db, n = n)
  if(grepl("|", data$to, fixed = TRUE)){
    Reduce(rbind, Map(function(x){
      data.frame(from = n,
                 to = strsplit(unlist(strsplit(data$to, "|", fixed = TRUE)), ";")[[x]],
                 cos = as.numeric(strsplit(unlist(strsplit(data$cos, "|", fixed = TRUE)), ";")[[x]]),
                 direction = x,
                 row.names = NULL)}, 1:2))
  } else{
    data.frame(from = ids$id[n],
               to = ids$id[as.numeric(strsplit(data$to, ";")[[1]])],
               cos = as.numeric(strsplit(data$cos, ";")[[1]]),
               row.names = NULL)
  }
  
}

getDetailsFromDB <- function(db, id){
  details <- getData("details", db)
  for(t in details$tname){
    df <- getData("details", db)
  }
}


