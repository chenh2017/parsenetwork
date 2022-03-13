#' Write data into a database
#' 
#' @param tname chr. The table name of the data.  A table name can contain numeric alphabets and underline (eg: table_1 ). The table name  should begin with an alphabet. The 8 table names below are specified:
#'                 * "edge": matrix data for network.
#'                 * "dict_cui": dict of cui nodes.
#'                 * "dict_codified": dict of codified nodes.
#'                 * "synonyms": synonyms of cui nodes.
#'                 * "rollup": rollup for cui nodes.
#'                 * some other names can't be used: "details", "ids", "node".
#' @param db chr. The database path to write to. eg. "./test.db".
#' @param file chr. The file path to read from. rds, csv, tsv, txt will be OK.
#' @param sep chr. Default "rds" or single character used to separate fields within a record (eg. "\\t" ",", "|", ";").
#' @param overwrite bool. Default FALSE. If TRUE, The existing table will be overwrite.
#' @param cutoff float. Default 0. Parameter for "edge" data. The cutoff for matrix data of network.
#' @param directed bool. Default FALSE. Parameter for "edge" data. If TRUE, the edge of the network will be with arrow. Not currently available.
#' @param title	chr. Default NULL. Parameter for more data. The title for more information data.
#' @return A sqlite3 database.
#' @examples
#' \dontrun{
#' saveRDS(data.frame("from" = 1:3, "to" = 4:6, "weight" = 7:9), "test_data2db.rds")
#' data2db("edge", "test_data2db.db", "test_data2db.rds")
#' }
#' @export
data2db <- function(tname, db, file, sep = "rds", overwrite = FALSE, cutoff = 0, directed = FALSE, title = NULL){
  if(sep == "rds"){
    df <- readr::read_rds(file)
  } else {
    df <- readr::read_delim(file, delim = sep)
  }
  if(tname == "edge"){
    edge2db(df, db, cutoff = cutoff)
  } else if(tname == "dict_cui"){
    cui2db(df, db)
  } else if(tname == "dict_codified"){
    codified2db(df, db)
  } else if(tname == "synonyms"){
    writeDB(df, "synonyms", db, overwrite = TRUE)
  } else{
    details <- getData("details", db)
    title = ifelse(is.null(title), tname, title)
    if((!overwrite) & (!is.null(details)) & (tname %in% details$tname)){
      details$title[details$tname == tname] <- title
      writeDB(details, "details", db, overwrite = TRUE)
      writeDB(df, tname, db, overwrite = TRUE)
    } else {
      writeDB(data.frame(tname = tname, title = title), "details", db, overwrite = overwrite)
      writeDB(df, tname, db, overwrite = overwrite)
    }
  }
}

# writeDB <- function(df, tname, db){
#   con <- dbConnect(RSQLite::SQLite(), db)
#   if(tname %in% dbListTables(con)){
#     dbWriteTable(con, tname, df, append = TRUE)
#   } else {
#     dbWriteTable(con, tname, df)
#   }
#   dbDisconnect(con)
# }

writeDB <- function(df, tname, db, overwrite = FALSE){
  con <- RSQLite::dbConnect(RSQLite::SQLite(), db)
  if(overwrite){
    RSQLite::dbWriteTable(con, tname, df, overwrite = overwrite)
  } else {
    RSQLite::dbWriteTable(con, tname, df, append = TRUE)
  }
  RSQLite::dbDisconnect(con)
}



# edge2db <- function(df, db, cutoff = 0){
#   df <- data.frame(df)
#   colnames(df) <- rownames(df)
#   df$id <- rownames(df)
#   df <- reshape2::melt(df)
#   df <- df[df$value > cutoff,]
#   df_index <- data.frame(index = 1:length(unique(df$id)), id = sort(unique(df$id)))
#   df <- left_join(df, df_index, by = "id")
#   df <- left_join(df, df_index, by = c("variable" = "id"))
#   
#   df <- df %>%
#     group_by(id) %>%
#     summarize(from = unique(index.x),
#               to = paste(index.y, collapse = ";"),
#               cos = paste(value, collapse = ";"))
#   df <- df[order(df$from), c("to", "cos")]
#   writeDB(df, "edge", db, overwrite = TRUE)
#   writeDB(df_index, "ids", db, overwrite = TRUE)
# }

edge2db <- function(df, db, cutoff = 0){
  # df <- data.frame(df)
  # colnames(df) <- rownames(df)
  # df$id <- rownames(df)
  # df <- reshape2::melt(df)
  df <- df[df$weight > cutoff,]
  ids <- unique(c(df$from, df$to))
  df_index <- data.frame("index" = 1:length(ids), "id" = sort(ids))
  df <- dplyr::left_join(df, df_index, by = c("from" = "id"))
  df <- dplyr::left_join(df, df_index, by = c("to" = "id"))
  
  df <- df %>%
    dplyr::group_by(.data$from) %>%
    dplyr::summarize(from = unique(.data$index.x),
              to = paste(.data$index.y, collapse = ";"),
              cos = paste(.data$weight, collapse = ";"))
  df <- df[order(df$from), c("to", "cos")]
  writeDB(df, "edge", db, overwrite = TRUE)
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


readDB <- function(sql, tname, db){
  con <- RSQLite::dbConnect(RSQLite::SQLite(), db)
  if(tname %in% RSQLite::dbListTables(con)){
    data <- DBI::dbGetQuery(conn = con, sql)
  } else {
    print(paste0(tname, " is't in ", db))
    data <- NULL
  }
  RSQLite::dbDisconnect(con)
  return(data)
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
  data.frame(from = ids$id[n],
             to = ids$id[as.numeric(strsplit(data$to, ";")[[1]])],
             cos = as.numeric(strsplit(data$cos, ";")[[1]]),
             row.names = NULL)
}

getDetailsFromDB <- function(db = "test/test.db", id){
  details <- getData("details", db)
  for(t in details$tname){
    df <- getData("details", db)
  }
}


