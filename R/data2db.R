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
