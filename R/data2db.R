#' Write data into a database
#' 
#' @param tname string. The table name of the data.  A table name can contain numeric alphabets and underline (e.g. table_1 ). The table name  should begin with an alphabet. The 8 table names below are specified:
#'                 * "edge": matrix data for network.
#'                 * "dict_cui": dict of cui nodes.
#'                 * "dict_codified": dict of codified nodes.
#'                 * "synonyms": synonyms of cui nodes.
#'                 * "rollup": rollup for cui nodes.
#'                 * some other names can't be used: "details", "ids", "node".
#' @param db string. The database path to write to. e.g. "./test.db".
#' @param file string. The file path to read from. rds, csv, tsv, txt will be OK.
#' @param sep string. Default "rds" or single character used to separate fields within a record (e.g. "\\t" ",", "|", ";").
#' @param overwrite logical. Default FALSE. If TRUE, The existing table will be overwrite.
#' @param cutoff float. Default 0. Parameter for "edge" data. The cutoff for matrix data of network.
#' @param directed logical. Default FALSE. Parameter for "edge" data. If TRUE, the edge of the network will be with arrow. Not currently available.
#' @param title string. Default NULL. Parameter for more data. The title for more information data.
#' @param note string. Default "". Parameter for more data. The description for more information data.
#' @return A sqlite3 database.
#' @examples
#' \dontrun{
#' saveRDS(data.frame("from" = 1:3, "to" = 4:6, "weight" = 7:9), "test_data2db.rds")
#' data2db("edge", "test_data2db.db", "test_data2db.rds")
#' }
#' @export
data2db <- function(tname, db, file, sep = "rds", overwrite = FALSE, cutoff = 0, directed = FALSE, title = NULL, note = ""){
  if(sep == "rds"){
    df <- readr::read_rds(file)
  } else {
    df <- readr::read_delim(file, delim = sep, show_col_types = FALSE)
  }
  if(!file.exists(db)){
    message("Create new database: ", db)
  }
  cat("Database: ", db, "\n")
  cat("Table: ", tname, "\n")
  if (! class(df)[1] %in% c("dgCMatrix", "matrix")){
    cat("Fields: ", colnames(df), "\n")
  }
  if(tname == "edge"){
    edge2db(df, db, cutoff = cutoff, directed = directed)
  } else if(tname == "dict_cui"){
    cui2db(df, db)
  } else if(tname == "dict_codified"){
    codified2db(df, db)
  } else if(tname == "synonyms"){
    writeDB(df, "synonyms", db, overwrite = TRUE)
  } else{
    details2db(tname, df, db, overwrite = overwrite, title = title, note = note)
  }
}
