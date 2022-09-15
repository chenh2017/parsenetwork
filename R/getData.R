#' Get data from db
#' 
#' @param tname string. The table name of the data.  A table name can contain numeric alphabets and underline (e.g. table_1 ). The table name  should begin with an alphabet. The 8 table names below are specified:
#'                 * "edge": matrix data for network.
#'                 * "dict_cui": dict of CUI nodes.
#'                 * "dict_codified": dict of codified nodes.
#'                 * "synonyms": synonyms of CUI nodes.
#'                 * "rollup": rollup for CUI nodes.
#'                 * some other names can't be used: "details", "ids", "node".
#' @param db string. The database path to write to. e.g. "./test.db".
#' @param n number. Default 0. To get the nth row data from db. If 0, will get all the data.
#' @param id string. Default NULL. To get the data of the id.
#' @return A dataframe.
#' @examples
#' \dontrun{
#' getData("details", "test_data2db.rds")
#' }
#' @export
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

#' Get cos data from edge table of db
#' 
#' @rdname getData
#' @param n number/string. Row number or id to get from the edge table.
#' @param db string. The database path to write to. e.g. "./test.db".
#' @return A dataframe.
#' @examples
#' \dontrun{
#' getCosFromDB(1, "ids", "test.db")
#' }
#' @export
getCosFromDB <- function(n, db){
  ids <- getData("ids", db)
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
    data.frame(from = ids$id[ids$index == n],
               to = ids$id[match(as.numeric(strsplit(data$to, ";")[[1]]), ids$index)],
               cos = as.numeric(strsplit(data$cos, ";")[[1]]),
               row.names = NULL)
  }
}
