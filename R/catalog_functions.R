#' Function to get sql list version of pmap data catalog
#'
#' Will return a connection to the pmap data catalog
#' @param dbpath string path to the sql list db
#' @export
#' @import data.table
#'
get_catalog_connection <- function(dbpath) {
  con <- DBI::dbConnect(RSQLite::SQLite(), dbpath)
  return(con)
}

#' return column information
#'
#' Function will return information from master table
#' in the sqlite db
#' @param column string name of column to find
#' @param con connection to the sqllite db, default is dbcon
#' @export
column_info <- function(column, con=dbcon, verbose=T) {
  #search for this column in master table
  result = dplyr::tbl(src=con,"master_table") %>%
    dplyr::filter(Column == column) %>%
    dplyr::collect()

  if(verbose) {
    cols = colnames(result)
    for(i in seq_len(nrow(result))) {
      for(j in seq_len(length(cols))) {
        cat(cols[j],": ", result[i,j][[1]], "\n", sep = "")
      }
      cat("\n")
    }
    return(invisible())
  } else {
    return(result)
  }
}

#' return table information
#'
#' Function will return information from master table
#' in the sqlite db given a table  name
#' @param table string name of table to find
#' @param con connection to the sqllite db, default is dbcon
#' @export
table_info <- function(table, con=dbcon) {
  #search for this table in master table
  result = dplyr::tbl(src=con,"master_table") %>%
    dplyr::filter(Table == table) %>%
    dplyr::collect()

  return(result)
}

#' Return available catalog tables
#'
#' Function will return a list of tables that are available
#' in the sqlite connection
#' @param con sqlite connection, default is dbcon
#' @export
catalog_tables <- function(con=dbcon) {
  tables <- dplyr::tbl(src=con,"master_table") %>%
    dplyr::distinct(Table) %>%
    dplyr::collect() %>%
    dplyr::pull()

  return(tables)
}

keyword_search <- function(keyword,ignore_case = TRUE,con=dbcon) {

  target = stringr::regex(keyword,ignore_case = ignore_case)
  mtable=data.table::setDT(
    dplyr::tbl(src=con,"master_table") %>%
      dplyr::collect()
  )
  found_rows = rowSums(mtable[,lapply(.SD, stringr::str_detect,pattern=target)],na.rm=T)>0
  if(any(found_rows)) {
    return(mtable[found_rows])
  } else {
    cat("Nothing found")
    return(invisible(NULL))
  }

}


