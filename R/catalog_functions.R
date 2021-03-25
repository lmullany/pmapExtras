#' Function to get sql list version of pmap data catalog
#'
#' Will return a connection to the pmap data catalog
#' @param dbpath string path to the sql list db
#' @export
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
column_info <- function(column, con=dbcon) {
  #search for this column in master table
  result = dplyr::tbl(src=con,"master_table") %>%
    dplyr::filter(Column == column) %>%
    dplyr::collect()

  return(result)


}
