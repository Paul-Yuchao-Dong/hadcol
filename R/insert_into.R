#' Insert_into
#' insert a dataframe / col into anther data.frame .
#' @param x a data.frame
#' @param y a data.frame
#' @param where an integer
#'
#' @return the combined data.frame
#' @export
#'
#' @examples
insert_into <- function(x, y, where = 1){
  if (where == 1){
    cbind(y,x)
  } else if (where > ncol(x)){
    cbind(x,y)
  } else {
    lhs <- 1:(where - 1)
    cbind(x[lhs], y, x[-lhs])
  }
}
