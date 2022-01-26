
#' sampling a subset of data from a nested data frame
#'
#' This function will return a list with the first item being the grouping
#' variable of the data frame, and second item being the subsetted datafram.
#'
#' @param nest_df The nested data frame to be subset from
#' @param index The index of the data set to be returned from. If no index is
#'              provided, a random subset will be returned. need to indicate this is optional
#'
#' @export
#'
samplesubset <- function(nest_df, index){

  if(missing(index)){
    n <- sample(1:nrow(nest_df), 1)
  }
  else{
    n = index
  }

  list(subset(nest_df[n,], select = -data), nest_df$data[n][[1]])

}

#samplesubset: no visible binding for global variable ‘data’
#Undefined global functions or variables:
#  data
#Consider adding
#importFrom("utils", "data")
#to your NAMESPACE file.
