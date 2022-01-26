
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

  if(index > nrow(nest_df)){
    stop(paste0('Index must be a numeric integer and range between 1 and ',
                nrow(nest_df),
                '.'))}

  if(index%%1!=0){
    stop('Index must be an integer.')}

  if(!"data" %in% colnames(nest_df)){
    stop('The dataframe does not contain a column named "data" with data frames.
         Please double check your data frame.
         Did you use nest() for your data frame first?')}

  if(missing(index)){
    print('random')
    n <- sample(1:nrow(nest_df), 1)
  }
  else{
    n = index
  }

  list(subset(nest_df[n,], select = -data), nest_df$data[n][[1]])

}

# Unsolved Warnings:

# Non-standard file/directory found at top level:
#  ‘LICENSE.md’

# samplesubset: no visible binding for global variable ‘data’
# Undefined global functions or variables:
#  data
# Consider adding
# importFrom("utils", "data")
# to your NAMESPACE file.
