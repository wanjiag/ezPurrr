
#' Sanity check for input dataframe, then return the index of the row to be sampled from.
#'
#' This function will return an index to be sampled from the input dataframe.
#'
#' @param nest_df The nested dataframe to be sampled from.
#'                The right-most column must be a dataframe type with name 'data'.
#' @param index Optional. The index of the dataframe to be sampled from. If no index is
#'              provided, a random subset will be returned.
#'
#' @keywords Internal
#'

sampling_check <- function(nest_df, index){

  if(!"data" %in% colnames(nest_df)){
    stop('The dataframe does not contain a column named "data" with data frame type.
           Please double check your data frame.
           Did you use nest() for your data frame first?')}

  if(!missing(index)){
    if(index > nrow(nest_df)){
      stop(paste0('Index must be a numeric integer and range between 1 and ',
                  nrow(nest_df),
                  '.'))}

    if(index%%1!=0){
      stop('Index must be an integer.')}

    n = index

  }
  else{
    print('a row is selected randomly')
    n <- sample(1:nrow(nest_df), 1)
  }

  n

}


#' Sampling one row from a nested dataframe. The sampling could be done
#' either randomly or a specific row could be indicated.
#'
#' This function will return a list with the first item being the grouping
#' variable of the dataframe, and second item being the nested data.
#'
#' @param nest_df The nested dataframe to be sampled from.
#'                The right-most column must be a dataframe type with name 'data'.
#' @param index Optional. The index of the dataframe to be sampled from. If no index is
#'              provided, a random subset will be returned.
#'
#' @export
#'

sample_row <- function(nest_df, index){

  n = sampling_check(nest_df, index)

  list(nest_df[n,c(1:ncol(nest_df)-1)], nest_df$data[n][[1]])

}


#' Sampling one grouping variable from a nested dataframe. The sampling could be done
#' either randomly or a specific row could be indicated.
#'
#' This function will return a dataframe of the grouping
#' variable from the input nested dataframe.
#'
#' @param nest_df The nested dataframe to be sampled from.
#'                The right-most column must be a dataframe type with name 'data'.
#' @param index Optional. The index of the dataframe to be sampled from. If no index is
#'              provided, a random subset will be returned.
#'
#' @export
#'

sample_grouping <- function(nest_df, index){

  n = sampling_check(nest_df, index)

  nest_df[n,c(1:ncol(nest_df)-1)]

}


#' Sampling one data frame from a nested dataframe. The sampling could be done
#' either randomly or a specific row could be indicated.
#'
#' This function will return a dataframe from the list of data frames from the
#' input nested dataframe.
#'
#' @param nest_df The nested dataframe to be sampled from.
#'                The right-most column must be a dataframe type with name 'data'.
#' @param index Optional. The index of the dataframe to be sampled from. If no index is
#'              provided, a random subset will be returned.
#'
#' @export
#'

sample_data <- function(nest_df, index){

  n = sampling_check(nest_df, index)

  nest_df$data[n][[1]]

}
