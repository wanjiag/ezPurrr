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
#' @noRd

sampling_check = function(nest_df, index){

  if(!"data" %in% colnames(nest_df)) {
    stop(
    'The dataframe does not contain a column named "data" with data frame type.
     Please double check your data frame.
     Did you use `purrr::nest()` for your data frame first?'
    )
  }

  if (!missing(index)){
    if (index > nrow(nest_df)) {
      stop(
        paste0(
          'Index must be a numeric integer and range between 1 and ',
          nrow(nest_df),
          '.'
        )
      )
    }
    if (!is_integer(index)) {
      stop('Index must be an integer.')
    }
    n = index
  }
  else {
    n = sample(seq_len(nrow(nest_df)), 1)
    message(paste0('row ', n, ' is selected randomly'))
  }
  n
}

#' Checks if a number is an integer
#' @param x A number to check
#' @return Logical, with \code{TRUE} if \code{x} is an integer.
#' @keywords Internal
#' @noRd
is_integer = function(x) {
  x %% 1 == 0
}


#' Sampling one row from a nested dataframe. The sampling could be done
#' either randomly or a specific row could be indicated.
#'
#' The default type of return is `list`, which will return the data frame
#' as the first item, and grouping variables as other items in the list.
#' If the return type is indicated as `df`, this function can also return
#' a dataframe with the nested data, and grouping variables will be
#' transformed as columns with identical values.
#'
#' @param nest_df The nested dataframe to be sampled from.
#'                The right-most column must be a dataframe type with name 'data'.
#' @param index Optional. The index of the dataframe to be sampled from. If no index is
#'              provided, a random subset will be returned.
#' @param type Optional. 'list' or 'df'. The type of return data structure.
#'             List is the default.
#' @export
#'

sample_row = function(nest_df, index, type = 'list'){

  n = sampling_check(nest_df, index)

  if (type == 'list'){

    df = list(nest_df$data[n][[1]])

    group = nest_df[n, c(seq_len(ncol(nest_df)) - 1)]

    group_name = group_vars(group)
    list_name = c('data')

    for (i in group_name){
      df = append(df, list(group[{{i}}][[1]]))
      list_name = c(list_name, i)
    }

    names(df) <- list_name

  }

  if (type == 'df'){

    df = nest_df$data[n][[1]]

    group = nest_df[n, c(seq_len(ncol(nest_df)) - 1)]

    group_name = group %>% group_vars()

    for (i in group_name){
      name = paste('group', i, sep = '.')
      df[{{name}}] = group[{{i}}]
    }

  }

  df

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

sample_group = function(nest_df, index){

  n = sampling_check(nest_df, index)

  nest_df[n, c(seq_len(ncol(nest_df)) - 1)]

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

sample_data = function(nest_df, index){

  n = sampling_check(nest_df, index)

  nest_df$data[n][[1]]

}
