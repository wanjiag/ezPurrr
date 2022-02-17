
#' broadcasting a function to all other rows
#'
#' This function will return a dataframe with last item being the result of broadcasting.
#'
#' @param nest_df The nested data frame to be broadcasted.
#'                Must include a column as dataframe type with name 'data'.
#' @param f The function to be broadcasted.
#' @param output_type The expected output column type. The default is a list.
#'                    Options: 'list'; 'double'
#'
#' @export


broadcast <- function(nest_df, f, output_type = 'list'){

  # TODO: adding optional input for grouping variables

  if (output_type == 'list'){
    new_df = mutate(nest_df, output = pmap(.l = list(nest_df$data),
                           .f = f))
  }

  if (output_type == 'double'){
    new_df = mutate(nest_df, output = map_dbl(.x = nest_df$data,
                              .f = f))
  }

  new_df
}


