
#' broadcasting a function to all other rows
#'
#' This function will return a dataframe with last item (named `output`)
#' being the result of broadcasting.
#'
#' @param nest_df The nested data frame to be broadcasted.
#'                Must include a column as dataframe type with name 'data'.
#' @param f The function to be broadcasted.
#' @param output_type The expected output column type. The default is a list.
#'                    Options: 'list'; 'double'
#' @param ... Additional arguments passed to \code{f}.
#'
#' @export

broadcast = function(nest_df, f, output_type = 'list', ...){

  # TODO: adding optional input for grouping variables

  if (output_type == 'list'){
      output = pmap(
        .l = list(nest_df$data),
        .f = f,
        ...
        )
  }

  if (output_type == 'double'){
      output = map_dbl(
        .x = nest_df$data,
        .f = f,
        ...
      )
  }

  nest_df$output = output
  nest_df
}

#' broadcasting a function with grouping variables to all other rows
#'
#' This function will return a dataframe with last item (named `output`)
#'  being the result of broadcasting.
#'
#' @param nest_df The nested data frame to be broadcasted.
#'                Must include a column as dataframe type with name 'data'.
#' @param f The function to be broadcasted.
#'
#' @export
#'
broadcast_group = function(nest_df, f){

  v = regmatches(deparse(plotting)[1], gregexpr("(?<=\\().*?(?=\\))", deparse(plotting)[1], perl=T))[[1]]

  v2 = str_split(v, pattern = ',')[[1]]

  command = 'output = pmap(list('
  for (i in v2){
    i = str_trim(i)
    command = paste0(command, i, '=nest_df$', i, ',')
  }
  command = substr(command,1,nchar(command)-1)
  command = paste0(command, '),~{f(')

  for (i in c(1:length(v2))){
    command = paste0(command, '..', i, ',')
  }
  command = substr(command,1,nchar(command)-1)
  command = paste0(command, ')})')

  eval(parse(text = command))

  nest_df$output = output
  nest_df

}
