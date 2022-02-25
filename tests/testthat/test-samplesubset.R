Name <- c("Jon", "Bill", "Maria", "Ben", "Tina")
Age <- c(23, 41, 32, 58, 26)
Gender <- c('M', 'M', 'F', 'M', 'F')

df <- data.frame(Name, Age, Gender)

test_that("Correct structure of the data frame", {
  expect_error(
    sample_row(df),
    'The dataframe does not contain a column named "data" with data frame type'
    )
})

nest_df <- df %>% dplyr::group_by(Gender) %>%
  tidyr::nest()

test_that("correct index", {
  expect_error(
    sample_row(nest_df, index = 'error'),
    'Index must be a numeric integer and range between 1 and 2.'
  )
  expect_error(
    sample_row(nest_df, index = 1.2),
    'Index must be an integer.'
  )
})

test_that("index sampling", {
  expect_equal(
    sample_row(nest_df, index = 1),
    list(nest_df[1,c(1:ncol(nest_df)-1)], nest_df[1,]$data[[1]]))
  expect_equal(
    sample_group(nest_df, index = 1),
    nest_df[1,c(1:ncol(nest_df)-1)])
  expect_equal(
    sample_data(nest_df, index = 2),
    nest_df[2,]$data[[1]])
})

set.seed(315)

set.seed(315)
test_that("random sampling and warning message", {
  expect_message(v1 <- sample_row(nest_df), "row 1 is selected randomly")
  expect_message(v2 <- sample_group(nest_df), "row 2 is selected randomly")
  expect_message(v3 <- sample_data(nest_df), "row 1 is selected randomly")
  expect_equal(
    v1, list(nest_df[1,c(1:ncol(nest_df)-1)],nest_df[1,]$data[[1]]))
  expect_equal(
    v2,nest_df[2,c(1:ncol(nest_df)-1)])
  expect_equal(
    v3, nest_df[1,]$data[[1]])
})

