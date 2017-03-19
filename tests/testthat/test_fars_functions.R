context("Testing fars functions")

test_that("Testing fars functions", {

  # In make_filename() function
  expect_equal(make_filename(2013), "accident_2013.csv.bz2")
  expect_equal(make_filename(2014), "accident_2014.csv.bz2")
  expect_equal(make_filename(2015), "accident_2015.csv.bz2")


  # In fars_read() function
  # The output must be a data.frame
  expect_is(fars_read("accident_2013.csv.bz2"), "data.frame")
  expect_is(fars_read("accident_2014.csv.bz2"), "data.frame")
  expect_is(fars_read("accident_2015.csv.bz2"), "data.frame")

  # Through error if the file does not exist
  expect_error(fars_read("/home/kanna/accident_2013.csv.bz2"),
               "file '/home/kanna/accident_2013.csv.bz2' does not exist")


  # In fars_read_years() function
  expect_warning(fars_read_years(2012), "invalid year: 2012")
  expect_warning(fars_read_years(2016), "invalid year: 2016")

  # Output must be a list
  expect_is(fars_read_years(2013), "list")
  expect_is(fars_read_years(2014), "list")
  expect_is(fars_read_years(2015), "list")
  expect_is(fars_read_years(c(2013,2015)), "list")

  # The list item must be a data.frame
  expect_is(fars_read_years(c(2013,2015))[[1]], "data.frame")
  expect_is(fars_read_years(c(2013,2015))[[2]], "data.frame")


  # Number of columns of output should be equal to number of years in input + 1
  expect_equal(ncol(fars_summarize_years(2013)), 2)
  expect_equal(ncol(fars_summarize_years(c(2013,2015))), 3)
  expect_equal(ncol(fars_summarize_years(c(2013,2014,2015))), 4)


  # In fars_map_state() function
  # Checking error for invalid state number
  expect_error(fars_map_state(150,2013), "invalid STATE number: 150")
})

