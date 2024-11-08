# Tests for get_lat_long()

## get_lat_long() ----

test_that("get_lat_long() stops work", {
  address <- '3286 E Vera Cir'
  zipcode <- 84121

    out <- get_lat_long(address = address, zipcode = zipcode, apikey = NULL)

    expect_equal(class(out), 'numeric')
    expect_equal(length(out), 2)


    zipcode <- '84121'
    out <- get_lat_long(address = address, zipcode = zipcode, apikey = NULL)
    expect_equal(class(out), 'numeric')
    expect_equal(length(out), 2)
    expect_equal(names(out), c('lat', 'long'))

    expect_error(get_lat_long(address = 1234, zipcode = zipcode, apikey = NULL))

})

