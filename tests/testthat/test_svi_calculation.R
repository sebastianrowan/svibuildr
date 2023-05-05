test_that(
  "Refuses to calculate SVI for year != 2020",
  {
    expect_error(
      calculate_svi("county", year = 2016, state = "nh"),
      "SVI calculation currently only available for"
    )
  }
)

test_that(
  "Refuses to request invalid geography",
  {
    expect_error(
      calculate_svi("block", year = 2020, state = "nh"),
      "Geography not valid."
    )
    expect_error(
      calculate_svi(c("tract", "county"), year = 2020, state = "nh"),
      "Geography not valid."
    )
  }
)

test_that(
  "Refuses to download and calculate SVI when state is NULL",
  {
    expect_error(
      calculate_svi("county", year = 2020),
      "No state specified"
    )
  }
)

test_that(
  "Refuses to download and calculate US SVI",
  {
    expect_error(
      calculate_svi("county", year = 2020, state = "us"),
      "Calculating US svi not currently supported"
    )
  }
)

test_that(
  "Rejects when geometry is not a logical",
  {
    expect_error(
      calculate_svi("county", year = 2020, state = "nh", geometry = "TRUE"),
      "Expected logical value for geometry."
    )
    expect_error(
      calculate_svi("county", year = 2020, state = "nh", geometry = 1),
      "Expected logical value for geometry."
    )
    expect_error(
      calculate_svi("county", year = 2020, state = "nh", geometry = NULL),
      "Expected logical value for geometry."
    )
  }
)

test_that(
  "Rejects invalid moe_level",
  {
    expect_error(
      calculate_svi("county", year = 2020, state = "nh", moe = 96),
      "Invalid moe_level."
    )
    expect_error(
      calculate_svi("county", year = 2020, state = "nh", moe = c(90, 95)),
      "Invalid moe_level."
    )
  }
)

test_that(
  "Properly downloads and calculates single state 2020 SVI",
  {
    x <- calculate_svi("county", year = 2020, state = "nh")
    expect_s3_class(x, "data.frame")
  }
)

test_that(
  "Properly downloads and calculates multi-state 2018 or 2020 SVI",
  {
    x <- calculate_svi("county", year = 2020, state = c(33, "Vermont", "Me"))
    y <- calculate_svi("county", year = 2018, state = c(33, "Vermont", "Me"))
    expect_s3_class(x, "data.frame")
  }
)

test_that(
  "Properly downloads and calculates 2018 or 2020 SVI with adjunct vars",
  {
    expect_s3_class(calculate_svi("county", year = 2020, state = 33, include_adjunct_vars = TRUE), "data.frame")
    expect_s3_class(calculate_svi("county", year = 2018, state = 33, include_adjunct_vars = TRUE), "data.frame")
  }
)
