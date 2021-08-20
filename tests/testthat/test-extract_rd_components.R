dplyr_arrange <-
    tools::parse_Rd("./example_rd_files/arrange.Rd")

dplyr_filter_joins <-
    tools::parse_Rd("./example_rd_files/filter-joins.Rd")

dplyr_mj <-
    tools::parse_Rd("./example_rd_files/mutate-joins.Rd")

dplyr_gba <-
    tools::parse_Rd("./example_rd_files/group_by_all.Rd")

test_that("extract_rd_components parses examples section correctly", {

  skip("Need to finish work on parsing examples and properly dealing with donttest etc")

  rd_components_arr <- extract_rd_components(dplyr_arrange)
  rd_components_fj <- extract_rd_components(dplyr_filter_joins)
  rd_components_mj <- extract_rd_components(dplyr_mj)
  rd_components_gba <- extract_rd_components(dplyr_gba)


  expect_identical(
    rd_components_arr$examples,
    c(
      "arrange(mtcars, cyl, disp)",
      "arrange(mtcars, desc(disp))",
      "by_cyl <- mtcars %>% group_by(cyl)",
      "by_cyl %>% arrange(desc(wt))",
      "by_cyl %>% arrange(desc(wt), .by_group = TRUE)",
      "tidy_eval_arrange <- function(.data, var) {\n    .data %>% arrange({\n        {\n            var\n        }\n    })\n}",
      "tidy_eval_arrange(mtcars, mpg)",
      "iris %>% arrange(across(starts_with(\"Sepal\")))",
      "iris %>% arrange(across(starts_with(\"Sepal\"), desc))"
    )
  )

  expect_identical(
    rd_components_fj$examples,
    c(
      "band_members %>% semi_join(band_instruments)",
      "band_members %>% anti_join(band_instruments)",
      "band_members %>% semi_join(band_instruments, by = \"name\")"
    )
  )

  expect_identical(
    rd_components_mj$examples,
    c(
      "band_members %>% inner_join(band_instruments)",
      "band_members %>% left_join(band_instruments)",
      "band_members %>% right_join(band_instruments)",
      "band_members %>% full_join(band_instruments)",
      "band_members %>% inner_join(band_instruments, by = \"name\")",
      "band_members %>% full_join(band_instruments2, by = c(name = \"artist\"))",
      "band_members %>% full_join(band_instruments2, by = c(name = \"artist\"), keep = TRUE)",
      "df1 <- tibble(x = 1:3)",
      "df2 <- tibble(x = c(1, 1, 2), y = c(\"first\", \"second\", \"third\"))",
      "df1 %>% left_join(df2)",
      "df1 <- data.frame(x = c(1, NA), y = 2)",
      "df2 <- data.frame(x = c(1, NA), z = 3)",
      "left_join(df1, df2)",
      "left_join(df1, df2, na_matches = \"never\")"
    )
  )

  expect_identical(
    rd_components_gba$examples,
    c(
      "group_by_all(mtcars)",
      "mtcars %>% group_by(across())",
      "group_by_if(iris, is.factor)",
      "iris %>% group_by(across(where(is.factor)))",
      "group_by_at(mtcars, vars(vs, am))",
      "mtcars %>% group_by(across(c(vs, am)))",
      "d <- tibble(x = c(1, 1, 2, 2), y = c(1, 2, 1, 2))",
      "group_by_all(d, as.factor)",
      "d %>% group_by(across(everything(), as.factor))",
      "group_by_if(iris, is.factor, as.character)",
      "iris %>% group_by(across(where(is.factor), as.character))"
    )
  )


})

test_that("extract_rd_components parses sections correctly", {

  rd_components_arr <- extract_rd_components(dplyr_arrange)
  rd_components_fj <- extract_rd_components(dplyr_filter_joins)
  rd_components_mj <- extract_rd_components(dplyr_mj)
  rd_components_gba <- extract_rd_components(dplyr_gba)

  rd_components <- extract_rd_components(dplyr_arrange)

  expect_identical(rd_components$usage, "arrange(.data, ..., .by_group = FALSE)")
  expect_identical(rd_components$aliases, c("arrange", "arrange.data.frame"))
  expect_identical(rd_components$args, c(".data", "...", ".by_group"))

  rd_components <- extract_rd_components(dplyr_filter_joins)
  expect_identical(
    rd_components$usage,
    c(
      "semi_join(x, y, by = NULL, copy = FALSE, ...)",
      "anti_join(x, y, by = NULL, copy = FALSE, ...)"
    )
  )

  expect_identical(
    rd_components$aliases,
    c(
      "filter-joins",
      "semi_join",
      "semi_join.data.frame",
      "anti_join",
      "anti_join.data.frame"
    )
  )

  expect_identical(rd_components$args,
                   c("x", "y", "by", "copy", "...", "na_matches"))



  rd_components <- extract_rd_components(dplyr_mj)
  expect_identical(
    rd_components$usage,
    c(
      "inner_join(x, y, by = NULL, copy = FALSE, suffix = c(\".x\", \".y\"), ..., keep = FALSE)",
      "left_join(x, y, by = NULL, copy = FALSE, suffix = c(\".x\", \".y\"), ..., keep = FALSE)",
      "right_join(x, y, by = NULL, copy = FALSE, suffix = c(\".x\", \".y\"), ..., keep = FALSE)",
      "full_join(x, y, by = NULL, copy = FALSE, suffix = c(\".x\", \".y\"), ..., keep = FALSE)"
    )
  )

  expect_identical(
    rd_components$aliases,
    c(
      "mutate-joins",
      "join",
      "join.data.frame",
      "inner_join",
      "inner_join.data.frame",
      "left_join",
      "left_join.data.frame",
      "right_join",
      "right_join.data.frame",
      "full_join",
      "full_join.data.frame"
    )
  )

  expect_identical(rd_components$args,
                   c("x", "y", "by", "copy", "suffix", "...", "keep", "na_matches"))

  rd_components <- extract_rd_components(dplyr_gba)
  expect_identical(
    rd_components$usage,
    c(
      "group_by_all(.tbl, .funs = list(), ..., .add = FALSE, .drop = group_by_drop_default(.tbl))",
      "group_by_at(.tbl, .vars, .funs = list(), ..., .add = FALSE, .drop = group_by_drop_default(.tbl))",
      "group_by_if(.tbl, .predicate, .funs = list(), ..., .add = FALSE, .drop = group_by_drop_default(.tbl))"
    )
  )

  expect_identical(rd_components$aliases,
                   c("group_by_all", "group_by_at", "group_by_if"))


  expect_identical(
    rd_components$args,
    c(".tbl", ".funs", "...", ".add", ".drop", ".vars", ".predicate")
  )


})
