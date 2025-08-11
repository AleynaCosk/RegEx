# Existing
describe('Given Character Limitation',{
  it('exist',{
    # Given
    character.limitation |> expect.exist()
  })
})

# Availability
describe("When character.limitation is called on simple input", {
  it("Then it returns JSON with text and flag", {
    res <- character.limitation("abc", 5)
    expect_true(is.character(res)); expect_length(res, 1L)

    j <- jsonlite::fromJSON(res, simplifyVector = FALSE)
    expect_identical(j$limit, 5L)
    expect_equal(length(j$results), 1L)
    expect_identical(j$results[[1]]$text, "abc")
    expect_true(isTRUE(j$results[[1]]$within))
    expect_identical(j$results[[1]]$length, 3L)
  })
})

# Functionality
test_that("character.limitation is vectorized and counts after removing line breaks", {
  x <- c("hi", "hello\nworld", NA_character_, "")
  res_json <- character.limitation(x, 5, output = "json")

  j <- jsonlite::fromJSON(res_json, simplifyVector = FALSE)

  # JSON top-level
  expect_true(is.list(j))
  expect_equal(j$limit, 5)               
  expect_equal(length(j$results), length(x))

  # Item 1: "hi" -> within
  expect_identical(j$results[[1]]$text, "hi")
  expect_true(isTRUE(j$results[[1]]$within))
  expect_equal(j$results[[1]]$length, 2)

  # Item 2: "hello\nworld" -> "helloworld" length 10 -> not within
  expect_identical(j$results[[2]]$text, "hello\nworld")
  expect_false(isTRUE(j$results[[2]]$within))
  expect_equal(j$results[[2]]$length, 10)

  # Item 3: NA -> nulls in JSON
  expect_true(is.null(j$results[[3]]$text))
  expect_true(is.null(j$results[[3]]$within))
  expect_true(is.null(j$results[[3]]$length))

  # Item 4: "" at limit 5 -> within
  expect_identical(j$results[[4]]$text, "")
  expect_true(isTRUE(j$results[[4]]$within))
  expect_equal(j$results[[4]]$length, 0)
})

test_that("character.limitation returns logical flags when output='logical'", {
  x <- c("hi", "hello world", NA_character_)
  out <- character.limitation(x, 5, output = "logical")
  expect_true(is.logical(out))
  expect_equal(out, c(TRUE, FALSE, NA))
})

test_that("character.limitation validates inputs", {
  expect_error(character.limitation("a", -1))
  expect_error(character.limitation("a", c(1, 2)))
  expect_error(character.limitation("a", NA_real_))
})