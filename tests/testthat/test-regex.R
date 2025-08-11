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
describe("When character.limitation(text, limitation) is called", {
  it("Then returns 1L when nchar(text) <= limitation and 0L when > limitation", {
    expect_equal(character.limitation("hello", 5), 1L)   # boundary ==
    expect_equal(character.limitation("hello", 4), 0L)   # over limit
    expect_equal(character.limitation("", 0), 1L)        # empty at 0
  })

  it("Then preserves NA and returns integer type", {
    out <- character.limitation(NA_character_, 5)
    expect_equal(out, NA_integer_)
    expect_true(is.integer(out))
  })

  it("Then errors on invalid 'limitation' (negative / length != 1 / NA)", {
    expect_error(character.limitation("a", -1))
    expect_error(character.limitation("a", c(1, 2)))
    expect_error(character.limitation("a", NA_real_))
  })
})

