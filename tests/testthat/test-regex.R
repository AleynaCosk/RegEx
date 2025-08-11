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
