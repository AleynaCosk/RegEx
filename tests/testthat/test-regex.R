# Existing
describe('Given Character Limitation',{
  it('exist',{
    # Given
    character.limitation |> expect.exist()
  })
})

# Availability
describe("When character.limitation is called on simple input", {
  it("Then it returns an integer flag", {
    out <- character.limitation("abc", 5)
    out |> expect.integer()  
    expect_length(out, 1L)
  })
})
