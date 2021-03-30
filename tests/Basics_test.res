type person = {
  eyes: string,
  head: bool,
}

let makePerson = (eyes, head) => {
  eyes: eyes,
  head: head,
}

let personDecoder = Decode.map2(
  makePerson,
  Decode.field("eyes", Decode.string),
  Decode.field("head", Decode.bool),
)

type foo = {
  hello: string,
  blah: string,
  person: person,
}

let makeFoo = (hello, blah, person) => {
  hello: hello,
  blah: blah,
  person: person,
}

let fooDecoder = Decode.map3(
  makeFoo,
  Decode.field("hello", Decode.string),
  Decode.field("blah", Decode.string),
  Decode.field("person", personDecoder),
)

let go = Decode.run(
  `{"hello":"Penny","blah": "Dave","person":{"eyes": "blue", "head": false}}`,
  fooDecoder,
)

Js.log2(">>>", go)

let intEqual = (~message=?, a: int, b: int) =>
  Test.assertion(~message?, ~operator="intEqual", (a, b) => a === b, a, b)

let stringEqual = (~message=?, a: string, b: string) =>
  Test.assertion(~message?, ~operator="stringEqual", (a, b) => a == b, a, b)

let equals = (~message=?, left, right) =>
  Test.assertion((left, right) => left == right, left, right, ~operator="left == right", ~message?)

Test.test("Primitive decoding", () => {
  equals(go, {hello: "Penny", blah: "Dave", person: {eyes: "blue", head: false}}->Ok)
})

type x = Foo(string)

let xDecoder = Decode.map(x => Foo(x), Decode.string)

Test.test("Decode to variant", () => {
  equals(go, {hello: "Penny", blah: "Dave", person: {eyes: "blue", head: false}}->Ok)
})

Test.test("Decode oneOf", () => {
  equals(
    Decode.run("1.1", Decode.oneOf(list{Decode.map(_ => 1.1, Decode.string), Decode.float})),
    Ok(1.1),
  )
})
