module Decode = {
  type value = Js.Json.tagged_t

  type rec error =
    | Field(string, error)
    | Index(int, error)
    | OneOf(list<error>)
    | Failure(string, value)

  type decoder<'a> = value => result<'a, error>

  let failure = (msg, json) => Failure(`Expecting ${msg}`, json)->Error

  module ResultX = {
    let mapError = (result, f) => {
      switch result {
      | Ok(a) => Ok(a)
      | Error(err) => f(err)->Error
      }
    }

    let map2 = (f, ra, rb) => {
      switch ra {
      | Error(x) => Error(x)
      | Ok(a) =>
        switch rb {
        | Error(x) => Error(x)
        | Ok(b) => Ok(f(a, b))
        }
      }
    }

    let map3 = (f, ra, rb, rc) => {
      switch ra {
      | Error(x) => Error(x)
      | Ok(a) =>
        switch rb {
        | Error(x) => Error(x)
        | Ok(b) =>
          switch rc {
          | Error(x) => Error(x)
          | Ok(c) => Ok(f(a, b, c))
          }
        }
      }
    }
  }

  let string = json => {
    open Js
    switch json {
    | Json.JSONString(v) => Ok(v)
    | _ => failure(`a STRING`, json)
    }
  }

  let bool = json => {
    switch json {
    | Js.Json.JSONTrue => Ok(true)
    | Js.Json.JSONFalse => Ok(false)
    | _ => failure(`a STRING`, json)
    }
  }

  let field = (name, decoder, json) => {
    open Js
    let toFailure = failure(`expecting an object with a field ${name}`, json)
    switch json {
    | Json.JSONObject(pairs) => {
        let entry = Dict.get(pairs, name)
        switch entry {
        | Some(val) => {
            let tagged = Json.classify(val)
            decoder(tagged)->ResultX.mapError(x => Field(name, x))
          }
        | None => toFailure
        }
      }
    | _ => toFailure
    }
  }

  let at = (path, decoder) => Array.reduceReverse2(path, decoder, field)

  let value = (json: value) => Ok(json)

  let map = (f, decode, json) => Result.map(decode(json), f)

  let map2 = (f, decoderA, decoderB, json) => ResultX.map2(f, decoderA(json), decoderB(json))

  let map3 = (f, decoderA, decoderB, decoderC, json) =>
    ResultX.map3(f, decoderA(json), decoderB(json), decoderC(json))

  /* Fancy primitives */

  let flatMap = (f, decode, json) => Result.flatMap(decode(json), f)

  let succeed = (a, _) => Ok(a)
  let fail = (msg, _) => flatMap(v => Failure(msg, v)->Error, value)

  let lazy_ = (thunk: unit => decoder<'a>): decoder<'a> => flatMap(succeed(), thunk())

  let run = (raw, decoder) => {
    open Js
    let json = try Json.parseExn(raw) catch {
    | _ => failwith("Error parsing JSON string")
    }

    Json.classify(json)->decoder
  }
}

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
