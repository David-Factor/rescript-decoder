type value = Js.Json.tagged_t

type rec error =
  | Field(string, error)
  | Index(int, error)
  | OneOf(list<error>)
  | Failure(string, value)

type decoder<'a> = value => result<'a, error>

let failure = (msg, json) => Failure(`Expecting ${msg}`, json)->Error

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
          decoder(tagged)->ResultExtra.mapError(x => Field(name, x))
        }
      | None => toFailure
      }
    }
  | _ => toFailure
  }
}

let at = (path, decoder) => Array.reduceReverse2(path, decoder, field)

let value = (json: value) => Ok(json)

let map = (fn, decode, json) => Result.map(decode(json), fn)

let map2 = (fn, decoderA, decoderB, json) => ResultExtra.map2(fn, decoderA(json), decoderB(json))

let apply = (decoderA: decoder<'a>, decoderB: decoder<'a => 'b>): decoder<'b> =>
  map2((a, b) => b(a), decoderA, decoderB)

let map3 = (fn: ('a, 'b, 'c) => 'd, a: decoder<'a>, b: decoder<'b>, c: decoder<'c>): decoder<'d> =>
  map(fn, a)->apply(b, _)->apply(c, _)

let map4 = (fn, a, b, c, d) => map(fn, a)->apply(b, _)->apply(c, _)->apply(d, _)
let map5 = (fn, a, b, c, d, e) => map(fn, a)->apply(b, _)->apply(c, _)->apply(d, _)->apply(e, _)
let map6 = (fn, a, b, c, d, e, f) =>
  map(fn, a)->apply(b, _)->apply(c, _)->apply(d, _)->apply(e, _)->apply(f, _)
let map7 = (fn, a, b, c, d, e, f, g) =>
  map(fn, a)->apply(b, _)->apply(c, _)->apply(d, _)->apply(e, _)->apply(f, _)->apply(g, _)
let map8 = (fn, a, b, c, d, e, f, g, h) =>
  map(fn, a)
  ->apply(b, _)
  ->apply(c, _)
  ->apply(d, _)
  ->apply(e, _)
  ->apply(f, _)
  ->apply(g, _)
  ->apply(h, _)

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
