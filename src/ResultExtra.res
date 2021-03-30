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

// todo define in terms of apply
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
