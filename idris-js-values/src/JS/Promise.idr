module JS.Promise

import Data.IORef

public export
data Promise : Type -> Type -> Type where
  MkPromise : Maybe e -> Maybe t -> Ptr String -> Promise e t

public export
%foreign "javascript:lambda: (wrap,x) => wrap(new Promise((e,r) => { console.log(x); r(); }))"
prom_conlog : (Ptr String -> Promise Void ()) -> a -> Promise Void ()

%foreign "javascript:lambda: (unwrap,wrap,prom,fn) => wrap((unwrap prom).then((a) => unwrap(fn(a))))"
prom_Then : (Promise e a -> Ptr String) -> (Ptr String -> Promise e b) -> Promise e a -> (a -> Promise e b) -> Promise e b

%foreign "javascript:lambda: (wrap,pconst) => wrap(new Promise((e,r) => r(pconst)))"
prom_Const : (Ptr String -> Promise e t) -> t2 -> Promise e t2

prom_Unwrap : Promise x y -> Ptr String
prom_Unwrap (MkPromise _ _ pi) = pi

public export
conlog : a -> Promise Void ()
conlog v =
  let
    maker : Ptr String -> Promise Void ()
    maker = MkPromise Nothing Nothing
  in
  prom_conlog maker v

public export
const_promise : (v : t) -> Promise e t
const_promise v =
  let
    maker : Ptr String -> Promise e t
    maker = MkPromise Nothing Nothing

    p : Promise e t
    p = prom_Const maker v
  in
  p

public export
performPromise : Promise e t -> IO ()
performPromise p =
  pure ()

public export
implementation Functor (Promise e) where
  map f p = map_f f p
  where
    map_f : (a -> b) -> Promise e a -> Promise e b
    map_f f p =
      let
        maker : Ptr String -> Promise e b
        maker = MkPromise Nothing Nothing

        mapper : b -> Promise e b
        mapper = const_promise

        mapped : a -> Promise e b
        mapped v = mapper (f v)
      in
      prom_Then prom_Unwrap maker p mapped

public export
implementation Applicative (Promise e) where
  pure v = const_promise v
  (<*>) f p = apply_f f p
  where
    apply_f : Promise e (a -> b) -> Promise e a -> Promise e b
    apply_f f p =
      let
        maker : Ptr String -> Promise e b
        maker = MkPromise Nothing Nothing
      in
      prom_Then prom_Unwrap maker f (\ff => map ff p)

public export
implementation Monad (Promise Void) where
    (>>=) v f = bind v f
    where
      bind : (Promise Void a) -> (a -> Promise Void b) -> Promise Void b
      bind v f =
        let
          maker : Ptr String -> Promise Void b
          maker = MkPromise Nothing Nothing
        in
        prom_Then prom_Unwrap maker v (\vv => f vv)

public export
implementation Monad (Promise e) where
    (>>=) v f = bind v f
    where
      bind : (Promise e a) -> (a -> Promise e b) -> Promise e b
      bind v f =
        let
          maker : Ptr String -> Promise e b
          maker = MkPromise Nothing Nothing
        in
        prom_Then prom_Unwrap maker v (\vv => f vv)
