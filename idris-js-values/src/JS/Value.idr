module JS.Value

data Kind
  = KindUndefined
  | KindNull
  | KindBool
  | KindNumber
  | KindString
  | KindArray
  | KindObject

export
record Value where
  constructor MkValue
  valueVal : Ptr String

public export
interface Encode x where
  encode : x -> Value

public export
interface Decode x where
  decode : Value -> Maybe x

%foreign "javascript:lambda: (x,y,z) => console.log(x,y,z)"
prim_conlog : a -> PrimIO ()

%foreign "javascript:lambda: function() { return {}; }"
prim_NewJSObject : Unit -> PrimIO (Ptr String)

%foreign "javascript:lambda: s => { try { return JSON.parse(s); } catch(e) { return undefined; } }"
prim_Parse : String -> PrimIO (Ptr String)

%foreign "javascript:lambda: v => JSON.stringify(v)"
prim_Stringify : Ptr String -> PrimIO String

%foreign "javascript:lambda: u => (u === undefined) ? 1 : 0"
prim_IsUndefined : Ptr String -> PrimIO Int

%foreign "javascript:lambda: n => (n === null) ? 1 : 0"
prim_IsNull : Ptr String -> PrimIO Int

%foreign "javascript:lambda: a => (a.length !== undefined) ? 1 : 0"
prim_ArrayLike : Ptr String -> PrimIO Int

%foreign "javascript:lambda: s => (typeof(s) === typeof('')) ? 1 : 0"
prim_StringLike : Ptr String -> PrimIO Int

%foreign "javascript:lambda: x => (typeof(x) === typeof(1) || typeof(x) === typeof(1n)) ? 1 : 0"
prim_NumberLike : Ptr String -> PrimIO Int

%foreign "javascript:lambda: b => (b === true || b === false) ? 1 : 0"
prim_BoolLike : Ptr String -> PrimIO Int

%foreign "javascript:lambda: o => { try { return Object.keys(o); } catch(e) { return undefined; } }"
prim_Object_Keys : Ptr String -> PrimIO (Ptr String)

%foreign "javascript:lambda: arr => { try { return BigInt(arr.length); } catch(e) { return BigInt(0); } }"
arrayLength_ : Ptr String -> PrimIO Int

%foreign "javascript:lambda: (arr,idx) => { if (idx < arr.length && idx >= 0) { return arr[idx]; } else { return undefined; } }"
arrayIndex_ : Ptr String -> Int -> PrimIO (Ptr String)

%foreign "javascript:lambda: s => { try { return s.toString(); } catch(e) { return ''; } }"
prim_toString : Ptr String -> PrimIO String

%foreign "javascript:lambda: (obj,key) => { try { return obj[key]; } catch(e) { return undefined; } }"
prim_getKey : Ptr String -> String -> PrimIO (Ptr String)

%foreign "javascript:lambda: i => parseInt(i.toString())"
prim_jsInt : Int -> PrimIO (Ptr String)

%foreign "javascript:lambda: _ => 1"
prim1 : a -> PrimIO Int

%foreign "javascript:lambda: _ => true"
prim_True : a -> PrimIO (Ptr String)

%foreign "javascript:lambda: _ => false"
prim_False : a -> PrimIO (Ptr String)

%foreign "javascript:lambda: s => s"
prim_String : String -> PrimIO (Ptr String)

%foreign "javascript:lambda: (obj,key,val) => { try { obj[key] = val; } catch(e) { } }"
prim_setKey : Ptr String -> String -> Ptr String -> PrimIO ()

%foreign "javascript:lambda: _ => []"
prim_EmptyArray : Unit -> PrimIO (Ptr String)

%foreign "javascript:lambda: (a,v) => { try { a.push(v); } catch(e) { } }"
prim_ArrayPush : Ptr String -> Ptr String -> PrimIO ()

%foreign "javascript:lambda: i => parseInt(i.toString())"
prim_toInt : Ptr String -> PrimIO Int

%foreign "javascript:lambda: i => BigInt(i)"
prim_toBigInt : Ptr String -> PrimIO Int

%foreign "javascript:lambda: x => (!!x) ? 1 : 0"
prim_intTruthy : Ptr String -> PrimIO Int

primClassify : Ptr String -> IO Kind
primClassify unk = do
  let
    choiceWalk : List (Pair (Ptr String -> PrimIO Int) Kind) -> IO Kind
    choiceWalk [] = pure KindUndefined
    choiceWalk ((pred,val) :: tl) = do
      result <- primIO $ pred unk
      if result > 0 then pure val else choiceWalk tl

  choiceWalk
    [ (prim_IsUndefined, KindUndefined)
    , (prim_IsNull, KindNull)
    , (prim_BoolLike, KindBool)
    , (prim_StringLike, KindString)
    , (prim_NumberLike, KindNumber)
    , (prim_ArrayLike, KindArray)
    , (prim1, KindObject)
    ]

public export
kind : Value -> Kind
kind (MkValue v) = unsafePerformIO $ primClassify v

wrap : Ptr String -> Value
wrap v = MkValue v

public export
newJSObject : Unit -> IO Value
newJSObject _ = do
  v <- primIO $ prim_NewJSObject ()
  pure $ wrap v

public export
stringify : Value -> String
stringify (MkValue v) = unsafePerformIO $ primIO $ prim_Stringify v

public export
parse : String -> Maybe Value
parse s = unsafePerformIO $ do
  parsed <- primIO $ prim_Parse s
  let
    wrapped = wrap parsed
  case kind wrapped of
    KindUndefined => pure $ Nothing
    _ => pure $ Just wrapped

public export
arrayLength : Value -> Int
arrayLength (MkValue a) = unsafePerformIO $ do
  primIO $ arrayLength_ a

public export
unrollArray : Value -> List Value
unrollArray (MkValue v) = unsafePerformIO $ do
  let
    returnBinData : Ptr String -> Int -> IO (List Value)
    returnBinData rawBinData rawLen =
      let
        arrayContent : Int -> Int -> IO (List Value)
        arrayContent 0 len = do
          elt <- primIO $ arrayIndex_ rawBinData (len - 1)
          pure $ [wrap elt]
        arrayContent n len = do
          elt <- primIO $ arrayIndex_ rawBinData (len - n - 1)
          rest <- arrayContent (n - 1) len
          pure $ (wrap elt) :: rest
      in
      arrayContent (rawLen - 1) rawLen

  binLen <- primIO $ arrayLength_ v
  if binLen == 0 then pure [] else returnBinData v binLen

public export
toString : Value -> String
toString (MkValue v) = unsafePerformIO $ primIO $ prim_toString v

public export
getKey : Value -> String -> Value
getKey (MkValue o) key = unsafePerformIO $ do
  res <- primIO $ prim_getKey o key
  pure $ wrap res

public export
keys : Value -> List String
keys (MkValue v) = unsafePerformIO $ do
  res <- primIO $ prim_Object_Keys v
  pure $ map toString $ unrollArray (wrap res)

public export
implementation Encode Value where
  encode v = v

public export
implementation Decode Value where
  decode v = Just v

public export
jsInt : Int -> Value
jsInt i = unsafePerformIO $ do
  ii <- primIO $ prim_jsInt i
  pure $ wrap ii

fromJSInt : Value -> Int
fromJSInt (MkValue v) = unsafePerformIO $ do
  res <- primIO $ prim_toInt v
  pure res

public export
implementation Encode Int where
  encode = jsInt

public export
implementation Decode Int where
  decode v =
    case kind v of
      KindNumber => Just $ fromJSInt v
      _ => Nothing

public export
implementation Encode Bool where
  encode True = wrap $ unsafePerformIO $ primIO $ prim_True ()
  encode False = wrap $ unsafePerformIO $ primIO $ prim_False ()

public export
truthy : Value -> Bool
truthy (MkValue v) = unsafePerformIO $ do
  tval <- primIO $ prim_intTruthy v
  pure $ tval > 0

public export
implementation Decode Bool where
  decode v =
    case kind v of
      KindBool => Just $ truthy v
      _ => Nothing

public export
data Object = ObjectKeys (List (Pair String Value))

jsonEncodeObjectFromList : List (Pair String Value) -> Value -> IO ()
jsonEncodeObjectFromList [] v = pure ()
jsonEncodeObjectFromList ((k,(MkValue v)) :: tl) (MkValue o) = do
  primIO $ prim_setKey o k v
  jsonEncodeObjectFromList tl (MkValue o)

public export
implementation Encode Object where
  encode (ObjectKeys l) = unsafePerformIO $ do
    baseObject <- newJSObject ()
    jsonEncodeObjectFromList l baseObject
    pure $ baseObject

public export
implementation Decode Object where
  decode v =
    let
      ks = keys v
      decoded : Unit -> List (Pair String Value)
      decoded _ = map (\k => (k, getKey v k)) ks
    in
    case kind v of
      KindObject => Just $ ObjectKeys $ decoded ()
      _ => Nothing

public export
implementation Encode String where
  encode s = unsafePerformIO $ do
    sp <- primIO $ prim_String s
    pure $ wrap sp

public export
implementation Decode String where
  decode s =
    case kind s of
      KindString => Just $ toString s
      _ => Nothing

public export
emptyArray : Unit -> IO Value
emptyArray _ = do
  arr <- primIO $ prim_EmptyArray ()
  pure $ wrap arr

pushArray : Value -> Value -> IO ()
pushArray (MkValue arr) (MkValue v) = primIO $ prim_ArrayPush arr v

pushArrayFromList : Encode a => Value -> List a -> IO ()
pushArrayFromList a@(MkValue arr) [] = pure ()
pushArrayFromList a@(MkValue arr) (hd :: tl) = do
  let
    (MkValue enc) = encode hd

  primIO $ prim_ArrayPush arr enc
  pushArrayFromList a tl

public export
implementation Encode a => Encode (List a) where
  encode [] = unsafePerformIO $ emptyArray ()
  encode l = unsafePerformIO $ do
    arr <- emptyArray ()
    pushArrayFromList arr l
    pure arr

catMaybes : List (Maybe a) -> List a
catMaybes [] = []
catMaybes ((Just a) :: tl) = a :: (catMaybes tl)
catMaybes (Nothing :: tl) = catMaybes tl

public export
implementation Decode a => Decode (List a) where
  decode v =
    case kind v of
      KindArray =>
        let
          rlist = catMaybes $ map decode $ unrollArray v
        in
        Just rlist
      _ => Nothing

public export
implementation Decode a => Decode (Maybe a) where
  decode v =
    case kind v of
      KindUndefined => Just Nothing
      KindNull => Just Nothing
      _ => Just $ decode v
