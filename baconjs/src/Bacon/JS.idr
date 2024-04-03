module Bacon.JS

import Data.IORef

public export
record BaconBus inputType where
  constructor MkBaconBus
  baconBusImpl : Ptr String

public export
record BaconObserver outputType where
  constructor MkBaconObserver
  baconObserverImpl : Ptr String

%foreign "javascript:lambda: (_,x) => console.log(x)"
conlog : a -> PrimIO ()

%foreign "javascript:lambda: () => require('baconjs')"
prim_RequireBacon : Unit -> PrimIO (Ptr String)

%foreign "javascript:lambda: bacon => new bacon.Bus()"
prim_BaconBus : Ptr String -> PrimIO (Ptr String)

public export
newBus : Applicative f => f a -> IO (BaconBus a)
newBus _ = do
  required <- primIO $ prim_RequireBacon ()
  busCore <- primIO $ prim_BaconBus required
  pure $ MkBaconBus busCore

%foreign "javascript:lambda: (_,bus,val) => bus.push(val)"
prim_Bus_Push : Ptr String -> a -> PrimIO ()

public export
push : BaconBus inputType -> inputType -> IO ()
push (MkBaconBus bus) elt = do
  primIO $ prim_Bus_Push bus elt

public export
getObserver : BaconBus etype -> BaconObserver etype
getObserver (MkBaconBus i) = MkBaconObserver i

%foreign "javascript:lambda: (_,obs,fn,ext_0) => obs.onValue(elt => fn(elt)(elt,ext_0))"
prim_Obs_onValue : Ptr String -> (a -> IO ()) -> PrimIO ()

%foreign "javascript:lambda: (obs,ext_0) => obs.onEnd(elt => fn(elt)(ext_0))"
prim_Obs_onEnd : Ptr String -> (() -> IO ()) -> PrimIO ()

%foreign "javascript:lambda: (bacon,v) => bacon.once(v)"
prim_BaconOnce : Ptr String -> a -> PrimIO (Ptr String)

%foreign "javascript:lambda: (a,b) => a.concat(b)"
prim_BusConcat : Ptr String -> Ptr String -> PrimIO ()

%foreign "javascript:lambda: bus => bus.end()"
prim_BusEnd : Ptr String -> PrimIO ()

public export
consume : BaconObserver outputType -> (outputType -> IO ()) -> IO ()
consume (MkBaconObserver obs) action = do
  primIO $ prim_Obs_onValue obs action

public export
end : BaconBus t -> IO ()
end (MkBaconBus b) = do
  primIO $ prim_BusEnd b

implementation Functor BaconObserver where
  map f obs@(MkBaconObserver o) =
    unsafePerformIO $ do
      txBus <- newBus Nothing
      let
        txObserver = getObserver txBus

      primIO $ prim_Obs_onValue o $ \elt => do
        push txBus $ f elt

      pure txObserver

implementation Semigroup (BaconObserver t) where
  (<+>) (MkBaconObserver a) (MkBaconObserver b) =
    unsafePerformIO $ do
      txBus@(MkBaconBus bus) <- newBus Nothing
      let
        txObserver = getObserver txBus
      primIO $ prim_BusConcat bus a
      primIO $ prim_BusConcat bus b
      primIO $ prim_BusEnd bus
      pure $ txObserver

implementation Applicative BaconObserver where
  pure v =
    unsafePerformIO $ do
      required <- primIO $ prim_RequireBacon ()
      onced <- primIO $ prim_BaconOnce required v
      pure $ MkBaconObserver onced

  (<*>) fObs vObs@(MkBaconObserver o) =
    unsafePerformIO $ do
      outBus@(MkBaconBus b) <- newBus Nothing
      consume fObs \f => do
        consume vObs \e => push outBus $ f e
      primIO $ prim_Obs_onEnd o $ \_ => do
        primIO $ prim_BusEnd b
      pure $ getObserver outBus

implementation Monoid (BaconObserver t) where
  neutral = unsafePerformIO $ do
    bus <- newBus Nothing
    let
      obs = getObserver bus
    end bus
    pure obs

public export
foldM : (acc -> elt -> IO acc) -> acc -> BaconObserver elt -> IO (BaconObserver acc)
foldM f start obs@(MkBaconObserver o) = do
  txBus <- newBus Nothing
  resultRef <- newIORef start
  primIO $ prim_Obs_onEnd o $ \_ => do
    val <- readIORef resultRef
    push txBus val
  consume obs $ \elt => do
    oldVal <- readIORef resultRef
    newVal <- f oldVal elt
    writeIORef resultRef newVal
  pure $ getObserver txBus
