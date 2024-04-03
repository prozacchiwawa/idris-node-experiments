module Main

import Decidable.Equality
import Data.Maybe
import Data.String
import Node.Http
import Node.WebSocket
import Bacon.JS
import JS.Value

record AppState where
  constructor MkAppState
  numEvents : Int

data IncomingEffect
  = IBumpCount Int

data OutboundEffect
  = OCounted Int

public export
implementation Decode IncomingEffect where
  decode val = do
    k <- decode $ getKey val "increment"
    pure $ IBumpCount k
    
public export
implementation Encode OutboundEffect where
  encode (OCounted i) =
    encode $ ObjectKeys
      [ ("count", encode i) ]

public export
parseRPCMessage : String -> Maybe IncomingEffect
parseRPCMessage s = do
  value <- parse s
  decode value

pushOutMsgs : BaconBus a -> List a -> IO ()
pushOutMsgs bus [] = pure ()
pushOutMsgs bus (hd :: tl) = do
  push bus hd
  pushOutMsgs bus tl

consumeEvent : BaconBus OutboundEffect -> AppState -> IncomingEffect -> IO AppState
consumeEvent outbus astate msg = do
  let
    new_count =
      case msg of
        IBumpCount n => (numEvents astate) + n

  pushOutMsgs outbus [OCounted new_count]
  pure $ record { numEvents = new_count } astate

main : IO ()
main = do
  httpMod <- getHttp ()
  server <- createServer httpMod $ \(request,response) => do
    putStrLn $ "idk"
    responseWriteHead response 404
    responseEnd response

  wss <- newWebSocketServer server
  inBus <- newBus Nothing
  outBus <- newBus Nothing

  let
    inObserver = getObserver inBus
    appState = MkAppState 0

  -- Automatically update some state for the user as we process each bus event.
  foldBus <- Bacon.JS.foldM (consumeEvent outBus) appState inObserver

  -- A bacon stream fold returns the last acc when upstream ends.
  consume foldBus $ \endState => do
    putStrLn $ "program ended after " ++ show (numEvents endState) ++ " events"

  wssOnRequest wss $ \request => do
    origin <- requestOrigin request
    putStrLn $ "connected " ++ origin
    connection <- wsrAccept request "count-proto" origin

    -- Broadcasting replies is ok since msgs contain identifiable txids.
    let
      rpcReply : OutboundEffect -> IO ()
      rpcReply eff = wscSendUTF connection $ stringify $ encode eff

    consume (getObserver outBus) rpcReply

    wscOnClose connection $ \_ => putStrLn "closed"

    wscOnMessage connection $ \m => do
      received <- wsmUtfData m

      case parseRPCMessage received of
        Nothing => pure ()
        Just msg => push inBus msg
