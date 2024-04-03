module Node.WebSocket

import Node.Http

%foreign "javascript:lambda:s=>(s!==null && s!==undefined)?1:0"
prim_stringTruthy : Ptr String -> PrimIO Int

%foreign "javascript:lambda:s=>s.toString()"
prim_realizeString : Ptr String -> PrimIO String

%foreign "javascript:lambda:(_,x,w) => console.log(x)"
prim_consoleLog : a -> PrimIO ()

public export
record WebSocketServer where
  constructor MkWebSocketServer
  wssImpl : Ptr String

public export
record WssRequest where
  constructor MkWssRequest
  wssRequestImpl : Ptr String

public export
record WssConnection where
  constructor MkWssConnection
  wssConnectionImpl : Ptr String

public export
record WssMessage where
  constructor MkWssMessage
  wssMessageImpl : Ptr String

%foreign "javascript:lambda: _ => require('websocket').server"
getWebSocketServerCtor_ : Unit -> PrimIO (Ptr String)

%foreign "javascript:lambda: _ => require('http')"
getHttp_ : Unit -> PrimIO (Ptr String)

%foreign "javascript:lambda: function(http,fn,ext_0) { return http.createServer((request,response) => { const obj = {'request':request,'response':response}; fn(obj)(obj,ext_0); }); }"
httpCreateServer_ : Ptr String -> (Ptr String -> IO ()) -> PrimIO (Ptr String)

%foreign "javascript:lambda: function(http,port,fn) { const p = port.toString(); http.listen(parseInt(p), fn); }"
httpListen_ : Ptr String -> Int -> (() -> IO ()) -> PrimIO ()

%foreign "javascript:lambda: cbreq => cbreq['request']"
getHttpRequest_ : Ptr String -> PrimIO (Ptr String)

%foreign "javascript:lambda: cbreq => cbreq['response']"
getHttpResponse_ : Ptr String -> PrimIO (Ptr String)

%foreign "javascript:lambda: function(response,code) { response.writeHead(parseInt(code.toString())); }"
responseWriteHead_ : Ptr String -> Int -> PrimIO ()

%foreign "javascript:lambda: function(response) { response.end(); }"
responseEnd_ : Ptr String -> PrimIO ()

%foreign "javascript:lambda: function(server,wss) { return new wss({ httpServer: server, autoAcceptConnections: false }) }"
newWebSocketServer_ : Ptr String -> Ptr String -> PrimIO (Ptr String)

%foreign "javascript:lambda: (wss,fn,ext_0) => wss.on('request', (req) => fn(req)(req,ext_0))"
wssOnRequest_ : Ptr String -> (Ptr String -> IO ()) -> PrimIO ()

%foreign "javascript:lambda: request => request['origin']"
requestOrigin_ : Ptr String -> PrimIO (Ptr String)

%foreign "javascript:lambda: wsr => wsr.reject()"
wsrReject_ : Ptr String -> PrimIO ()

%foreign "javascript:lambda: (wsr,proto,origin) => wsr.accept(proto,origin)"
wsrAccept_ : Ptr String -> String -> String -> PrimIO (Ptr String)

%foreign "javascript:lambda: (wsc,str) => wsc.sendUTF(str)"
wscSendUTF_ : Ptr String -> String -> PrimIO ()

%foreign "javascript:lambda: (wsc,fn,ext_0) => wsc.on('message', (msg) => fn(msg)(msg,ext_0))"
wscOnMessage_ : Ptr String -> (Ptr String -> IO ()) -> PrimIO ()

%foreign "javascript:lambda: (wsc,fn,ext_0) => wsc.on('close', () => fn()(ext_0))"
wscOnClose_ : Ptr String -> (() -> IO ()) -> PrimIO ()

%foreign "javascript:lambda: wsm => wsm['type']"
wsmType_ : Ptr String -> PrimIO String

%foreign "javascript:lambda: wsm => { if (wsm['type'] == 'utf8') { return wsm['utf8Data']; } else { return ''; } }"
wsmUtfData_ : Ptr String -> PrimIO String

%foreign "javascript:lambda: wsm => { if (wsm['type'] == 'binary') { return wsm['binaryData']; } else { return []; } }"
wsmBinaryData_ : Ptr String -> PrimIO (Ptr String)

%foreign "javascript:lambda: arr => BigInt(arr.length)"
arrayLength_ : Ptr String -> PrimIO Int

%foreign "javascript:lambda: (arr,idx) => { if (idx < arr.length && idx >= 0) { return BigInt(arr[idx]); } else { return BigInt(0); } }"
intArrayIndex_ : Ptr String -> Int -> PrimIO Int

public export
getWebSocketServer : Unit -> IO WebSocketServer
getWebSocketServer _ = do
  rawSvr <- primIO $ getWebSocketServerCtor_ ()
  pure $ MkWebSocketServer rawSvr

public export
getHttp : Unit -> IO Http
getHttp _ = do
  rawMod <- primIO $ getHttp_ ()
  pure $ MkHttp rawMod

public export
newWebSocketServer : HttpServer -> IO WebSocketServer
newWebSocketServer http = do
  wssc <- primIO $ getWebSocketServerCtor_ ()
  server <- primIO $ newWebSocketServer_ (httpServerImpl http) wssc
  pure $ MkWebSocketServer server

httpWrap : (Pair HttpRequest HttpResponse -> IO ()) -> Ptr String -> IO ()
httpWrap fn cbreq = do
  bareReq <- primIO $ getHttpRequest_ cbreq
  bareResp <- primIO $ getHttpResponse_ cbreq
  fn (MkHttpRequest bareReq, MkHttpResponse bareResp)

public export
createServer : Http -> (Pair HttpRequest HttpResponse -> IO ()) -> IO HttpServer
createServer http fn = do
  let
    wrappedFn : Ptr String -> IO ()
    wrappedFn = httpWrap fn

  server <- primIO $ httpCreateServer_ (httpImpl http) wrappedFn
  primIO $ httpListen_ server 8080 $ \_ => do
    primIO $ prim_consoleLog "listening http 8080"
  pure $ MkHttpServer server

public export
responseWriteHead : HttpResponse -> Int -> IO ()
responseWriteHead (MkHttpResponse r) code = do
  primIO $ responseWriteHead_ r code

public export
responseEnd : HttpResponse -> IO ()
responseEnd (MkHttpResponse r) = do
  primIO $ responseEnd_ r

wssRequestFun : (WssRequest -> IO ()) -> Ptr String -> IO ()
wssRequestFun fn ptr = fn (MkWssRequest ptr)

public export
wssOnRequest : WebSocketServer -> (WssRequest -> IO ()) -> IO ()
wssOnRequest wss fn =
  primIO $ wssOnRequest_ (wssImpl wss) (wssRequestFun fn)

public export
requestOrigin : WssRequest -> IO String
requestOrigin wss = do
  origin <- primIO $ requestOrigin_ $ wssRequestImpl wss
  truthy <- primIO $ prim_stringTruthy origin
  if truthy > 0 then primIO $ prim_realizeString origin else pure "*"

public export
wsrReject : WssRequest -> IO ()
wsrReject wsr = do
  primIO $ wsrReject_ $ wssRequestImpl wsr

public export
wsrAccept : WssRequest -> String -> String -> IO WssConnection
wsrAccept wsr proto origin = do
  accepted <- primIO $ wsrAccept_ (wssRequestImpl wsr) proto origin
  pure $ MkWssConnection accepted

public export
wscSendUTF : WssConnection -> String -> IO ()
wscSendUTF wsc msg = do
  primIO $ wscSendUTF_ (wssConnectionImpl wsc) msg

public export
wscOnMessage : WssConnection -> (WssMessage -> IO ()) -> IO ()
wscOnMessage wsc fn = do
  primIO $ wscOnMessage_ (wssConnectionImpl wsc) $ \rawMsg => do
    fn $ MkWssMessage rawMsg

public export
wscOnClose : WssConnection -> (() -> IO ()) -> IO ()
wscOnClose wsc fn = do
  primIO $ wscOnClose_ (wssConnectionImpl wsc) fn

public export
wsmType : WssMessage -> IO String
wsmType wsm = do
  primIO $ wsmType_ $ wssMessageImpl wsm

public export
wsmUtfData : WssMessage -> IO String
wsmUtfData wsm = do
  primIO $ wsmUtfData_ $ wssMessageImpl wsm

public export
wsmBinaryData : WssMessage -> IO (List Int)
wsmBinaryData wsm = do
  let
    returnBinData : Ptr String -> Int -> IO (List Int)
    returnBinData rawBinData rawLen =
      let
        arrayContent : Int -> Int -> IO (List Int)
        arrayContent 0 len = do
          elt <- primIO $ intArrayIndex_ rawBinData (len - 1)
          pure $ [elt]
        arrayContent n len = do
          elt <- primIO $ intArrayIndex_ rawBinData (len - n - 1)
          rest <- arrayContent (n - 1) len
          pure $ elt :: rest
      in
      arrayContent (rawLen - 1) rawLen

  rawBinData <- primIO $ wsmBinaryData_ $ wssMessageImpl wsm
  rawLen <- primIO $ arrayLength_ rawBinData
  if rawLen == 0 then pure [] else returnBinData rawBinData rawLen
