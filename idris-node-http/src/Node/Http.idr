module Node.Http

%foreign "javascript:lambda:s=>(s!==null && s!==undefined)?1:0"
prim_stringTruthy : Ptr String -> PrimIO Int

%foreign "javascript:lambda:s=>s.toString()"
prim_realizeString : Ptr String -> PrimIO String

%foreign "node:lambda:(_,x,w) => console.log(x)"
prim_consoleLog : a -> PrimIO ()

public export
record Http where
  constructor MkHttp
  httpImpl : Ptr String

public export
record HttpRequest where
  constructor MkHttpRequest
  httpRequestImpl : Ptr String

public export
record HttpResponse where
  constructor MkHttpResponse
  httpResponseImpl : Ptr String

public export
record HttpServer where
  constructor MkHttpServer
  httpServerImpl : Ptr String
