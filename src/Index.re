open Express;

let checkProperty = (req, next, property, k, res) => {
  let reqData = Request.asJsonObject(req);
  switch (Js.Dict.get(reqData, property)) {
  | None => next(Next.route, res)
  | Some(x) =>
    switch (Js.Json.decodeBoolean(x)) {
    | Some(b) when b => k(res)
    | _ => next(Next.route, res)
    }
  };
};

/* same as [checkProperty] but with a list of properties */
let checkProperties = (req, next, properties, k, res) => {
  let rec aux = properties =>
    switch (properties) {
    | [] => k(res)
    | [p, ...tl] => checkProperty(req, next, p, (_) => aux(tl), res)
    };
  aux(properties);
};

/* [setProperty req property] sets the [property] in the [req] Request
   value */
let setProperty = (req, property, res) => {
  let reqData = Request.asJsonObject(req);
  Js.Dict.set(reqData, property, Js.Json.boolean(true));
  res;
};

/* return the string value for [key], None if the key is not in [dict]
   TODO once BOption.map is released */
let getDictString = (dict, key) =>
  switch (Js.Dict.get(dict, key)) {
  | Some(json) => Js.Json.decodeString(json)
  | _ => None
  };

/* make a common JSON object representing success */
let makeSuccessJson = () => {
  let json = Js.Dict.empty();
  Js.Dict.set(json, "success", Js.Json.boolean(true));
  Js.Json.object_(json);
};

let app = express();

let raiseIfNone =
  fun
  | Some(value) => value
  | None => failwith("Body is none");



let onListen = e =>
  switch (e) {
  | exception (Js.Exn.Error(e)) =>
    Js.log(e);
    Node.Process.exit(1);
  | _ => Js.log @@ "Listening at http://127.0.0.1:3000"
  };

let server = App.listen(app, ~port=3000, ~onListen, ());

let countRequestsInJavascript: (HttpServer.t, unit) => int = [%bs.raw
  {|
    function setupEnpointWithHttpServer(server) {
      let count = 0;
      server.on('request', (req, res) => ++count)
      return () => {
        const result = count;
        count = -1 // reset the count
        return result
      }
    }
  |}
];

let getRequestsCount = countRequestsInJavascript(server);



App.disable(app, ~name="x-powered-by");

App.useOnPath(app, ~path="/") @@
Middleware.from((next, req, res) =>
  res |> setProperty(req, "middleware0") |> next(Next.middleware)
) /* call the next middleware in the processing pipeline */;

App.useWithMany(
  app,
  [|
    Middleware.from((next, req) =>
      checkProperty(req, next, "middleware0", res =>
        res |> setProperty(req, "middleware1") |> next(Next.middleware)
      )
    ),
    Middleware.from((next, req) =>
      checkProperties(req, next, ["middleware0", "middleware1"], res =>
        next(Next.middleware, setProperty(req, "middleware2", res))
      )
    ),
  |],
);

App.get(app, ~path="/") @@
Middleware.from((next, req) => {
  let previousMiddlewares = ["middleware0", "middleware1", "middleware2"];
  checkProperties(
    req,
    next,
    previousMiddlewares,
    Response.sendJson(makeSuccessJson()),
  );
});

