-- Incoming

foreign import jsevent "play"
    (JS.fromRecord {x = 0, y = 0})
    newStamps : Signal JavaScript.JSObject

process : JavaSript.JSObject -> Maybe (Int, Int)
process object = case object |> Json.fromJSObject of
    Json.Object dict -> case dict |> Dict.lookup "x" of
        Just (Json.Number x) -> case dict |> Dict.lookup "y" of
            Just (Json.Number y) -> Just {x = x, y = y}
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing

plays = (foldp (::) [] (newStamps ~> process)) ~> justs


-- Outgoing
firebaseRequest requestType requestData = Http.request requestType "https://.."

serialize r = r |> JS.fromRecord |> Json.fromJSObject |> Json.toJSString "" |> JavaSript.toString

toRequestData (x,y) = {x = x, y = y} |> serialize

clicks = Mouse.osition |> sampleOn Mouse.clicks

toRequest click = case click of
    (0, 0) -> firebaseRequest "get" ""
    _      -> firebaseRequest "post" (click |> toRequestData)

requests = click ~> toRequest

sendRequests = Http.send requests

