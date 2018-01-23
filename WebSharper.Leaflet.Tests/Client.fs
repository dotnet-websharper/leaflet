namespace WebSharper.Leaflet.Tests

open WebSharper
open WebSharper.JavaScript
open WebSharper.Html.Client

[<JavaScript>]
module Client =

    [<SPAEntryPoint>]
    let Main () =
        let coordinates = Div []
        let elt =
            Div [
                Div [Attr.Style "height: 600px;"]
                |>! OnAfterRender (fun div ->
                    let map = Leaflet.Map(div.Dom)
                    map.SetView((47.49883, 19.0582), 14)
                    map.AddLayer(
                        Leaflet.TileLayer(
                            Leaflet.TileLayer.OpenStreetMap.UrlTemplate,
                            Leaflet.TileLayer.Options(
                                Attribution = Leaflet.TileLayer.OpenStreetMap.Attribution)))
                    map.AddLayer(
                        let m = Leaflet.Marker((47.4952, 19.07114))
                        m.BindPopup("IntelliFactory")
                        m)
                    map.On_mousemove(fun map ev ->
                        coordinates.Text <- "Position: " + ev.Latlng.ToString())
                    map.On_mouseout(fun map ev ->
                        coordinates.Text <- "")
                )
                coordinates
            ]
        (elt :> IControlBody).ReplaceInDom(JS.Document.QuerySelector "#main")
