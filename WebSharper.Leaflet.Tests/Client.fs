// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
//
// Licensed under the Apache License, Version 2.0 (the "License"); you
// may not use this file except in compliance with the License.  You may
// obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied.  See the License for the specific language governing
// permissions and limitations under the License.
//
// $end{copyright}
namespace WebSharper.Leaflet.Tests

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI
open WebSharper.UI.Client
open WebSharper.UI.Html

[<JavaScript>]
module Client =

    [<SPAEntryPoint>]
    let Main () =
        let coordinates = div [] [] :?> Elt
        div [] [
            div [
                attr.style "height: 600px;"
                on.afterRender (fun div ->
                    let map = Leaflet.Map(div)
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
            ] []
            coordinates
        ]
        |> Doc.RunById "main"
