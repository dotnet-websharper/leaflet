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
namespace WebSharper.Leaflet

open WebSharper
open WebSharper.JavaScript.Dom

module Definition =
    open WebSharper.InterfaceGenerator

    let TileLayerT = Type.New()
    let MapT = Type.New()
    let PopupT = Type.New()
    let TooltipT = Type.New()
    let LatLngT = Type.New()
    let LatLngOrCoords = LatLngT + T<float * float>
    let PointT = Type.New()
    let PointOrCoords = PointT + T<int * int>
    let LatLngBoundsT = Type.New()
    let LatLngBoundsOrCoords = LatLngBoundsT + Type.ArrayOf LatLngOrCoords

    let LatLng =
        let x = T<float>?longitude
        Class "L.LatLng"
        |=> LatLngT
        |+> Static [
            Constructor (T<float>?latitude * T<float>?longitude * !?T<float>?altitude)
            |> WithComment "Creates an object representing a geographical point with the given latitude and longitude (and optionally altitude)."
            "DEG_TO_RAD" =? T<float>
            |> WithComment "A multiplier for converting degrees into radians."
            "RAD_TO_DEG" =? T<float>
            |> WithComment "A multiplier for converting radians into degrees."
            "MAX_MARGIN" =@ T<float>
            |> WithComment "Max margin of error for the equality check."
        ]
        |+> Instance [
            "lat" =? T<float>
            |> WithComment "Latitude in degrees."
            "lng" =? T<float>
            |> WithComment "Longitude in degrees."

            "distanceTo" => LatLngOrCoords ^-> T<float>
            |> WithComment "Returns the distance (in meters) to the given LatLng calculated using the Haversine formula. See description on wikipedia"
            "equals" => LatLngOrCoords ^-> T<bool>
            |> WithComment "Returns true if the given LatLng point is at the same position (within a small margin of error)."
            "toString" => T<unit -> string>
            |> WithComment "Returns a string representation of the point (for debugging purposes)."
            "wrap" => T<float>?left * T<float>?right ^-> LatLngT
            |> WithComment "Returns a new LatLng object with the longitude wrapped around left and right boundaries (-180 to 180 by default)."
            "wrap" => T<unit> ^-> LatLngT
            |> WithComment "Returns a new LatLng object with the longitude wrapped around left and right boundaries (-180 to 180 by default)."
        ]

    let LatLngBounds =
        Class "L.LatLngBounds"
        |=> LatLngBoundsT
        |+> Static [
            Constructor (LatLngOrCoords?southWest * LatLngOrCoords?northEast)
            |> WithComment "Creates a latLngBounds object by defining south-west and north-east corners of the rectangle."
            Constructor (Type.ArrayOf LatLngOrCoords)
            |> WithComment "Creates a LatLngBounds object defined by the geographical points it contains. Very useful for zooming the map to fit a particular set of locations with fitBounds."
        ]
        |+> Instance [
            "extend" => (LatLngOrCoords + LatLngBoundsOrCoords) ^-> T<unit>
            |> WithComment "Extends the bounds to contain the given point or bounds."
            "getSouthWest" => T<unit> ^-> LatLng
            |> WithComment "Returns the south-west point of the bounds."
            "getNorthEast" => T<unit> ^-> LatLng
            |> WithComment "Returns the north-east point of the bounds."
            "getSouthEast" => T<unit> ^-> LatLng
            |> WithComment "Returns the south-east point of the bounds."
            "getNorthWest" => T<unit> ^-> LatLng
            |> WithComment "Returns the north-west point of the bounds."
            "getSouth" => T<unit -> float>
            |> WithComment "Returns the south latitude of the bounds."
            "getNorth" => T<unit -> float>
            |> WithComment "Returns the north latitude of the bounds."
            "getEast" => T<unit -> float>
            |> WithComment "Returns the east longitude point of the bounds."
            "getWest" => T<unit -> float>
            |> WithComment "Returns the west longitude point of the bounds."
            "contains" => LatLngBoundsOrCoords ^-> T<bool>
            |> WithComment "Returns true if the rectangle contains the given one."
            "contains" => LatLngOrCoords ^-> T<bool>
            |> WithComment "Returns true if the rectangle contains the given point."
            "intersects" => LatLngBoundsOrCoords ^-> T<bool>
            |> WithComment "Returns true if the rectangle intersects the given bounds."
            "equals" => LatLngBoundsOrCoords ^-> T<bool>
            |> WithComment "Returns true if the rectangle is equivalent (within a small margin of error) to the given bounds."
            "toBBoxString" => T<unit -> string>
            |> WithComment "Returns a string with bounding box coordinates in a 'southwest_lng,southwest_lat,northeast_lng,northeast_lat' format. Useful for sending requests to web services that return geo data."
            "pad" => T<float> ^-> LatLngBoundsT
            |> WithComment "Returns bigger bounds created by extending the current bounds by a given percentage in each direction."
            "isValid" => T<unit -> bool>
            |> WithComment "Returns true if the bounds are properly initialized."
        ]

    let Point =
        Class "L.Point"
        |=> PointT
        |+> Static [
            Constructor (T<float>?x * T<float>?y)
            |> WithComment "Creates a Point object with the given x and y coordinates. Rounds the x and y values."
            Constructor (T<int>?x * T<int>?y)
            |> WithComment "Creates a Point object with the given x and y coordinates."
        ]
        |+> Instance [
            "x" =? T<int>
            |> WithComment "The x coordinate."
            "y" =? T<int>
            |> WithComment "The y coordinate."

            "add" => PointOrCoords ^-> PointT
            |> WithComment "Returns the result of addition of the current and the given points."
            "subtract" => PointOrCoords ^-> PointT
            |> WithComment "Returns the result of subtraction of the given point from the current."
            "multiplyBy" => T<float> ^-> PointT
            |> WithComment "Returns the result of multiplication of the current point by the given number."
            "divideBy" => T<float> * !?T<bool>?round ^-> PointT
            |> WithComment "Returns the result of division of the current point by the given number. If optional round is set to true, returns a rounded result."
            "distanceTo" => PointOrCoords ^-> T<float>
            |> WithComment "Returns the distance between the current and the given points."
            "clone" => T<unit> ^-> PointT
            |> WithComment "Returns a copy of the current point."
            "round" => T<unit> ^-> PointT
            |> WithComment "Returns a copy of the current point with rounded coordinates."
            "floor" => T<unit> ^-> PointT
            |> WithComment "Returns a copy of the current point with floored coordinates (rounded down)."
            "equals" => PointOrCoords ^-> T<bool>
            |> WithComment "Returns true if the given point has the same coordinates."
            "contains" => PointOrCoords ^-> T<bool>
            |> WithComment "Returns true if the both coordinates of the given point are less than the corresponding current point coordinates (in absolute values)."
            "toString" => T<unit -> string>
            |> WithComment "Returns a string representation of the point for debugging purposes."
        ]

    let Bounds =
        let Bounds = Type.New()
        Class "L.Bounds"
        |=> Bounds
        |+> Static [
            Constructor (PointOrCoords?topLeft * PointOrCoords?bottomRight)
            |> WithComment "Creates a Bounds object from two coordinates (usually top-left and bottom-right corners)."
        ]
        |+> Instance [
            "min" =? Point
            |> WithComment "The top left corner of the rectangle."
            "max" =? Point
            |> WithComment "The bottom right corner of the rectangle."

            "extend" => PointOrCoords ^-> T<unit>
            |> WithComment "Extends the bounds to contain the given point."
            "getCenter" => T<unit> ^-> Point
            |> WithComment "Returns the center point of the bounds."
            "contains" => Bounds ^-> T<bool>
            |> WithComment "Returns true if the rectangle contains the given one."
            "contains" => PointOrCoords ^-> T<bool>
            |> WithComment "Returns true if the rectangle contains the given point."
            "intersects" => Bounds ^-> T<bool>
            |> WithComment "Returns true if the rectangle intersects the given bounds."
            "isValid" => T<unit -> bool>
            |> WithComment "Returns true if the bounds are properly initialized."
            "getSize" => T<unit> ^-> Point
            |> WithComment "Returns the size of the given bounds."
        ]

    let IconOptions =
        Class "L.Icon.Options"
        |+> Static [Constructor T<string>?IconUrl |> WithInline "{iconUrl:$IconUrl}"]
        |+> Instance [
            "iconUrl" =@ T<string>
            |> WithComment "(required) The URL to the icon image (absolute or relative to your script path)."
            "iconRetinaUrl" =@ T<string>
            |> WithComment "The URL to a retina sized version of the icon image (absolute or relative to your script path). Used for Retina screen devices."
            "iconSize" =@ Point
            |> WithComment "Size of the icon image in pixels."
            "iconAnchor" =@ Point
            |> WithComment "The coordinates of the \"tip\" of the icon (relative to its top left corner). The icon will be aligned so that this point is at the marker's geographical location. Centered by default if size is specified, also can be set in CSS with negative margins."
            "shadowUrl" =@ T<string>
            |> WithComment "The URL to the icon shadow image. If not specified, no shadow image will be created."
            "shadowRetinaUrl" =@ T<string>
            |> WithComment "The URL to the retina sized version of the icon shadow image. If not specified, no shadow image will be created. Used for Retina screen devices."
            "shadowSize" =@ Point
            |> WithComment "Size of the shadow image in pixels."
            "shadowAnchor" =@ Point
            |> WithComment "The coordinates of the \"tip\" of the shadow (relative to its top left corner) (the same as iconAnchor if not specified)."
            "popupAnchor" =@ Point
            |> WithComment "The coordinates of the point from which popups will \"open\", relative to the icon anchor."
            "className" =@ T<string>
            |> WithComment "A custom class name to assign to both icon and shadow images. Empty by default."
        ]

    let Icon =
        Class "L.Icon"
        |=> Nested [IconOptions]
        |+> Static [
            Constructor IconOptions
            |> WithComment "Creates an icon instance with the given options."
        ]

    let DivIconOptions =
        Class "L.DivIcon.Options"
        |+> Static [Constructor T<unit> |> WithInline "{}"]
        |+> Instance [
            "iconSize" =@ Point
            |> WithComment "Size of the icon in pixels. Can be also set through CSS."
            "iconAnchor" =@ Point
            |> WithComment "The coordinates of the \"tip\" of the icon (relative to its top left corner). The icon will be aligned so that this point is at the marker's geographical location. Centered by default if size is specified, also can be set in CSS with negative margins."
            "className" =@ T<string>
            |> WithComment "A custom class name to assign to the icon. 'leaflet-div-icon' by default."
            "html" =@ T<string>
            |> WithComment "A custom HTML code to put inside the div element, empty by default."
        ]

    let DivIcon =
        Class "L.DivIcon"
        |=> Nested [DivIconOptions]
        |=> Inherits Icon
        |+> Static [
            Constructor DivIconOptions
            |> WithComment "Creates a div icon instance with the given options."
        ]

    let Transformation =
        Class "L.Transformation"
        |> WithComment "Represents an affine transformation: a set of coefficients a, b, c, d for transforming a point of a form (x, y) into (a*x + b, c*y + d) and doing the reverse. Used by Leaflet in its projections code."
        |+> Static [
            Constructor (T<float>?a * T<float>?b * T<float>?c * T<float>?d)
        ]
        |+> Instance [
            "transform" => PointOrCoords * T<float>?scale ^-> Point
            |> WithComment "Returns a transformed point, optionally multiplied by the given scale. Only accepts real L.Point instances, not arrays."
            "untransform" => PointOrCoords * T<float>?scale ^-> Point
            |> WithComment "Returns the reverse transformation of the given point, optionally divided by the given scale. Only accepts real L.Point instances, not arrays."
        ]

    let IHandler =
        Interface "L.IHandler"
        |+> [
            "enable" => T<unit -> unit>
            |> WithComment "Enables the handler."
            "disable" => T<unit -> unit>
            |> WithComment "Disables the handler."
            "enabled" => T<unit -> bool>
            |> WithComment "Returns true if the handler is enabled."
        ]

    let ILayer =
        Interface "L.ILayer"
        |+> [
            "onAdd" => MapT ^-> T<unit>
            |> WithComment "Should contain code that creates DOM elements for the overlay, adds them to map panes where they should belong and puts listeners on relevant map events. Called on map.addLayer(layer)."
            "onRemove" => MapT ^-> T<unit>
            |> WithComment "Should contain all clean up code that removes the overlay's elements from the DOM and removes listeners previously added in onAdd. Called on map.removeLayer(layer)."
        ]

    let IControl =
        Interface "L.IControl"
        |+> [
            "onAdd" => MapT ^-> T<unit>
            |> WithComment "Should contain code that creates all the neccessary DOM elements for the control, adds listeners on relevant map events, and returns the element containing the control. Called on map.addControl(control) or control.addTo(map)."
            "onRemove" => MapT ^-> T<unit>
            |> WithComment "Optional, should contain all clean up code (e.g. removes control's event listeners). Called on map.removeControl(control) or control.removeFrom(map). The control's DOM container is removed automatically."
        ]

    let IProjection =
        Interface "L.IProjection"
        |+> [
            "project" => LatLngOrCoords ^-> Point
            |> WithComment "Projects geographical coordinates into a 2D point."
            "unproject" => PointOrCoords ^-> LatLng
            |> WithComment "The inverse of project. Projects a 2D point into geographical location."
        ]

    let ICRS =
        Interface "L.ICRS"
        |+> (
            [
            "latLngToPoint" => LatLngOrCoords * T<int>?zoom ^-> Point
            |> WithComment "Projects geographical coordinates on a given zoom into pixel coordinates." 
            "pointToLatLng" => PointOrCoords * T<int>?zoom ^-> LatLng
            |> WithComment "The inverse of latLngToPoint. Projects pixel coordinates on a given zoom into geographical coordinates."
            "project" => LatLngOrCoords ^-> Point
            |> WithComment "Projects geographical coordinates into coordinates in units accepted for this CRS (e.g. meters for EPSG:3857, for passing it to WMS services)."
            "scale" => T<int>?zoom ^-> T<float>
            |> WithComment "Returns the scale used when transforming projected coordinates into pixel coordinates for a particular zoom. For example, it returns 256 * 2^zoom for Mercator-based CRS."
            "getSize" => T<int>?zoom ^-> Point
            |> WithComment "Returns the size of the world in pixels for a particular zoom."

            "projection" =? IProjection
            |> WithComment "Projection that this CRS uses."
            "transformation" =? Transformation
            |> WithComment "Transformation that this CRS uses to turn projected coordinates into screen coordinates for a particular tile service."
            "code" =? T<string>
            |> WithComment "Standard code name of the CRS passed into WMS services (e.g. 'EPSG:3857')."
        ] : list<CodeModel.IInterfaceMember>)

    let CRS =
        Class "L.CRS"
        |+> Static [
            "EPSG3857" =? ICRS
            |> WithComment "The most common CRS for online maps, used by almost all free and commercial tile providers. Uses Spherical Mercator projection. Set in by default in Map's crs option."
            "EPSG4326" =? ICRS
            |> WithComment "A common CRS among GIS enthusiasts. Uses simple Equirectangular projection."
            "EPSG3395" =? ICRS
            |> WithComment "Rarely used by some commercial tile providers. Uses Elliptical Mercator projection."
            "Simple" =? ICRS
            |> WithComment "A simple CRS that maps longitude and latitude into x and y directly. May be used for maps of flat surfaces (e.g. game maps). Note that the y axis should still be inverted (going from bottom to top)."
        ]

    let Projection =
        Class "L.Projection"
        |+> Static [
            "SphericalMercator" =? IProjection
            |> WithComment "Spherical Mercator projection — the most common projection for online maps, used by almost all free and commercial tile providers. Assumes that Earth is a sphere. Used by the EPSG:3857 CRS."
            "Mercator" =? IProjection
            |> WithComment "Elliptical Mercator projection — more complex than Spherical Mercator. Takes into account that Earth is a geoid, not a perfect sphere. Used by the EPSG:3395 CRS."
            "LonLat" =? IProjection
            |> WithComment "Equirectangular, or Plate Carree projection — the most simple projection, mostly used by GIS enthusiasts. Directly maps x as longitude, and y as latitude. Also suitable for flat worlds, e.g. game maps. Used by the EPSG:3395 and Simple CRS."
        ]

    let Event =
        Class "L.Event"
        |+> Instance [
            "type" =? T<string>
            |> WithComment "The event type (e.g. 'click')."
            "target" =? T<obj>
            |> WithComment "The object that fired the event."
        ]

    let KeyboardEvent =
        Class "L.KeyboardEvent"
        |=> Inherits Event
        |+> Instance [
            "originalEvemt" =? T<Element>
        ]

    let MouseEvent =
        Class "L.MouseEvent"
        |=> Inherits Event
        |+> Instance [
            "latlng" =? LatLng
            |> WithComment "The geographical point where the mouse event occured."
            "layerPoint" =? Point
            |> WithComment "Pixel coordinates of the point where the mouse event occured relative to the map layer."
            "containerPoint" =? Point
            |> WithComment "Pixel coordinates of the point where the mouse event occured relative to the map сontainer."
            "originalEvent" =? T<MouseEvent>
            |> WithComment "The original DOM mouse event fired by the browser."
        ]

    let LocationEvent =
        Class "L.LocationEvent"
        |=> Inherits Event
        |+> Instance [
            "latlng" =? LatLng
            |> WithComment "Detected geographical location of the user."
            "bounds" =? LatLngBounds
            |> WithComment "Geographical bounds of the area user is located in (with respect to the accuracy of location)."
            "accuracy" =? T<float>
            |> WithComment "Accuracy of location in meters."
            "altitude" =? T<float>
            |> WithComment "Height of the position above the WGS84 ellipsoid in meters."
            "altitudeAccuracy" =? T<float>
            |> WithComment "Accuracy of altitude in meters."
            "heading" =? T<float>
            |> WithComment "The direction of travel in degrees counting clockwise from true North."
            "speed" =? T<float>
            |> WithComment "Current velocity in meters per second."
            "timestamp" =? T<float>
            |> WithComment "The time when the position was acquired."
        ]

    let ErrorEvent =
        Class "L.ErrorEvent"
        |=> Inherits Event
        |+> Instance [
            "message" =? T<string>
            |> WithComment "Error message."
            "code" =? T<int>
            |> WithComment "Error code (if applicable)."
        ]

    let LayerEvent =
        Class "L.LayerEvent"
        |=> Inherits Event
        |+> Instance [
            "layer" =? ILayer
            |> WithComment "The layer that was added or removed."
        ]

    let LayersControlEvent =
        Class "L.LayersControlEvent"
        |=> Inherits Event
        |+> Instance [
            "layer" =? ILayer
            |> WithComment "The layer that was added or removed."
            "name" =? T<string>
            |> WithComment "The name of the layer that was added or removed."
        ]

    let TileEvent =
        Class "L.TileEvent"
        |=> Inherits Event
        |+> Instance [
            "tile" =? T<Element>
            |> WithComment "The tile element (image)."
            "url" =? T<string>
            |> WithComment "The source URL of the tile."
        ]

    let ResizeEvent =
        Class "L.ResizeEvent"
        |=> Inherits Event
        |+> Instance [
            "oldSize" =? Point
            |> WithComment "The old size before resize event."
            "newSize" =? Point
            |> WithComment "The new size after the resize event."
        ]

    let GeoJSONEvent =
        Class "L.GeoJSONEvent"
        |=> Inherits Event
        |+> Instance [
            "layer" =? ILayer
            |> WithComment "The layer for the GeoJSON feature that is being added to the map."
            "properties" =? T<obj>
            |> WithComment "GeoJSON properties of the feature."
            "geometryType" =? T<string>
            |> WithComment "GeoJSON geometry type of the feature."
            "id" =? T<string>
            |> WithComment "GeoJSON ID of the feature (if present)."
        ]

    let PopupEvent =
        Class "L.PopupEvent"
        |=> Inherits Event
        |+> Instance [
            "popup" =? PopupT
            |> WithComment "The popup that was opened or closed."
        ]

    let TooltipEvent =
        Class "L.TooltipEvent"
        |=> Inherits Event
        |+> Instance [
            "tooltip" =? TooltipT
            |> WithComment "The tooltip that was opened or closed."
        ]

    let DragEndEvent =
        Class "L.DragEndEvent"
        |=> Inherits Event
        |+> Instance [
            "distance" =? T<int>
            |> WithComment "The distance in pixels the draggable element was moved by."
        ]

    let WithEvents events cls : CodeModel.Class =
        cls
        |+> (
            Instance [
                "addEventListener" => T<string>?``type`` * (cls -* Event ^-> T<unit>)?fn * !?T<obj>?context ^-> T<unit>
                |> WithComment "Adds a listener function (fn) to a particular event type of the object. You can optionally specify the context of the listener (object the this keyword will point to). You can also pass several space-separated types (e.g. 'click dblclick')."
                "addOneTimeEventListener" => T<string>?``type`` * (cls -* Event ^-> T<unit>)?fn * !?T<obj>?context ^-> T<unit>
                |> WithComment "The same as addEventListener except the listener will only get fired once and then removed."
                "addEventListener" => T<obj>?eventMap * !?T<obj>?context ^-> T<unit>
                |> WithComment "Adds a set of type/listener pairs, e.g. {click: onClick, mousemove: onMouseMove}"
                "removeEventListener" => T<string>?``type`` * !?(cls -* Event ^-> T<unit>)?fn * !?T<obj>?context ^-> T<unit>
                |> WithComment "Removes a previously added listener function. If no function is specified, it will remove all the listeners of that particular event from the object."
                "removeEventListener" => T<obj>?eventMap * !?T<obj>?context ^-> T<unit>
                |> WithComment "Removes a set of type/listener pairs."
                "removeEventListener" => T<unit> ^-> T<unit>
                |> WithComment "Removes all listeners. An alias to clearAllEventListeners when you use it without arguments."
                "hasEventListeners" => T<string>?``type`` ^-> T<bool>
                |> WithComment "Returns true if a particular event type has some listeners attached to it."
                "fireEvent" => T<string>?``type`` * !?Event?data ^-> T<unit>
                |> WithComment "Fires an event of the specified type. You can optionally provide an data object — the first argument of the listener function will contain its properties."
                "clearAllEventListeners" => T<unit> ^-> T<unit>
                |> WithComment "Removes all listeners to all events on the object."
                "on" => T<string>?``type`` * (cls -* Event ^-> T<unit>)?fn * !?T<obj>?context ^-> T<unit>
                |> WithComment "Adds a listener function (fn) to a particular event type of the object. You can optionally specify the context of the listener (object the this keyword will point to). You can also pass several space-separated types (e.g. 'click dblclick')."
                "on" => T<obj>?eventMap * !?T<obj>?context ^-> T<unit>
                |> WithComment "Adds a set of type/listener pairs, e.g. {click: onClick, mousemove: onMouseMove}"
                "once" => T<string>?``type`` * (cls -* Event ^-> T<unit>)?fn * !?T<obj>?context ^-> T<unit>
                |> WithComment "The same as addEventListener except the listener will only get fired once and then removed."
                "off" => T<string>?``type`` * !?(cls -* Event ^-> T<unit>)?fn * !?T<obj>?context ^-> T<unit>
                |> WithComment "Removes a previously added listener function. If no function is specified, it will remove all the listeners of that particular event from the object."
                "off" => T<obj>?eventMap * !?T<obj>?context ^-> T<unit>
                |> WithComment "Removes a set of type/listener pairs."
                "off" => T<unit> ^-> T<unit>
                |> WithComment "Removes all listeners. An alias to clearAllEventListeners when you use it without arguments."
                "fire" => T<string>?``type`` * !?Event?data ^-> T<unit>
                |> WithComment "Fires an event of the specified type. You can optionally provide an data object — the first argument of the listener function will contain its properties."
            ])
        |+> Instance (events |> List.collect (fun (name, data, descr) ->
            [
                ("on_" + name) => (cls * data ^-> T<unit>)?fn ^-> T<unit>
                |> WithInline ("$this.on('" + name + "', function(x){$fn(this,x)})")
                |> WithComment descr
                ("once_" + name) => (cls * data ^-> T<unit>)?fn ^-> T<unit>
                |> WithInline ("$this.once('" + name + "', function(x){$fn(this,x)})")
                |> WithComment descr
                ("off_" + name) => (cls * data ^-> T<unit>)?fn ^-> T<unit>
                |> WithInline ("$this.off('" + name + "', function(x){$fn(this,x)})")
                |> WithComment descr
                ("off_" + name) => T<unit> ^-> T<unit>
                |> WithInline ("$this.off('" + name + "')")
                |> WithComment descr
                ("fire_" + name) => data ^-> T<unit>
                |> WithInline ("$this.fire('" + name + "')")
                |> WithComment descr
            ]))

    let TooltipOptions =
        Class "L.Tooltip.Options"
        |+> Static [Constructor T<unit> |> WithInline "{}"]
        |+> Instance [
            "pane" =@ T<string>
            |> WithComment "Map pane where the tooltip will be added."
            "offset" =@ Point
            |> WithComment "Optional offset of the tooltip position."
            "direction" =@ T<string>
            |> WithComment "Direction where to open the tooltip. Possible values are: right, left, top, bottom, center, auto. auto will dynamically switch between right and left according to the tooltip position on the map."
            "permanent" =@ T<bool>
            |> WithComment "Whether to open the tooltip permanently or only on mouseover."
            "sticky" =@ T<bool>
            |> WithComment "If true, the tooltip will follow the mouse instead of being fixed at the feature center."
            "interactive" =@ T<bool>
            |> WithComment "If true, the tooltip will listen to the feature events."
            "opacity" =@ T<float>
            |> WithComment "Tooltip container opacity."
        ]

    let Tooltip =
        Class "L.Tooltip"
        |=> Nested [TooltipOptions]
        |=> Implements [ILayer]
        |=> TooltipT
        |+> Static [
            Constructor (!?TooltipOptions * !?ILayer?source)
            |> WithComment "Instantiates a Popup object given an optional options object that describes its appearance and location and an optional source object that is used to tag the popup with a reference to the ILayer to which it refers."
        ]
        |> WithEvents [
            "add", Event, "Fired after the layer is added to a map"
            "remove", Event, "Fired after the layer is removed from a map"
            "popupopen", PopupEvent, "Fired when a popup bound to this layer is opened"
            "popupclose", PopupEvent, "Fired when a popup bound to this layer is closed"
            "toolipopen", TooltipEvent, "Fired when a tooltip bound to this layer is opened."
            "toolipclose", TooltipEvent, "Fired when a tooltip bound to this layer is closed."
        ]
        |+> Instance [
            "addTo" => MapT ^-> T<unit>
            |> WithComment "Adds the popup to the map."
            "openOn" => MapT ^-> T<unit>
            |> WithComment "Adds the popup to the map and closes the previous one. The same as map.openPopup(popup)."
            "setLatLng" => LatLngOrCoords ^-> T<unit>
            |> WithComment "Sets the geographical point where the popup will open."
            "getLatLng" => T<unit> ^-> LatLng
            |> WithComment "Returns the geographical point of popup."
            "setContent" => (T<string> + T<Element>) ^-> T<unit>
            |> WithComment "Sets the HTML content of the popup."
            "getContent" => T<unit> ^-> T<Element>
            |> WithComment "Returns the content of the popup."
            "update" => T<unit -> unit>
            |> WithComment "Updates the popup content, layout and position. Useful for updating the popup after something inside changed, e.g. image loaded."
            "getElement" => T<unit> ^-> T<string> + T<Element>
            |> WithComment "Returns the HTML container of the popup."
            "isOpen" => T<unit> ^-> T<bool>
            |> WithComment "Returns true when the popup is visible on the map."
            "bringToFront" => T<unit> ^-> T<unit>
            |> WithComment "Brings this popup in front of other popups (in the same map pane)."
            "bringToBack" => T<unit> ^-> T<unit>
            |> WithComment "Brings this popup to the back of other popups (in the same map pane)."
        ]

    let PopupOptions =
        Class "L.Popup.Options"
        |+> Static [Constructor T<unit> |> WithInline "{}"]
        |+> Instance [
            "maxWidth" =@ T<int>
            |> WithComment "Max width of the popup."
            "minWidth" =@ T<int>
            |> WithComment "Min width of the popup."
            "maxHeight" =@ T<int>
            |> WithComment "If set, creates a scrollable container of the given height inside a popup if its content exceeds it."
            "autoPan" =@ T<bool>
            |> WithComment "Set it to false if you don't want the map to do panning animation to fit the opened popup."
            "keepInView" =@ T<bool>
            |> WithComment "Set it to true if you want to prevent users from panning the popup off of the screen while it is open."
            "closeButton" =@ T<bool>
            |> WithComment "Controls the presense of a close button in the popup."
            "offset" =@ Point
            |> WithComment "The offset of the popup position. Useful to control the anchor of the popup when opening it on some overlays."
            "autoPanPaddingTopLeft" =@ Point
            |> WithComment "The margin between the popup and the top left corner of the map view after autopanning was performed."
            "autoPanPaddingBottomRight" =@ Point
            |> WithComment "The margin between the popup and the bottom right corner of the map view after autopanning was performed."
            "autoPanPadding" =@ Point
            |> WithComment "Equivalent of setting both top left and bottom right autopan padding to the same value."
            "zoomAnimation" =@ T<bool>
            |> WithComment "Whether to animate the popup on zoom. Disable it if you have problems with Flash content inside popups."
            "closeOnClick" =@ T<bool>
            |> WithComment "Set it to false if you want to override the default behavior of the popup closing when user clicks the map (set globally by the Map closePopupOnClick option)."
            "className" =@ T<string>
            |> WithComment "A custom class name to assign to the popup."
            "autoClose" =@ T<bool>
            |> WithComment "Set it to false if you want to override the default behavior of the popup closing when another popup is opened."
            "closeOnEscapeKey" =@ T<bool>
            |> WithComment "Set it to false if you want to override the default behavior of the ESC key for closing of the popup."
            "className" =@ T<string>
            |> WithComment "A custom CSS class name to assign to the popup."
            "pane" =@ T<string>
            |> WithComment "Map pane where the popup will be added."
            "attribution" =@ T<string>
            |> WithComment "String to be shown in the attribution control, e.g. © OpenStreetMap contributors. It describes the layer data and is often a legal obligation towards copyright holders and tile providers."
        ]

    let Popup =
        Class "L.Popup"
        |=> Nested [PopupOptions]
        |=> Implements [ILayer]
        |=> PopupT
        |+> Static [
            Constructor (!?PopupOptions * !?ILayer?source)
            |> WithComment "Instantiates a Popup object given an optional options object that describes its appearance and location and an optional source object that is used to tag the popup with a reference to the ILayer to which it refers."
        ]
        |> WithEvents [
            "add", Event, "Fired after the layer is added to a map"
            "remove", Event, "Fired after the layer is removed from a map"
            "popupopen", PopupEvent, "Fired when a popup bound to this layer is opened"
            "popupclose", PopupEvent, "Fired when a popup bound to this layer is closed"
            "toolipopen", TooltipEvent, "Fired when a tooltip bound to this layer is opened."
            "toolipclose", TooltipEvent, "Fired when a tooltip bound to this layer is closed."
        ]
        |+> Instance [
            "addTo" => MapT ^-> T<unit>
            |> WithComment "Adds the popup to the map."
            "openOn" => MapT ^-> T<unit>
            |> WithComment "Adds the popup to the map and closes the previous one. The same as map.openPopup(popup)."
            "setLatLng" => LatLngOrCoords ^-> T<unit>
            |> WithComment "Sets the geographical point where the popup will open."
            "getLatLng" => T<unit> ^-> LatLng
            |> WithComment "Returns the geographical point of popup."
            "setContent" => (T<string> + T<Element>) ^-> T<unit>
            |> WithComment "Sets the HTML content of the popup."
            "getContent" => T<unit> ^-> T<Element>
            |> WithComment "Returns the content of the popup."
            "update" => T<unit -> unit>
            |> WithComment "Updates the popup content, layout and position. Useful for updating the popup after something inside changed, e.g. image loaded."
            "getElement" => T<unit> ^-> T<string> + T<Element>
            |> WithComment "Returns the HTML container of the popup."
            "isOpen" => T<unit> ^-> T<bool>
            |> WithComment "Returns true when the popup is visible on the map."
            "bringToFront" => T<unit> ^-> T<unit>
            |> WithComment "Brings this popup in front of other popups (in the same map pane)."
            "bringToBack" => T<unit> ^-> T<unit>
            |> WithComment "Brings this popup to the back of other popups (in the same map pane)."
        ]

    let TileLayerWMSOptions =
        Class "L.TileLayer.WMS.Options"
        |+> Static [Constructor T<string>?Layers |> WithInline "{layers:$Layers}"]
        |+> Instance [
            "layers" =@ T<string>
            |> WithComment "(required) Comma-separated list of WMS layers to show."
            "styles" =@ T<string>
            |> WithComment "Comma-separated list of WMS styles."
            "format" =@ T<string>
            |> WithComment "WMS image format (use 'image/png' for layers with transparency)."
            "transparent" =@ T<bool>
            |> WithComment "If true, the WMS service will return images with transparency."
            "version" =@ T<string>
            |> WithComment "Version of the WMS service to use."
            "crs" =@ CRS
            |> WithComment "Coordinate Reference System to use for the WMS requests, defaults to map CRS. Don't change this if you're not sure what it means."
        ]

    let TileLayerWMS =
        Class "L.TileLayer.WMS"
        |=> Nested [TileLayerWMSOptions]
        |=> Inherits TileLayerT
        |+> Static [
            Constructor (T<string>?baseUrl * !?TileLayerWMSOptions)
            |> WithComment "Instantiates a WMS tile layer object given a base URL of the WMS service and a WMS parameters/options object."
        ]
        |+> Instance [
            "setParams" => TileLayerWMSOptions * !?T<bool>?noRedraw ^-> T<unit>
            |> WithComment "Merges an object with the new parameters and re-requests tiles on the current screen (unless noRedraw was set to true)."
        ]

    let TileLayerCanvasOptions =
        Class "L.TileLayer.Canvas.Options"
        |+> Static [Constructor T<unit> |> WithInline "{}"]
        |+> Instance [
            "async" =@ T<bool>
            |> WithComment "Indicates that tiles will be drawn asynchronously. tileDrawn method should be called for each tile after drawing completion."
        ]

    let TileLayerCanvas =
        Class "L.TileLayer.Canvas"
        |=> Nested [TileLayerCanvasOptions]
        |=> Inherits TileLayerT
        |+> Static [
            Constructor (!?TileLayerCanvasOptions)
            |> WithComment "Instantiates a Canvas tile layer object given an options object (optionally)."
        ]
        |+> Instance [
            "drawTile" => T<JavaScript.CanvasElement> * PointOrCoords * T<int>?zoom ^-> T<unit>
            |> WithComment "You need to define this method after creating the instance to draw tiles; canvas is the actual canvas tile on which you can draw, tilePoint represents the tile numbers, and zoom is the current zoom."
            "tileDrawn" => T<JavaScript.CanvasElement> ^-> T<unit>
            |> WithComment "If async option is defined, this function should be called for each tile after drawing completion. canvas is the same canvas element, that was passed to drawTile."
        ]

    let TileLayerOptions =
        Class "L.TileLayer.Options"
        |+> Static [Constructor T<unit> |> WithInline "{}"]
        |+> Instance [
            "minZoom" =@ T<int>
            |> WithComment "Minimum zoom number."
            "maxZoom" =@ T<int>
            |> WithComment "Maximum zoom number."
            "maxNativeZoom" =@ T<int>
            |> WithComment "Maximum zoom number the tiles source has available. If it is specified, the tiles on all zoom levels higher than maxNativeZoom will be loaded from maxZoom level and auto-scaled."
            "tileSize" =@ T<int>
            |> WithComment "Tile size (width and height in pixels, assuming tiles are square)."
            "subdomains" =@ T<string[]>
            |> WithComment "Subdomains of the tile service. Can be passed in the form of one string (where each letter is a subdomain name) or an array of strings."
            "errorTileUrl" =@ T<string>
            |> WithComment "URL to the tile image to show in place of the tile that failed to load."
            "attribution" =@ T<string>
            |> WithComment "e.g. \"© Mapbox\" — the string used by the attribution control, describes the layer data."
            "tms" =@ T<bool>
            |> WithComment "If true, inverses Y axis numbering for tiles (turn this on for TMS services)."
            "continuousWorld" =@ T<bool>
            |> WithComment "If set to true, the tile coordinates won't be wrapped by world width (-180 to 180 longitude) or clamped to lie within world height (-90 to 90). Use this if you use Leaflet for maps that don't reflect the real world (e.g. game, indoor or photo maps)."
            "noWrap" =@ T<bool>
            |> WithComment "If set to true, the tiles just won't load outside the world width (-180 to 180 longitude) instead of repeating."
            "zoomOffset" =@ T<int>
            |> WithComment "The zoom number used in tile URLs will be offset with this value."
            "zoomReverse" =@ T<bool>
            |> WithComment "If set to true, the zoom number used in tile URLs will be reversed (maxZoom - zoom instead of zoom)"
            "opacity" =@ T<float>
            |> WithComment "The opacity of the tile layer."
            "zIndex" =@ T<int>
            |> WithComment "The explicit zIndex of the tile layer. Not set by default."
            "unloadInvisibleTiles" =@ T<bool>
            |> WithComment "If true, all the tiles that are not visible after panning are removed (for better performance). true by default on mobile WebKit, otherwise false."
            "updateWhenIdle" =@ T<bool>
            |> WithComment "If false, new tiles are loaded during panning, otherwise only after it (for better performance). true by default on mobile WebKit, otherwise false."
            "detectRetina" =@ T<bool>
            |> WithComment "If true and user is on a retina display, it will request four tiles of half the specified size and a bigger zoom level in place of one to utilize the high resolution."
            "reuseTiles" =@ T<bool>
            |> WithComment "If true, all the tiles that are not visible after panning are placed in a reuse queue from which they will be fetched when new tiles become visible (as opposed to dynamically creating new ones). This will in theory keep memory usage low and eliminate the need for reserving new memory whenever a new tile is needed."
            "bounds" =@ LatLngBounds
            |> WithComment "When this option is set, the TileLayer only loads tiles that are in the given geographical bounds."
            "crossOrigin" =@ T<bool> + T<string>
            |> WithComment "Whether the crossOrigin attribute will be added to the tiles. If a String is provided, all tiles will have their crossOrigin attribute set to the String provided. This is needed if you want to access tile pixel data. Refer to CORS Settings for valid String values."

        ]

    let TileLayerOSM =
        Class "L.TileLayer.OpenStreetMap"
        |+> Static [
            "urlTemplate" =? T<string>
            |> WithGetterInline "'https://{s}.tile.osm.org/{z}/{x}/{y}.png'"
            "attribution" =? T<string>
            |> WithGetterInline """'&copy; <a href="https://osm.org/copyright">OpenStreetMap</a> contributors, <a href="https://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>'"""
        ]

    let TileLayerMapbox =
        Class "L.TileLayer.Mapbox"
        |+> Static [
            "urlTemplate" => T<string>?mapId ^-> T<string>
            |> WithInline "'https://{s}.tiles.mapbox.com/v3/'+$mapId+'/{z}/{x}/{y}.png'"
            "attribution" =? T<string>
            |> WithGetterInline """'Map data &copy; <a href="https://osm.org/copyright">OpenStreetMap</a> contributors, <a href="https://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>, Imagery &copy; <a href="https://mapbox.com/">Mapbox</a>'"""
        ]

    let TileLayer =
        Class "L.TileLayer"
        |=> TileLayerT
        |=> Implements [ILayer]
        |=> Nested [TileLayerOptions; TileLayerWMS; TileLayerCanvas; TileLayerOSM; TileLayerMapbox]
        |+> Static [
            Constructor (T<string>?urlTemplate * !?TileLayerOptions)
            |> WithComment "Instantiates a tile layer object given a URL template and optionally an options object."
        ]
        |> WithEvents [
            "loading", Event, "Fired when the tile layer starts loading tiles."
            "load", Event, "Fired when the tile layer loaded all visible tiles."
            "tileloadstart", TileEvent, "Fired when a tile is requested and starts loading."
            "tileload", TileEvent, "Fired when a tile loads."
            "tileunload", TileEvent, "Fired when a tile is removed (e.g. when you have unloadInvisibleTiles on)."
            "tileerror", ErrorEvent, "Fired when there is an error loading a tile."
            "add", Event, "Fired when the path is added to the map."
            "remove", Event, "Fired when the path is removed from the map."
            "popupopen", PopupEvent, "Fired when a popup bound to this layer is opened"
            "popupclose", PopupEvent, "Fired when a popup bound to this layer is closed"
            "tooltipopen", TooltipEvent, "Fired when a tooltip bound to this layer is opened."
            "tooltipclose", TooltipEvent, "Fired when a tooltip bound to this layer is closed."
        ]
        |+> Instance [
            "getTileUrl" => T<obj> ^-> T<string>
            |> WithComment "Called only internally, returns the URL for a tile given its coordinates. Classes extending TileLayer can override this function to provide custom tile URL naming schemes."
            "bringToFront" => T<unit -> unit>
            |> WithComment "Brings the tile layer to the top of all tile layers."
            "bringToBack" => T<unit -> unit>
            |> WithComment "Brings the tile layer to the bottom of all tile layers."
            "setOpacity" => T<float> ^-> T<unit>
            |> WithComment "Changes the opacity of the tile layer."
            "setZIndex" => T<int> ^-> T<unit>
            |> WithComment "Sets the zIndex of the tile layer."
            "redraw" => T<unit -> unit>
            |> WithComment "Causes the layer to clear all the tiles and request them again."
            "setUrl" => T<string -> unit>
            |> WithComment "Updates the layer's URL template and redraws it."
            "getContainer" => T<unit -> Element>
            |> WithComment "Returns the HTML element that contains the tiles for this layer."
            "isLoading" => T<unit> ^-> T<bool>
            |> WithComment "Returns true if any tile in the grid layer has not finished loading."
            "redraw" => T<unit> ^-> T<unit>
            |> WithComment "Causes the layer to clear all the tiles and request them again."
            "getTileSize" => T<unit> ^-> Point 
            "bindPopup" => (T<string> + T<Element> + Popup) * !?PopupOptions ^-> T<unit>
            |> WithComment "Binds a popup with a particular HTML content to a click on this marker. You can also open the bound popup with the Marker openPopup method."
            "unbindPopup" => T<unit -> unit>
            |> WithComment "Unbinds the popup previously bound to the marker with bindPopup."
            "openPopup" => T<unit -> unit>
            |> WithComment "Opens the popup previously bound by the bindPopup method."
            "getPopup" => T<unit> ^-> Popup
            |> WithComment "Returns the popup previously bound by the bindPopup method."
            "closePopup" => T<unit -> unit>
            |> WithComment "Closes the bound popup of the marker if it's opened."
            "togglePopup" => T<unit -> unit>
            |> WithComment "Toggles the popup previously bound by the bindPopup method."
            "setPopupContent" => (T<string> + T<Element>) * !?PopupOptions ^-> T<unit>
            |> WithComment "Sets an HTML content of the popup of this marker."
            "isPopupOpen"=> T<unit> ^-> T<bool>
            |> WithComment "Returns true if the popup bound to this layer is currently open."
            "toGeoJSON" => T<unit -> obj>
            |> WithComment "Returns a GeoJSON representation of the marker (GeoJSON Point Feature)."
            "getIcon" => T<unit> ^-> Icon
            |> WithComment "Returns the current icon used by the marker"
            "toGeoJSON" => T<int> ^-> T<unit>
            |> WithComment "precision is the number of decimal places for coordinates. The default value is 6 places. Returns a GeoJSON representation of the marker (as a GeoJSON Point Feature)."
            "remove" => T<unit> ^-> T<unit>
            |> WithComment "Removes the layer from the map it is currently active on."
            "getPane" => T<string> ^-> T<Element>
            |> WithComment "Returns the HTMLElement representing the named pane on the map. If name is omitted, returns the pane for this layer."
            "getAttribution" => T<unit> ^-> T<string>
            |> WithComment "Used by the attribution control, returns the attribution option."
            "bindTooltip" => (T<string> + T<Element> + Popup) * !?TooltipOptions ^-> T<unit>
            |> WithComment "Binds a tooltip to the layer with the passed content and sets up the necessary event listeners. If a Function is passed it will receive the layer as the first argument and should return a String or HTMLElement."
            "unbindTooltip" => T<unit> ^-> T<unit>
            |> WithComment "Removes the tooltip previously bound with bindTooltip."
            "openTooltip" => !? LatLng ^-> T<unit>
            |> WithComment "Opens the bound tooltip at the specified latlng or at the default tooltip anchor if no latlng is passed."
            "closeTooltip" => T<unit> ^-> T<unit>
            |> WithComment "Closes the tooltip bound to this layer if it is open."
            "toggleTooltip" => T<unit> ^-> T<unit>
            |> WithComment "Opens or closes the tooltip bound to this layer depending on its current state."
            "isTooltipOpen" => T<unit> ^-> T<bool>
            |> WithComment "Returns true if the tooltip bound to this layer is currently open."
            "setTooltipContent" => T<string> + T<Element> + Tooltip ^-> T<unit>
            |> WithComment "Sets the content of the tooltip bound to this layer."
            "getTooltip" => T<unit> ^-> Tooltip
            |> WithComment "Returns the tooltip bound to this layer."
            "on" => T<string> * (T<'a> ^-> T<'a>) * !? T<obj> ^-> T<unit>
            |> WithComment "Adds a listener function (fn) to a particular event type of the object. You can optionally specify the context of the listener (object the this keyword will point to). You can also pass several space-separated types (e.g. 'click dblclick')."
            "on" => T<obj> ^-> T<unit>
            |> WithComment "Adds a set of type/listener pairs, e.g. {click: onClick, mousemove: onMouseMove}"
            "off" => T<string> * (T<'a> ^-> T<'a>) * !? T<obj> ^-> T<unit>
            |> WithComment "Removes a previously added listener function. If no function is specified, it will remove all the listeners of that particular event from the object. Note that if you passed a custom context to on, you must pass the same context to off in order to remove the listener."
            "off" => T<obj> ^-> T<unit>
            |> WithComment "Removes a set of type/listener pairs."
            "off" => T<unit> ^-> T<unit>
            |> WithComment "Removes all listeners to all events on the object. This includes implicitly attached events."
            "fire" => T<string> * T<obj> * !? T<bool> ^-> T<unit>
            |> WithComment "Fires an event of the specified type. You can optionally provide an data object — the first argument of the listener function will contain its properties. The event can optionally be propagated to event parents."
            "listens" => T<string> ^-> T<bool>
            |> WithComment "Returns true if a particular event type has any listeners attached to it."
            "once" => T<string> * (T<'a> ^-> T<'a>) * !? T<obj> ^-> T<unit>
            |> WithComment "Behaves as on(…), except the listener will only get fired once and then removed."
            "once" => T<obj> ^-> T<unit>
            |> WithComment "Behaves as on(…), except the listener will only get fired once and then removed."
            "addEventParent" => Evented ^-> T<unit>
            |> WithComment "Adds an event parent - an Evented that will receive propagated events"
            "removeEventParent" => Evented ^-> T<unit>
            |> WithComment "Removes an event parent, so it will stop receiving propagated events"
        ]

    let ImageOverlayOptions =
        Class "L.ImageOverlay.Options"
        |+> Static [Constructor T<unit> |> WithInline "{}"]
        |+> Instance [
            "opacity" =@ T<float>
            |> WithComment "The opacity of the image overlay."
            "attribution" =@ T<string>
            |> WithComment "The attribution text of the image overlay."
        ]

    let ImageOverlay =
        Class "L.ImageOverlay"
        |+> Static [
            Constructor (T<string>?url * LatLngBoundsOrCoords?bounds * !?ImageOverlayOptions)
            |> WithComment "Instantiates an image overlay object given the URL of the image and the geographical bounds it is tied to."
        ]
        |=> Nested [ImageOverlayOptions]
        |=> Implements [ILayer]
        |+> Instance [
            "addTo" => MapT ^-> T<unit>
            |> WithComment "Adds the overlay to the map."
            "setOpacity" => T<float -> unit>
            |> WithComment "Sets the opacity of the overlay."
            "setUrl" => T<string -> unit>
            |> WithComment "Changes the URL of the image."
            "bringToFront" => T<unit -> unit>
            |> WithComment "Brings the layer to the top of all overlays."
            "bringToBack" => T<unit -> unit>
            |> WithComment "Brings the layer to the bottom of all overlays."
        ]

    let PathOptions =
        Class "L.Path.Options"
        |+> Static [Constructor T<unit> |> WithInline "{}"]
        |+> Instance [
            "stroke" =@ T<bool>
            |> WithComment "Whether to draw stroke along the path. Set it to false to disable borders on polygons or circles."
            "color" =@ T<string>
            |> WithComment "Stroke color."
            "weight" =@ T<int>
            |> WithComment "Stroke width in pixels."
            "opacity" =@ T<float>
            |> WithComment "Stroke opacity."
            "lineCap" =@ T<string>
            |> WithComment "A string that defines shape to be used at the end of the stroke."
            "lineJoin" =@ T<string>
            |> WithComment "A string that defines shape to be used at the corners of the stroke."
            "fill" =@ T<bool>
            |> WithComment "Whether to fill the path with color. Set it to false to disable filling on polygons or circles."
            "fillColor" =@ T<string>
            |> WithComment "Fill color."
            "fillOpacity" =@ T<float>
            |> WithComment "Fill opacity."
            "dashArray" =@ T<string>
            |> WithComment "A string that defines the stroke dash pattern. Doesn't work on canvas-powered layers (e.g. Android 2)."
            "lineCap" =@ T<JavaScript.LineCap>
            |> WithComment "A string that defines shape to be used at the end of the stroke."
            "lineJoin" =@ T<JavaScript.LineJoin>
            |> WithComment "A string that defines shape to be used at the corners of the stroke."
            "clickable" =@ T<bool>
            |> WithComment "If false, the vector will not emit mouse events and will act as a part of the underlying map."
            "pointerEvents" =@ T<string>
            |> WithComment "Sets the pointer-events attribute on the path if SVG backend is used."
            "className" =@ T<string>
            |> WithComment "Custom class name set on an element."
            "dashOffset" =@ T<string>
            |> WithComment "A string that defines the distance into the dash pattern to start the dash. Doesn't work on Canvas-powered layers in some old browsers."
            "fillRule" =@ T<string>
            |> WithComment "A string that defines how the inside of a shape is determined."
            "bubblingMOuseEvents" =@ T<bool>
            |>WithComment "When true, a mouse event on this path will trigger the same event on the map (unless L.DomEvent.stopPropagation is used)."
            "renderer" =@ Renderer
            |> WithComment "Use this specific instance of Renderer for this path. Takes precedence over the map's default renderer."
            "className" =@ T<string>
            |> WithComment "Custom class name set on an element. Only for SVG renderer."
        ]

    let Path =
        Class "L.Path"
        |=> Nested [PathOptions]
        |=> Implements [ILayer]
        |+> Instance [
            "addTo" => MapT ^-> T<unit>
            |> WithComment "Adds the layer to the map."
            "bindPopup" => (T<string> + T<Element> + Popup) * !?PopupOptions ^-> T<unit>
            |> WithComment "Binds a popup with a particular HTML content to a click on this path."
            "unbindPopup" => T<unit -> unit>
            |> WithComment "Binds a given popup object to the path."
            "openPopup" => !?LatLngOrCoords ^-> T<unit>
            |> WithComment "Opens the popup previously bound by the bindPopup method in the given point, or in one of the path's points if not specified."
            "closePopup" => T<unit -> unit>
            |> WithComment "Closes the path's bound popup if it is opened."
            "setStyle" => PathOptions ^-> T<unit>
            |> WithComment "Changes the appearance of a Path based on the options in the Path options object."
            "getBounds" => T<unit> ^-> LatLngBounds
            |> WithComment "Returns the LatLngBounds of the path."
            "bringToFront" => T<unit -> unit>
            |> WithComment "Brings the layer to the top of all path layers."
            "bringToBack" => T<unit -> unit>
            |> WithComment "Brings the layer to the bottom of all path layers."
            "redraw" => T<unit -> unit>
            |> WithComment "Redraws the layer. Sometimes useful after you changed the coordinates that the path uses."
        ]
        |> WithEvents [
            "click", MouseEvent, "Fired when the user clicks (or taps) the object."
            "dblclick", MouseEvent, "Fired when the user double-clicks (or double-taps) the object."
            "mousedown", MouseEvent, "Fired when the user pushes the mouse button on the object."
            "mouseover", MouseEvent, "Fired when the mouse enters the object."
            "mouseout", MouseEvent, "Fired when the mouse leaves the object."
            "contextmenu", MouseEvent, "Fired when the user pushes the right mouse button on the object, prevents default browser context menu from showing if there are listeners on this event."
            "add", Event, "Fired when the path is added to the map."
            "remove", Event, "Fired when the path is removed from the map."
            "popupopen", PopupEvent, "Fired when a popup bound to the path is open."
            "popupclose", PopupEvent, "Fired when a popup bound to the path is closed."
            "tooltipopen", TooltipEvent, "Fired when a tooltip bound to this layer is opened."
            "tooltipclose", TooltipEvent, "Fired when a tooltip bound to this layer is closed."
        ]
        |+> Static [
            "SVG" =? T<bool>
            |> WithComment "True if SVG is used for vector rendering (true for most modern browsers)."
            "VMS" =? T<bool>
            |> WithComment "True if VML is used for vector rendering (IE 6-8)."
            "CANVAS" =? T<bool>
            |> WithComment "True if Canvas is used for vector rendering (Android 2). You can also force this by setting global variable L_PREFER_CANVAS to true before the Leaflet include on your page — sometimes it can increase performance dramatically when rendering thousands of circle markers, but currently suffers from a bug that causes removing such layers to be extremely slow."
            "CLIP_PADDING" =? T<float>
            |> WithComment "How much to extend the clip area around the map view (relative to its size, e.g. 0.5 is half the screen in each direction). Smaller values mean that you will see clipped ends of paths while you're dragging the map, and bigger values decrease drawing performance."
        ]

    let PolylineOptions =
        Class "L.Polyline.Options"
        |=> Inherits PathOptions
        |+> Static [Constructor T<unit> |> WithInline "{}"]
        |+> Instance [
            "smoothFactor" =@ T<float>
            |> WithComment "How much to simplify the polyline on each zoom level. More means better performance and smoother look, and less means more accurate representation."
            "noClip" =@ T<bool>
            |> WithComment "Disabled polyline clipping."
        ]

    let Polyline =
        Class "L.Polyline"
        |=> Nested [PolylineOptions]
        |=> Inherits Path
        |+> Static [
            Constructor (Type.ArrayOf LatLngOrCoords * !?PolylineOptions)
            |> WithComment "Instantiates a polyline object given an array of geographical points and optionally an options object."
        ]
        |+> Instance [
            "addLatLng" => LatLngOrCoords ^-> T<unit>
            |> WithComment "Adds a given point to the polyline."
            "setLatLngs" => Type.ArrayOf LatLngOrCoords ^-> T<unit>
            |> WithComment "Replaces all the points in the polyline with the given array of geographical points."
            "getLatLngs" => T<unit> ^-> Type.ArrayOf LatLng
            |> WithComment "Returns an array of the points in the path."
            "spliceLatLngs" => T<int>?index * T<int>?pointsToRemove *+ Type.ArrayOf LatLngOrCoords ^-> Type.ArrayOf LatLng
            |> WithComment "Allows adding, removing or replacing points in the polyline. Syntax is the same as in Array#splice. Returns the array of removed points (if any)."
            "getBounds" => T<unit> ^-> LatLngBounds
            |> WithComment "Returns the LatLngBounds of the polyline."
            "toGeoJSON" => T<unit -> obj>
            |> WithComment "Returns a GeoJSON representation of the polyline (GeoJSON LineString Feature)."
        ]

    let MultiPolyline =
        Class "L.MultiPolyline"
        |=> Inherits Polyline
        |+> Static [
            Constructor (Type.ArrayOf (Type.ArrayOf LatLngOrCoords) * !?PolylineOptions)
            |> WithComment "Instantiates a multi-polyline object given an array of arrays of geographical points (one for each individual polyline) and optionally an options object."
        ]
        |+> Instance [
            "setLatLngs" => Type.ArrayOf (Type.ArrayOf LatLngOrCoords) ^-> T<unit>
            |> WithComment "Replace all lines and their paths with the given array of arrays of geographical points."
            "getLatLngs" => T<unit> ^-> Type.ArrayOf (Type.ArrayOf LatLngOrCoords)
            |> WithComment "Returns an array of arrays of geographical points in each line."
            "openPopup" => T<unit -> unit>
            |> WithComment "Opens the popup previously bound by bindPopup."
            "toGeoJSON" => T<unit -> obj>
            |> WithComment "Returns a GeoJSON representation of the polyline (GeoJSON MultiLineString Feature)."
            "isEmpty" => T<unit> ^-> T<bool>
            |> WithComment "Returns true if the Polyline has no LatLngs."
            "closestLayerPoint" => Point ^-> Point
            |> WithComment "Returns the point closest to p on the Polyline."
            "getCenter" => T<unit> ^-> LatLng
            |> WithComment "Returns the center (centroid) of the polyline."
            "getBounds" => T<unit> ^-> LatLngBounds
            |> WithComment "Returns the LatLngBounds of the path."
            "addLatLng" => LatLng * !? !| LatLng ^-> T<unit>
            |> WithComment "Adds a given point to the polyline. By default, adds to the first ring of the polyline in case of a multi-polyline, but can be overridden by passing a specific ring as a LatLng array (that you can earlier access with getLatLngs)."
        ]

    let Polygon =
        Class "L.Polygon"
        |=> Inherits Polyline
        |+> Static [
            Constructor (Type.ArrayOf LatLngOrCoords * !?PolylineOptions)
            |> WithComment "Instantiates a polygon object given an array of geographical points and optionally an options object (the same as for Polyline). You can also create a polygon with holes by passing an array of arrays of latlngs, with the first latlngs array representing the exterior ring while the remaining represent the holes inside."
        ]
        |+> Instance [
            "toGeoJSON" => T<unit -> obj>
            |> WithComment "Returns a GeoJSON representation of the polyline (GeoJSON Polygon Feature)."
        ]

    let MultiPolygon =
        Class "L.MultiPolygon"
        |=> Inherits Polyline
        |+> Static [
            Constructor (Type.ArrayOf (Type.ArrayOf LatLngOrCoords) * !?PolylineOptions)
            |> WithComment "Instantiates a multi-polygon object given an array of latlngs arrays (one for each individual polygon) and optionally an options object (the same as for MultiPolyline)."
        ]
        |+> Instance [
            "setLatLngs" => Type.ArrayOf (Type.ArrayOf LatLngOrCoords) ^-> T<unit>
            |> WithComment "Replace all polygons and their paths with the given array of arrays of geographical points."
            "getLatLngs" => T<unit> ^-> Type.ArrayOf (Type.ArrayOf LatLng)
            |> WithComment "Returns an array of arrays of geographical points in each polygon."
            "openPopup" => T<unit -> unit>
            |> WithComment "Opens the popup previously bound by bindPopup."
            "toGeoJSON" => T<unit -> obj>
            |> WithComment "Returns a GeoJSON representation of the polyline (GeoJSON MultiPolygon Feature)."
        ]

    let Rectangle =
        Class "L.Rectangle"
        |=> Inherits Polygon
        |+> Static [
            Constructor (LatLngBounds * !?PolylineOptions)
            |> WithComment "Instantiates a rectangle object with the given geographical bounds and optionally an options object."
        ]
        |+> Instance [
            "setBounds" => LatLngBoundsOrCoords ^-> T<unit>
            |> WithComment "Redraws the rectangle with the passed bounds."
        ]

    let CircleOptions =
        Class "L.Circle.Options"
        |=> Inherits PathOptions
        |+> Static [Constructor T<unit> |> WithInline "{}"]
        |+> Instance [
            "radius" =@ T<int>
            |> WithComment "Radius of the circle, in meters."
        ]    

    let Circle =
        Class "L.Circle"
        |=> Inherits Path
        |+> Static [
            Constructor (LatLngOrCoords?latlng * !? T<float>?radius * !? CircleOptions)
            |> WithComment "Instantiates a circle object given a geographical point, a radius in meters and optionally an options object."
        ]
        |+> Instance [
            "getLatLng" => T<unit> ^-> LatLng
            |> WithComment "Returns the current geographical position of the circle."
            "getRadius" => T<unit -> float>
            |> WithComment "Returns the current radius of a circle. Units are in meters."
            "setLatLng" => LatLngOrCoords ^-> T<unit>
            |> WithComment "Sets the position of a circle to a new location."
            "setRadius" => T<float -> unit>
            |> WithComment "Sets the radius of a circle. Units are in meters."
            "toGeoJSON" => T<unit -> obj>
            |> WithComment "Returns a GeoJSON representation of the circle (GeoJSON Point Feature)."
            "getRadius" => T<unit> ^-> T<int>
            |> WithComment "Returns the current radius of a circle. Units are in meters."
            "getBounds" => T<unit> ^-> LatLngBounds
            |> WithComment "Returns the LatLngBounds of the path."
        ]

    let CircleMarkerOptions =
        Class "L.CircleMarker.Options"
        |=> Inherits PathOptions
        |+> Static [Constructor T<unit> |> WithInline "{}"]
        |+> Instance [
            "radius" =@ T<int>
            |> WithComment "Radius of the circle, in meters."
        ] 

    let CircleMarker =
        Class "L.CircleMarker"
        |=> Inherits Circle
        |+> Static [
            Constructor (LatLngOrCoords * !? CircleMarkerOptions)
            |> WithComment "Instantiates a circle marker given a geographical point and optionally an options object. The default radius is 10 and can be altered by passing a \"radius\" member in the path options object."
        ]
        |> WithEvents [
            "move", Event, "Fired when the marker is moved via setLatLng. Old and new coordinates are included in event arguments as oldLatLng, latlng."
        ]
        |+> Instance [
            "setLatLng" => LatLngOrCoords ^-> T<unit>
            |> WithComment "Sets the position of a circle marker to a new location."
            "getLatLng" => T<unit> ^-> LatLng
            |> WithComment "Returns the current geographical position of the circle marker"
            "setRadius" => T<int> ^-> T<unit>
            |> WithComment "Sets the radius of a circle marker. Units are in pixels."
            "getRadius" => T<unit> ^-> T<int>
            |> WithComment "Returns the current radius of the circle"
            "toGeoJSON" => T<unit -> obj>
            |> WithComment "Returns a GeoJSON representation of the circle marker (GeoJSON Point Feature)."
        ]

    let LayerGroup =
        Class "L.LayerGroup"
        |=> Implements [ILayer]
        |+> Static [
            Constructor (!? !| ILayer * !? T<obj>)
            |> WithComment "Create a layer group, optionally given an initial set of layers."
        ]
        |+> Instance [
            "addTo" => MapT ^-> T<unit>
            |> WithComment "Adds the group of layers to the map."
            "addLayer" => ILayer ^-> T<unit>
            |> WithComment "Adds a given layer to the group."
            "removeLayer" => ILayer ^-> T<unit>
            |> WithComment "Removes a given layer from the group."
            "removeLayer" => T<int> ^-> T<unit>
            |> WithComment "Removes the layer with the given internal ID from the group."
            "hasLayer" => ILayer ^-> T<bool>
            |> WithComment "Returns true if the given layer is currently added to the group."
            "getLayer" => T<int> ^-> ILayer
            |> WithComment "Returns the layer with the given id."
            "getLayers" => T<unit> ^-> Type.ArrayOf ILayer
            |> WithComment "Returns an array of all the layers added to the group."
            "clearLayers" => T<unit -> unit>
            |> WithComment "Removes all the layers from the group."
            "eachLayer" => (ILayer ^-> T<unit>) ^-> T<unit>
            |> WithComment "Iterates over the layers of the group, optionally specifying context of the iterator function."
            Generic - fun t -> "eachLayer" => (t -* ILayer ^-> T<unit>) * t?context ^-> T<unit>
            |> WithComment "Iterates over the layers of the group, optionally specifying context of the iterator function."
            "toGeoJSON" => T<unit -> obj>
            |> WithComment "Returns a GeoJSON representation of the layer group (GeoJSON FeatureCollection)."
            "setZIndex" => T<int> ^-> T<unit>
            |> WithComment "Calls setZIndex on every layer contained in this group, passing the z-index."
            "getLayerId" => ILayer ^-> T<int>
            |> WithComment "Returns the internal ID for a layer"
        ]

    let FeatureGroup =
        Class "L.FeatureGroup"
        |=> Inherits LayerGroup
        |+> Static [
            Constructor (!? !| ILayer * !? T<obj>)
            |> WithComment "Create a layer group, optionally given an initial set of layers."
        ]
        |+> Instance [
            "getBounds" => T<unit> ^-> LatLngBounds
            |> WithComment "Returns the LatLngBounds of the Feature Group (created from bounds and coordinates of its children)."
            "setSytle" => PathOptions ^-> T<unit>
            |> WithComment "Sets the given path options to each layer of the group that has a setStyle method."
            "bringToFront" => T<unit -> unit>
            |> WithComment "Brings the layer group to the top of all other layers."
            "bringToBack" => T<unit -> unit>
            |> WithComment "Brings the layer group to the bottom of all other layers."
        ]
        |> WithEvents [
            "layeradd", LayerEvent, "Fired when a layer is added to the group."
            "layerremove", LayerEvent, "Fired when a layer is removed from the map."
            "add", Event, "Fired after the layer is added to a map"
            "remove", Event, "Fired after the layer is removed from a map"
            "popupopen", PopupEvent, "Fired when a popup bound to this layer is opened"
            "popupclose", PopupEvent, "Fired when a popup bound to this layer is closed"
            "tooltipopen", TooltipEvent, "Fired when a tooltip bound to this layer is opened."
            "tooltipclose", TooltipEvent, "Fired when a tooltip bound to this layer is closed."
        ]

    let GeoJSONT = Type.New()

    let GeoJSONOptions =
        Class "L.GeoJSON.Options"
        |=> Inherits PathOptions
        |+> Static [Constructor T<unit> |> WithInline "{}"]
        |+> Instance [
            "pointToLayer" =@ GeoJSONT * LatLngOrCoords ^-> T<unit>
            |> WithComment "Function that will be used for creating layers for GeoJSON points (if not specified, simple markers will be created)."
            "style" =@ GeoJSONT ^-> T<unit>
            |> WithComment "Function that will be used to get style options for vector layers created for GeoJSON features."
            "onEachFeature" =@ GeoJSONT * ILayer ^-> T<unit>
            |> WithComment "Function that will be called on each created feature layer. Useful for attaching events and popups to features."
            "filter" =@ GeoJSONT * ILayer ^-> T<bool>
            |> WithComment "Function that will be used to decide whether to show a feature or not."
            "coordsToLatLng" =@ T<float * float> ^-> LatLng
            |> WithComment "Function that will be used for converting GeoJSON coordinates to LatLng points (if not specified, coords will be assumed to be WGS84 — standard [longitude, latitude] values in degrees)."
            "markersInheritOptions" =@ T<bool>
            |> WithComment "Whether default Markers for Point type Features inherit from group options."
        ]

    let GeoJSON =
        Class "L.GeoJSON"
        |=> GeoJSONT
        |=> Nested [GeoJSONOptions]
        |=> Inherits FeatureGroup
        |+> Instance [
            "addData" => GeoJSONT ^-> T<unit>
            |> WithComment "Adds a GeoJSON object to the layer."
            "setStyle" => (GeoJSONT ^-> T<unit>) ^-> T<unit>
            |> WithComment "Changes styles of GeoJSON vector layers with the given style function."
            "resetStyle" => Path ^-> T<unit>
            |> WithComment "Resets the the given vector layer's style to the original GeoJSON style, useful for resetting style after hover events."
        ]
        |+> Static [
            Constructor (!? T<obj> * !? GeoJSONOptions)
            |> WithComment "Create a layer group, optionally given an initial set of layers."
            "geometryToLayer" => GeoJSONT * !?(GeoJSONT * LatLngOrCoords ^-> T<unit>) ^-> ILayer
            |> WithComment "Creates a layer from a given GeoJSON feature."
            "coordsToLatLng" => (T<float * float> + T<float[]>) * !?T<bool>?reverse ^-> LatLng
            |> WithComment "Creates a LatLng object from an array of 2 numbers (latitude, longitude) used in GeoJSON for points. If reverse is set to true, the numbers will be interpreted as (longitude, latitude)."
            "coordsToLatLngs" => (T<(float * float)[]> + T<float[][]>)?coords * T<bool>?reverse ^-> Type.ArrayOf LatLng
            |> WithInline "L.GeoJSON.coordsToLatLngs($coords, 0, $reverse)"
            |> WithComment "Creates a multidimensional array of LatLng objects from a GeoJSON coordinates array. If reverse is set to true, the numbers will be interpreted as (longitude, latitude)."
            "coordsToLatLngs" => (T<(float * float)[]> + T<float[][]>)?coords ^-> Type.ArrayOf LatLng
            |> WithInline "L.GeoJSON.coordsToLatLngs($coords, 0)"
            |> WithComment "Creates a multidimensional array of LatLng objects from a GeoJSON coordinates array. If reverse is set to true, the numbers will be interpreted as (longitude, latitude)."
            "coordsToLatLngs" => (T<(float * float)[][]> + T<float[][][]>)?coords * T<bool>?reverse ^-> Type.ArrayOf (Type.ArrayOf LatLng)
            |> WithInline "L.GeoJSON.coordsToLatLngs($coords, 1, $reverse)"
            |> WithComment "Creates a multidimensional array of LatLng objects from a GeoJSON coordinates array. If reverse is set to true, the numbers will be interpreted as (longitude, latitude)."
            "coordsToLatLngs" => (T<(float * float)[][]> + T<float[][][]>)?coords ^-> Type.ArrayOf (Type.ArrayOf LatLng)
            |> WithInline "L.GeoJSON.coordsToLatLngs($coords, 1)"
            |> WithComment "Creates a multidimensional array of LatLng objects from a GeoJSON coordinates array. If reverse is set to true, the numbers will be interpreted as (longitude, latitude)."
            "coordsToLatLngs" => (T<(float * float)[][][]> + T<float[][][][]>)?coords * T<bool>?reverse ^-> Type.ArrayOf (Type.ArrayOf (Type.ArrayOf LatLng))
            |> WithInline "L.GeoJSON.coordsToLatLngs($coords, 2, $reverse)"
            |> WithComment "Creates a multidimensional array of LatLng objects from a GeoJSON coordinates array. If reverse is set to true, the numbers will be interpreted as (longitude, latitude)."
            "coordsToLatLngs" => (T<(float * float)[][][]> + T<float[][][][]>)?coords ^-> Type.ArrayOf (Type.ArrayOf (Type.ArrayOf LatLng))
            |> WithInline "L.GeoJSON.coordsToLatLngs($coords, 2)"
            |> WithComment "Creates a multidimensional array of LatLng objects from a GeoJSON coordinates array. If reverse is set to true, the numbers will be interpreted as (longitude, latitude)."
            "latLngToCoords" => LatLng * !? T<int> ^-> !| (T<int> * T<int>)
            |> WithComment "Reverse of coordsToLatLng"
            "latLngsToCoords" => !| LatLng * !? T<int> * !? T<bool> ^-> !| (T<int> * T<int>)
            |> WithComment "Reverse of coordsToLatLngs closed determines whether the first point should be appended to the end of the array to close the feature, only used when levelsDeep is 0. False by default."
            "asFeature" => T<obj> ^-> T<obj>
            |> WithComment "Normalize GeoJSON geometries/features into GeoJSON features."
        ]

    let GridLayerOptions =
        Class "L.GridLayer.Options"
        |=> Inherits PathOptions
        |+> Static [Constructor T<unit> |> WithInline "{}"]
        |+> Instance [
            "tileSize" =@ T<int> + Point
            |> WithComment "Width and height of tiles in the grid. Use a number if width and height are equal, or L.point(width, height) otherwise."
            "opacity" =@ T<float>
            |> WithComment "Opacity of the tiles. Can be used in the createTile() function."
            "updateWhenIdle" =@ T<bool>
            |> WithComment "Load new tiles only when panning ends. true by default on mobile browsers, in order to avoid too many requests and keep smooth navigation. false otherwise in order to display new tiles during panning, since it is easy to pan outside the keepBuffer option in desktop browsers."
            "updateWhenZooming" =@ T<bool>
            |> WithComment "By default, a smooth zoom animation (during a touch zoom or a flyTo()) will update grid layers every integer zoom level. Setting this option to false will update the grid layer only when the smooth animation ends."
            "updateInterval" =@ T<int>
            |> WithComment "Tiles will not update more than once every updateInterval milliseconds when panning."
            "zIndex" =@ T<int>
            |> WithComment "The explicit zIndex of the tile layer."
            "bounds" =@ LatLngBounds
            |> WithComment "If set, tiles will only be loaded inside the set LatLngBounds."
            "minZoom" =@ T<int>
            |> WithComment "The minimum zoom level down to which this layer will be displayed (inclusive)."
            "maxZoom" =@ T<int>
            |> WithComment "The maximum zoom level up to which this layer will be displayed (inclusive)."
            "maxNativeZoom" =@ T<int>
            |> WithComment "Maximum zoom number the tile source has available. If it is specified, the tiles on all zoom levels higher than maxNativeZoom will be loaded from maxNativeZoom level and auto-scaled."
            "minNativeZoom" =@ T<int>
            |> WithComment "Minimum zoom number the tile source has available. If it is specified, the tiles on all zoom levels lower than minNativeZoom will be loaded from minNativeZoom level and auto-scaled."
            "noWrap" =@ T<bool>
            |> WithComment "Whether the layer is wrapped around the antimeridian. If true, the GridLayer will only be displayed once at low zoom levels. Has no effect when the map CRS doesn't wrap around. Can be used in combination with bounds to prevent requesting tiles outside the CRS limits."
            "pane" =@ T<string>
            |> WithComment "Map pane where the grid layer will be added."
            "className" =@ T<string>
            |> WithComment "A custom class name to assign to the tile layer. Empty by default."
            "keepBuffer" =@ T<int>
            |> WithComment "When panning the map, keep this many rows and columns of tiles before unloading them."
        ]

    let GridLayer =
        Class "L.GridLayer"
        |=> Inherits GridLayerOptions
        |+> Static [
            Constructor (!? GridLayerOptions)
            |> WithComment "Create a layer group, optionally given an initial set of layers."
        ]
        |+> Instance [
            //"getBounds" => T<unit> ^-> LatLngBounds
            //|> WithComment "Returns the LatLngBounds of the Feature Group (created from bounds and coordinates of its children)."
        ]
        |> WithEvents [
            //"layeradd", LayerEvent, "Fired when a layer is added to the group."
        ]


    let ControlPosition =
        let ControlPosition = Type.New()
        Class "L.Control.Position"
        |=> ControlPosition
        |+> Static [
            "topleft" =? ControlPosition
            "topright" =? ControlPosition
            "bottomleft" =? ControlPosition
            "bottomright" =? ControlPosition
        ]

    let ControlOptions =
        Class "L.Control.Options"
        |+> Static [Constructor T<unit> |> WithInline "{}"]
        |+> Instance [
            "position" =@ ControlPosition
            |> WithComment "The initial position of the control (one of the map corners)."
        ]

    let ControlT = Type.New()

    let ControlZoomOptions =
        Class "L.Control.Zoom.Options"
        |+> Static [Constructor T<unit> |> WithInline "{}"]
        |+> Instance [
            "position" =@ ControlPosition
            "zoomInText" =@ T<string>
            "zoomOutText" =@ T<string>
            "zoomInTitle" =@ T<string>
            "zoomOutTitle" =@ T<string>
        ]

    let ControlZoom =
        Class "L.Control.Zoom"
        |=> Nested [ControlZoomOptions]
        |=> Inherits ControlT
        |+> Static [
            Constructor !?ControlZoomOptions
            |> WithComment "Creates a zoom control."
        ]

    let ControlAttributionOptions =
        Class "L.Control.Attribution.Options"
        |+> Static [Constructor T<unit> |> WithInline "{}"]
        |+> Instance [
            "position" =@ ControlPosition
            "prefix" =@ T<string>
            |> WithComment "The HTML text shown before the attributions. Pass false to disable."
        ]

    let ControlAttribution =
        Class "L.Control.Attribution"
        |=> Nested [ControlAttributionOptions]
        |=> Inherits ControlT
        |+> Static [
            Constructor !?ControlAttributionOptions
        ]
        |+> Instance [
            "setPrefix" => T<string -> unit>
            "addAttribution" => T<string -> unit>
            "removeAttribution" => T<string -> unit>
        ]

    let ControlLayersOptions =
        Class "L.Control.Layers.Options"
        |+> Static [Constructor T<unit> |> WithInline "{}"]
        |+> Instance [
            "position" =@ T<string>
            "collapsed" =@ T<bool>
            |> WithComment "If true, the control will be collapsed into an icon and expanded on mouse hover or touch."
            "autoZIndex" =@ T<bool>
            |> WithComment "If true, the control will assign zIndexes in increasing order to all of its layers so that the order is preserved when switching them on/off."
        ]

    let ControlLayers =
        Class "L.Control.Layers"
        |=> Nested [ControlLayersOptions]
        |=> Inherits ControlT
        |+> Static [
            Constructor (!?T<obj>?baseLayers * !?T<obj>?overlays * !?ControlLayersOptions)
            |> WithComment "Creates an attribution control with the given layers. Base layers will be switched with radio buttons, while overlays will be switched with checkboxes. Note that all base layers should be passed in the base layers object, but only one should be added to the map during map instantiation."
        ]
        |+> Instance [
            "addBaseLayer" => ILayer * T<string>?name ^-> T<unit>
            |> WithComment "Adds a base layer (radio button entry) with the given name to the control."
            "addOverlay" => ILayer * T<string>?name ^-> T<unit>
            |> WithComment "Adds an overlay (checkbox entry) with the given name to the control."
            "removeLayer" => ILayer ^-> T<unit>
            |> WithComment "Remove the given layer from the control."
        ]
        |> WithEvents [
            "baselayerchange", LayersControlEvent, "Fired when the base layer is changed through the control."
            "overlayadd", LayersControlEvent, "Fired when an overlay is selected through the control."
            "overlayremove", LayersControlEvent, "Fired when an overlay is deselected through the control."
        ]

    let ControlScaleOptions =
        Class "L.Control.Scale.Options"
        |+> Static [Constructor T<unit> |> WithInline "{}"]
        |+> Instance [
            "position" =@ T<string>
            "maxWidth" =@ T<int>
            |> WithComment "Maximum width of the control in pixels. The width is set dynamically to show round values (e.g. 100, 200, 500)."
            "metric" =@ T<bool>
            |> WithComment "Whether to show the metric scale line (m/km)."
            "imperial" =@ T<bool>
            |> WithComment "Whether to show the imperial scale line (mi/ft)."
            "updateWhenIdle" =@ T<bool>
            |> WithComment "If true, the control is updated on moveend, otherwise it's always up-to-date (updated on move)."
        ]

    let ControlScale =
        Class "L.Control.Scale"
        |=> Nested [ControlScaleOptions]
        |=> Inherits ControlT
        |+> Static [
            Constructor !?ControlScaleOptions
        ]

    let Control =
        Class "L.Control"
        |=> ControlT
        |=> Implements [IControl]
        |=> Nested [ControlOptions; ControlPosition; ControlZoom; ControlAttribution; ControlLayers; ControlScale]
        |+> Static [
            Constructor (!?ControlOptions)
            |> WithComment "Creates a control with the given options."
        ]
        |+> Instance [
            "setPosition" => ControlPosition ^-> T<unit>
            |> WithComment "Sets the position of the control."
            "getPosition" => T<unit> ^-> ControlPosition
            |> WithComment "Returns the current position of the control."
            "addTo" => MapT ^-> T<unit>
            |> WithComment "Adds the control to the map."
            "removeFrom" => MapT ^-> T<unit>
            |> WithComment "Removes the control from the map."
            "getContainer" => T<unit -> Element>
            |> WithComment "Returns the HTML container of the control."
        ]

    let LocateOptions =
        Class "L.LocateOptions"
        |+> Static [Constructor T<unit> |> WithInline "{}"]
        |+> Instance [
            "watch" =@ T<bool>
            |> WithComment "If true, starts continous watching of location changes (instead of detecting it once) using W3C watchPosition method. You can later stop watching using map.stopLocate() method."
            "setView" =@ T<bool>
            |> WithComment "If true, automatically sets the map view to the user location with respect to detection accuracy, or to world view if geolocation failed."
            "maxZoom" =@ T<int>
            |> WithComment "The maximum zoom for automatic view setting when using `setView` option."
            "timeout" =@ T<int>
            |> WithComment "Number of milliseconds to wait for a response from geolocation before firing a locationerror event."
            "maximumAge" =@ T<int>
            |> WithComment "Maximum age of detected location. If less than this amount of milliseconds passed since last geolocation response, locate will return a cached location."
            "enableHighAccuracy" =@ T<bool>
            |> WithComment "Enables high accuracy, see description in the W3C spec [https://dev.w3.org/geo/api/spec-source.html#high-accuracy]."
        ]

    let PanOptions =
        Class "L.PanOptions"
        |+> Static [Constructor T<unit> |> WithInline "{}"]
        |+> Instance [
            "animate" =@ T<bool>
            |> WithComment "If true, panning will always be animated if possible. If false, it will not animate panning, either resetting the map view if panning more than a screen away, or just setting a new offset for the map pane (except for `panBy` which always does the latter)."
            "duration" =@ T<float>
            |> WithComment "Duration of animated panning."
            "easeLinearity" =@ T<float>
            |> WithComment "The curvature factor of panning animation easing (third parameter of the Cubic Bezier curve). 1.0 means linear animation, the less the more bowed the curve."
            "noMoveStart" =@ T<bool>
            |> WithComment "If true, panning won't fire movestart event on start (used internally for panning inertia)."
        ]

    let ZoomOptions =
        Class "L.ZoomOptions"
        |+> Static [Constructor T<unit> |> WithInline "{}"]
        |+> Instance [
            "animate" =@ T<bool>
            |> WithComment "If not specified, zoom animation will happen if the zoom origin is inside the current view. If true, the map will attempt animating zoom disregarding where zoom origin is. Setting false will make it always reset the view completely without animation."
        ]

    let ZoomPanOptions =
        Class "L.ZoomPanOptions"
        |+> Static [Constructor T<unit> |> WithInline "{}"]
        |+> Instance [
            "reset" =@ T<bool>
            |> WithComment "If true, the map view will be completely reset (without any animations)."
            "pan" =@ PanOptions
            |> WithComment "Sets the options for the panning (without the zoom change) if it occurs."
            "zoom" =@ ZoomOptions
            |> WithComment "Sets the options for the zoom change if it occurs."
            "animate" =@ T<bool>
            |> WithComment "An equivalent of passing animate to both zoom and pan options (see below)."
        ]

    let FitBoundsOptions =
        Class "L.FitBoundsOptions"
        |+> Static [Constructor T<unit> |> WithInline "{}"]
        |+> Instance [
            "paddingTopLeft" =@ Point
            |> WithComment "Sets the amount of padding in the top left corner of a map container that shouldn't be accounted for when setting the view to fit bounds. Useful if you have some control overlays on the map like a sidebar and you don't want them to obscure objects you're zooming to."
            "paddingBottomRight" =@ Point
            |> WithComment "The same for bottom right corner of the map."
            "padding" =@ Point
            |> WithComment "Equivalent of setting both top left and bottom right padding to the same value."
            "maxZoom" =@ T<int>
            |> WithComment "The maximum possible zoom to use."
            "reset" =@ T<bool>
            |> WithComment "If true, the map view will be completely reset (without any animations)."
            "pan" =@ PanOptions
            |> WithComment "Sets the options for the panning (without the zoom change) if it occurs."
            "zoom" =@ ZoomOptions
            |> WithComment "Sets the options for the zoom change if it occurs."
            "animate" =@ T<bool>
            |> WithComment "An equivalent of passing animate to both zoom and pan options (see below)."
        ]

    let MapOptions =
        Class "L.Map.Options"
        |+> Static [Constructor T<unit> |> WithInline "{}"]
        |+> Instance [
            "preferCanvas" =@ T<bool>
            |> WithComment "Whether Paths should be rendered on a Canvas renderer. By default, all Paths are rendered in a SVG renderer."
            
            "center" =@ LatLng
            |> WithComment "Initial geographical center of the map."
            "zoom" =@ T<int>
            |> WithComment "Initial map zoom."
            "layers" =@ Type.ArrayOf ILayer
            |> WithComment "Layers that will be added to the map initially."
            "minZoom" =@ T<int>
            |> WithComment "Minimum zoom level of the map. Overrides any minZoom set on map layers."
            "maxZoom" =@ T<int>
            |> WithComment "Maximum zoom level of the map. This overrides any maxZoom set on map layers."
            "maxBounds" =@ LatLngBounds
            |> WithComment "When this option is set, the map restricts the view to the given geographical bounds, bouncing the user back when he tries to pan outside the view. To set the restriction dynamically, use setMaxBounds method"
            "crs" =@ CRS
            |> WithComment "Coordinate Reference System to use. Don't change this if you're not sure what it means."
            "renderer" =@ Renderer
            |> WithComment "The default method for drawing vector layers on the map. L.SVG or L.Canvas by default depending on browser support."
            "zoomSnap" =@ T<int>
            |> WithComment "the zoom level snaps to the nearest integer; lower values (e.g. 0.5 or 0.1) allow for greater granularity. A value of 0 means the zoom level will not be snapped after fitBounds or a pinch-zoom."
            "zoomDelta" =@ T<int>
            |> WithComment "Controls how much the map's zoom level will change after a zoomIn(), zoomOut(), pressing + or - on the keyboard, or using the zoom controls. Values smaller than 1 (e.g. 0.5) allow for greater granularity."

            "dragging" =@ T<bool>
            |> WithComment "Whether the map be draggable with mouse/touch or not."
            "touchZoom" =@ T<bool>
            |> WithComment "Whether the map can be zoomed by touch-dragging with two fingers."
            "scrollWhellZoom" =@ T<bool>
            |> WithComment "Whether the map can be zoomed by using the mouse wheel. If passed 'center', it will zoom to the center of the view regardless of where the mouse was."
            "doubleClickZoom" =@ T<bool>
            |> WithComment "Whether the map can be zoomed in by double clicking on it and zoomed out by double clicking while holding shift. If passed 'center', double-click zoom will zoom to the center of the view regardless of where the mouse was."
            "boxZoom" =@ T<bool>
            |> WithComment "Whether the map can be zoomed to a rectangular area specified by dragging the mouse while pressing shift."
            "tap" =@ T<bool>
            |> WithComment "Enables mobile hacks for supporting instant taps (fixing 200ms click delay on iOS/Android) and touch holds (fired as contextmenu events)."
            "tapTolerance" =@ T<int>
            |> WithComment "The max number of pixels a user can shift his finger during touch for it to be considered a valid tap."
            "trackResize" =@ T<bool>
            |> WithComment "Whether the map automatically handles browser window resize to update itself."
            "worldCopyJump" =@ T<bool>
            |> WithComment "With this option enabled, the map tracks when you pan to another \"copy\" of the world and seamlessly jumps to the original one so that all overlays like markers and vector layers are still visible."
            "closePopupOnClick" =@ T<bool>
            |> WithComment "Set it to false if you don't want popups to close when user clicks the map."
            "bounceAtZoomLimits" =@ T<bool>
            |> WithComment "Set it to false if you don't want the map to zoom beyond min/max zoom and then bounce back when pinch-zooming."

            "keyboard" =@ T<bool>
            |> WithComment "Makes the map focusable and allows users to navigate the map with keyboard arrows and +/- keys."
            "keyboardPanDelta" =@ T<int>
            |> WithComment "Amount of pixels to pan when pressing an arrow key."

            "inertia" =@ T<bool>
            |> WithComment "If enabled, panning of the map will have an inertia effect where the map builds momentum while dragging and continues moving in the same direction for some time. Feels especially nice on touch devices."
            "inertiaDeceleration" =@ T<int>
            |> WithComment "The rate with which the inertial movement slows down, in pixels/second^2."
            "inertiaMaxSpeed" =@ T<int>
            |> WithComment "Max speed of the inertial movement, in pixels/second."
            "easeLinearity" =@ T<int>
            |> WithComment ""
            "worldCopyJump" =@ T<bool>
            |> WithComment "With this option enabled, the map tracks when you pan to another copy of the world and seamlessly jumps to the original one so that all overlays like markers and vector layers are still visible."
            "maxBoundsViscosity" =@ T<int>
            |> WithComment "If maxBounds is set, this option will control how solid the bounds are when dragging the map around. The default value of 0.0 allows the user to drag outside the bounds at normal speed, higher values will slow down map dragging outside bounds, and 1.0 makes the bounds fully solid, preventing the user from dragging outside the bounds."

            "zoomControl" =@ T<bool>
            |> WithComment "Whether the zoom control is added to the map by default."
            "attributionControl" =@ T<bool>
            |> WithComment "Whether the attribution control is added to the map by default."

            "transform3DLimit" =@ T<int>
            |> WithComment "Defines the maximum size of a CSS translation transform. The default value should not be changed unless a web browser positions layers in the wrong place after doing a large panBy."
            "fadeAnimation" =@ T<bool>
            |> WithComment "Whether the tile fade animation is enabled. By default it's enabled in all browsers that support CSS3 Transitions except Android."
            "zoomAnimation" =@ T<bool>
            |> WithComment "Whether the tile zoom animation is enabled. By default it's enabled in all browsers that support CSS3 Transitions except Android."
            "zoomAnimationThreshold" =@ T<bool>
            |> WithComment "Won't animate zoom if the zoom difference exceeds this value."
            "markerZoomAnimation" =@ T<bool>
            |> WithComment "Whether markers animate their zoom with the zoom animation, if disabled they will disappear for the length of the animation. By default it's enabled in all browsers that support CSS3 Transitions except Android."
        
            "scrollWheelZoom" =@ T<bool> + T<string>
            |> WithComment "Whether the map can be zoomed by using the mouse wheel. If passed 'center', it will zoom to the center of the view regardless of where the mouse was."
            "wheelDebounceTime" =@ T<int>
            |>WithComment "Limits the rate at which a wheel can fire (in milliseconds). By default user can't zoom via wheel more often than once per 40 ms."
            "wheelPxPerZoomLevel" =@ T<int>
            |> WithComment "How many scroll pixels (as reported by L.DomEvent.getWheelDelta) mean a change of one full zoom level. Smaller values will make wheel-zooming faster (and vice versa)."
        ]

    let MapPanes =
        Class "L.MapPanes"
        |+> Instance [
            "mapPane" =? T<Element>
            |> WithComment "Pane that contains all other map panes."
            "tilePane" =? T<Element>
            |> WithComment "Pane for tile layers."
            "shadowPane" =? T<Element>
            |> WithComment "Pane for overlay shadows (e.g. marker shadows)."
            "overlayPane" =? T<Element>
            |> WithComment "Pane for overlays like polylines and polygons."
            "markerPane" =? T<Element>
            |> WithComment "Pane for marker icons."
            "popupPane" =? T<Element>
            |> WithComment "Pane for popups."
            "tooltipPane" =? T<Element>
            |> WithComment "Pane for Tooltips."
        ]

    let MarkerOptions =
        Class "L.Marker.Options"
        |+> Static [Constructor T<unit> |> WithInline "{}"]
        |+> Instance [
            "icon" =@ Icon
            |> WithComment "Icon class to use for rendering the marker. See Icon documentation for details on how to customize the marker icon. Set to new L.Icon.Default() by default."
            "keyboard" =@ T<bool>
            |> WithComment "Whether the marker can be tabbed to with a keyboard and clicked by pressing enter."
            "title" =@ T<string>
            |> WithComment "Text for the browser tooltip that appear on marker hover (no tooltip by default)."
            "alt" =@ T<string>
            |> WithComment "Text for the alt attribute of the icon image (useful for accessibility)."
            "zIndexOffset" =@ T<int>
            |> WithComment "By default, marker images zIndex is set automatically based on its latitude. Use this option if you want to put the marker on top of all others (or below), specifying a high value like 1000 (or high negative value, respectively)."
            "opacity" =@ T<float>
            |> WithComment "The opacity of the marker."
            "riseOnHover" =@ T<bool>
            |> WithComment "If true, the marker will get on top of others when you hover the mouse over it."
            "riseOffset" =@ T<int>
            |> WithComment "The z-index offset used for the riseOnHover feature."
            "pane" =@ T<string>
            |> WithComment "Map pane where the markers icon will be added."
            "shadowPane" =@ T<string>
            |> WithComment "Map pane where the markers shadow will be added."
            "bubblingMouseEvents" =@ T<bool>
            |> WithComment "When true, a mouse event on this marker will trigger the same event on the map (unless L.DomEvent.stopPropagation is used)."
            "draggable" =@ T<bool>
            |> WithComment "Whether the marker is draggable with mouse/touch or not."
            "autoPan" =@ T<bool>
            |> WithComment "Whether to pan the map when dragging this marker near its edge or not."
            "autoPanPadding" =@ Point
            |> WithComment "Distance (in pixels to the left/right and to the top/bottom) of the map edge to start panning the map."
            "autoPanSpeed" =@ T<int>
            |> WithComment "Number of pixels the map should pan by."
            "interactive" =@ T<bool>
            |> WithComment "If false, the layer will not emit mouse events and will act as a part of the underlying map."
            "attribution" =@ T<string>
            |> WithComment "String to be shown in the attribution control, e.g. © OpenStreetMap contributors. It describes the layer data and is often a legal obligation towards copyright holders and tile providers."
        ]

    let Marker =
        Class "L.Marker"
        |=> Nested [MarkerOptions]
        |=> Implements [ILayer]
        |+> Static [
            Constructor (LatLngOrCoords * !?MarkerOptions)
            |> WithComment "Instantiates a Marker object given a geographical point and optionally an options object."
        ]
        |> WithEvents [
            "click", MouseEvent, "Fired when the user clicks (or taps) the marker."
            "dblclick", MouseEvent, "Fired when the user double-clicks (or double-taps) the marker."
            "mousedown", MouseEvent, "Fired when the user pushes the mouse button on the marker."
            "mouseover", MouseEvent, "Fired when the mouse enters the marker."
            "mouseup", MouseEvent, "Fired when the user releases the mouse button pushed on the layer."
            "mouseout", MouseEvent, "Fired when the mouse leaves the marker."
            "contextmenu", MouseEvent, "Fired when the user right-clicks on the marker."
            "dragstart", Event, "Fired when the user starts dragging the marker."
            "drag", Event, "Fired repeatedly while the user drags the marker."
            "dragend", Event, "Fired when the user stops dragging the marker."
            "move", Event, "Fired when the marker is moved via setLatLng. New coordinate include in event arguments."
            "add", Event, "Fired when the marker is added to the map."
            "remove", Event, "Fired when the marker is removed from the map."
            "popupopen", PopupEvent, "Fired when a popup bound to the marker is open."
            "popupclose", PopupEvent, "Fired when a popup bound to the marker is closed."
            "movestart", Event, "Fired when the marker starts moving (because of dragging)."
            "moveend", Event, "Fired when the marker stops moving (because of dragging)."
            "tooltipopen", TooltipEvent, "Fired when a tooltip bound to this layer is opened."
            "tooltipclose", TooltipEvent, "Fired when a tooltip bound to this layer is closed."

        ]
        |+> Instance [
            // Methods
            "addTo" => (MapT + LayerGroup) ^-> T<unit>
            |> WithComment "Adds the marker to the map."
            "getLatLng" => T<unit> ^-> LatLng
            |> WithComment "Returns the current geographical position of the marker."
            "setLatLng" => LatLngOrCoords ^-> T<unit>
            |> WithComment "Changes the marker position to the given point."
            "setIcon" => Icon ^-> T<unit>
            |> WithComment "Changes the marker icon."
            "setZIndexOffset" => T<int> ^-> T<unit>
            |> WithComment "Changes the zIndex offset of the marker."
            "setOpacity" => T<float> ^-> T<unit>
            |> WithComment "Changes the opacity of the marker."
            "update" => T<unit -> unit>
            |> WithComment "Updates the marker position, useful if coordinates of its latLng object were changed directly."
            "bindPopup" => (T<string> + T<Element> + Popup) * !?PopupOptions ^-> T<unit>
            |> WithComment "Binds a popup with a particular HTML content to a click on this marker. You can also open the bound popup with the Marker openPopup method."
            "unbindPopup" => T<unit -> unit>
            |> WithComment "Unbinds the popup previously bound to the marker with bindPopup."
            "openPopup" => T<unit -> unit>
            |> WithComment "Opens the popup previously bound by the bindPopup method."
            "getPopup" => T<unit> ^-> Popup
            |> WithComment "Returns the popup previously bound by the bindPopup method."
            "closePopup" => T<unit -> unit>
            |> WithComment "Closes the bound popup of the marker if it's opened."
            "togglePopup" => T<unit -> unit>
            |> WithComment "Toggles the popup previously bound by the bindPopup method."
            "setPopupContent" => (T<string> + T<Element>) * !?PopupOptions ^-> T<unit>
            |> WithComment "Sets an HTML content of the popup of this marker."
            "isPopupOpen"=> T<unit> ^-> T<bool>
            |> WithComment "Returns true if the popup bound to this layer is currently open."
            "toGeoJSON" => T<unit -> obj>
            |> WithComment "Returns a GeoJSON representation of the marker (GeoJSON Point Feature)."
            "getIcon" => T<unit> ^-> Icon
            |> WithComment "Returns the current icon used by the marker"
            "toGeoJSON" => T<int> ^-> T<unit>
            |> WithComment "precision is the number of decimal places for coordinates. The default value is 6 places. Returns a GeoJSON representation of the marker (as a GeoJSON Point Feature)."
            "remove" => T<unit> ^-> T<unit>
            |> WithComment "Removes the layer from the map it is currently active on."
            "removeFrom" => (MapT + LayerGroup) ^-> T<unit>
            |> WithComment "Removes the layer from the given map/layergroup"
            "getPane" => T<string> ^-> T<Element>
            |> WithComment "Returns the HTMLElement representing the named pane on the map. If name is omitted, returns the pane for this layer."
            "getAttribution" => T<unit> ^-> T<string>
            |> WithComment "Used by the attribution control, returns the attribution option."
            "bindTooltip" => (T<string> + T<Element> + Popup) * !?TooltipOptions ^-> T<unit>
            |> WithComment "Binds a tooltip to the layer with the passed content and sets up the necessary event listeners. If a Function is passed it will receive the layer as the first argument and should return a String or HTMLElement."
            "unbindTooltip" => T<unit> ^-> T<unit>
            |> WithComment "Removes the tooltip previously bound with bindTooltip."
            "openTooltip" => !? LatLng ^-> T<unit>
            |> WithComment "Opens the bound tooltip at the specified latlng or at the default tooltip anchor if no latlng is passed."
            "closeTooltip" => T<unit> ^-> T<unit>
            |> WithComment "Closes the tooltip bound to this layer if it is open."
            "toggleTooltip" => T<unit> ^-> T<unit>
            |> WithComment "Opens or closes the tooltip bound to this layer depending on its current state."
            "isTooltipOpen" => T<unit> ^-> T<bool>
            |> WithComment "Returns true if the tooltip bound to this layer is currently open."
            "setTooltipContent" => T<string> + T<Element> + Tooltip ^-> T<unit>
            |> WithComment "Sets the content of the tooltip bound to this layer."
            "getTooltip" => T<unit> ^-> Tooltip
            |> WithComment "Returns the tooltip bound to this layer."
            "on" => T<string> * (T<'a> ^-> T<'a>) * !? T<obj> ^-> T<unit>
            |> WithComment "Adds a listener function (fn) to a particular event type of the object. You can optionally specify the context of the listener (object the this keyword will point to). You can also pass several space-separated types (e.g. 'click dblclick')."
            "on" => T<obj> ^-> T<unit>
            |> WithComment "Adds a set of type/listener pairs, e.g. {click: onClick, mousemove: onMouseMove}"
            "off" => T<string> * (T<'a> ^-> T<'a>) * !? T<obj> ^-> T<unit>
            |> WithComment "Removes a previously added listener function. If no function is specified, it will remove all the listeners of that particular event from the object. Note that if you passed a custom context to on, you must pass the same context to off in order to remove the listener."
            "off" => T<obj> ^-> T<unit>
            |> WithComment "Removes a set of type/listener pairs."
            "off" => T<unit> ^-> T<unit>
            |> WithComment "Removes all listeners to all events on the object. This includes implicitly attached events."
            "fire" => T<string> * T<obj> * !? T<bool> ^-> T<unit>
            |> WithComment "Fires an event of the specified type. You can optionally provide an data object — the first argument of the listener function will contain its properties. The event can optionally be propagated to event parents."
            "listens" => T<string> ^-> T<bool>
            |> WithComment "Returns true if a particular event type has any listeners attached to it."
            "once" => T<string> * (T<'a> ^-> T<'a>) * !? T<obj> ^-> T<unit>
            |> WithComment "Behaves as on(…), except the listener will only get fired once and then removed."
            "once" => T<obj> ^-> T<unit>
            |> WithComment "Behaves as on(…), except the listener will only get fired once and then removed."
            "addEventParent" => Evented ^-> T<unit>
            |> WithComment "Adds an event parent - an Evented that will receive propagated events"
            "removeEventParent" => Evented ^-> T<unit>
            |> WithComment "Removes an event parent, so it will stop receiving propagated events"




            // Properties
            "dragging" =? IHandler
            |> WithComment "Marker dragging handler (by both mouse and touch)."
        ]

    let Map =
        Class "L.Map"
        |=> MapT
        |=> Nested [MapOptions]
        |+> Static [
            Constructor ((T<Element> + T<string>)?id * !?MapOptions)
            |> WithComment "Instantiates a map object given a div element (or its id) and optionally an object literal with map options."
        ]
        |> WithEvents [
            "click", MouseEvent, "Fired when the user clicks (or taps) the map."
            "dblclick", MouseEvent, "Fired when the user double-clicks (or double-taps) the map."
            "mousedown", MouseEvent, "Fired when the user pushes the mouse button on the map."
            "mouseup", MouseEvent, "Fired when the user pushes the mouse button on the map."
            "mouseover", MouseEvent, "Fired when the mouse enters the map."
            "mouseout", MouseEvent, "Fired when the mouse leaves the map."
            "mousemove", MouseEvent, "Fired while the mouse moves over the map."
            "contextmenu", MouseEvent, "Fired when the user pushes the right mouse button on the map, prevents default browser context menu from showing if there are listeners on this event. Also fired on mobile when the user holds a single touch for a second (also called long press)."
            "focus", Event, "Fired when the user focuses the map either by tabbing to it or clicking/panning."
            "blur", Event, "Fired when the map looses focus."
            "preclick", MouseEvent, "Fired before mouse click on the map (sometimes useful when you want something to happen on click before any existing click handlers start running)."
            "load", Event, "Fired when the map is initialized (when its center and zoom are set for the first time)."
            "unload", Event, "Fired when the map is destroyed with remove method."
            "viewreset", Event, "Fired when the map needs to redraw its content (this usually happens on map zoom or load). Very useful for creating custom overlays."
            "movestart", Event, "Fired when the view of the map starts changing (e.g. user starts dragging the map)."
            "move", Event, "Fired on any movement of the map view."
            "moveend", Event, "Fired when the view of the map ends changed (e.g. user stopped dragging the map)."
            "dragstart", Event, "Fired when the user starts dragging the map."
            "drag", Event, "Fired repeatedly while the user drags the map."
            "dragend", DragEndEvent, "Fired when the user stops dragging the map."
            "zoom", Event, "Fired repeatedly during any change in zoom level, including zoom and fly animations."
            "zoomstart", Event, "Fired when the map zoom is about to change (e.g. before zoom animation)."
            "zoomend", Event, "Fired when the map zoom changes."
            "zoomlevelschange", Event, "Fired when the number of zoomlevels on the map is changed due to adding or removing a layer."
            "resize", ResizeEvent, "Fired when the map is resized."
            "autopanstart", Event, "Fired when the map starts autopanning when opening a popup."
            "layeradd", LayerEvent, "Fired when a new layer is added to the map."
            "layerremove", LayerEvent, "Fired when some layer is removed from the map."
            "baselayerchange", LayerEvent, "Fired when the base layer is changed through the layer control."
            "overlayadd", LayerEvent, "Fired when an overlay is selected through the layer control."
            "overlayremove", LayerEvent, "Fired when an overlay is deselected through the layer control."
            "locationfound", LocationEvent, "Fired when geolocation (using the locate method) went successfully."
            "locationerror", ErrorEvent, "Fired when geolocation (using the locate method) failed."
            "popupopen", PopupEvent, "Fired when a popup is opened (using openPopup method)."
            "popupclose", PopupEvent, "Fired when a popup is closed (using closePopup method)."
            "tooltipopen", TooltipEvent, "Fired when a tooltip is opened in the map."
            "tooltipclose", TooltipEvent, "Fired when a tooltip in the map is closed."
            "keypress", KeyboardEvent, "Fired when the user presses a key from the keyboard that produces a character value while the map is focused."
            "keydown", KeyboardEvent, "Fired when the user presses a key from the keyboard while the map is focused. Unlike the keypress event, the keydown event is fired for keys that produce a character value and for keys that do not produce a character value."
            "keyup", KeyboardEvent, "Fired when the user releases a key from the keyboard while the map is focused."
            "zoomanim", ZoomAnimEvent, "Fired at least once per zoom animation. For continuous zoom, like pinch zooming, fired once per frame during zoom."
        ]
        |+> Instance [
            // Methods
            "getRenderer" => Path ^-> Renderer
            |> WithComment "Returns the instance of Renderer that should be used to render the given Path. It will ensure that the renderer options of the map and paths are respected, and that the renderers do exist on the map."

            "setView" => LatLngOrCoords?center * !?T<int>?zoom * !?ZoomPanOptions?options ^-> T<unit>
            |> WithComment "Sets the view of the map (geographical center and zoom) with the given animation options."
            "setZoom" => T<int>?zoom * !?ZoomOptions?options ^-> T<unit>
            |> WithComment "Sets the zoom of the map."
            "zoomIn" => !?T<int>?delta * !?ZoomOptions?options ^-> T<unit>
            |> WithComment "Increases the zoom of the map by delta (1 by default)."
            "zoomOut" => !?T<int>?delta * !?ZoomOptions?options ^-> T<unit>
            |> WithComment "Decreases the zoom of the map by delta (1 by default)."
            "setZoomAround" => LatLngOrCoords?latlng * T<int>?zoom * ZoomOptions?options ^-> T<unit>
            |> WithComment "Zooms the map while keeping a specified geographical point on the map stationary (e.g. used internally for scroll zoom and double-click zoom)."
            "setZoomAround" => Point * T<int> * ZoomOptions ^-> T<unit>
            |> WithComment "Zooms the map while keeping a specified pixel on the map (relative to the top-left corner) stationary."
            "fitBounds" => LatLngBoundsOrCoords?bounds * !?FitBoundsOptions?options ^-> T<unit>
            |> WithComment "Sets a map view that contains the given geographical bounds with the maximum zoom level possible."
            "fitWorld" => !?FitBoundsOptions?options ^-> T<unit>
            |> WithComment "Sets a map view that mostly contains the whole world with the maximum zoom level possible."
            "panTo" => LatLngOrCoords?latlng * !?PanOptions?options ^-> T<unit>
            |> WithComment "Pans the map to a given center. Makes an animated pan if new center is not more than one screen away from the current one."
            "panInside" => LatLng * PanOptions ^-> T<unit>
            |> WithComment "Pans the map the minimum amount to make the latlng visible. Use padding, paddingTopLeft and paddingTopRight options to fit the display to more restricted bounds, like fitBounds. If latlng is already within the (optionally padded) display bounds, the map will not be panned."

            "panInsideBounds" => LatLngBoundsOrCoords?bounds * !?PanOptions?options ^-> T<unit>
            |> WithComment "Pans the map to the closest view that would lie inside the given bounds (if it's not already), controlling the animation using the options specific, if any."
            "panBy" => PointOrCoords?point * !?PanOptions?options ^-> T<unit>
            |> WithComment "Pans the map by a given number of pixels (animated)."
            "invalidateSize" => T<bool>?animate ^-> T<unit>
            |> WithComment "Checks if the map container size changed and updates the map if so — call it after you've changed the map size dynamically, also animating pan by default."
            "invalidateSize" => ZoomPanOptions?options ^-> T<unit>
            |> WithComment "Checks if the map container size changed and updates the map if so — call it after you've changed the map size dynamically, also animating pan by default. If options.pan is false, panning will not occur. If options.debounceMoveend is true, it will delay moveend event so that it doesn't happen often even if the method is called many times in a row."
            "setMaxBounds" => LatLngBoundsOrCoords?bounds ^-> T<unit>
            |> WithComment "Restricts the map view to the given bounds (see map maxBounds option)."
            "locate" => LocateOptions?options ^-> T<unit>
            |> WithComment "Tries to locate the user using the Geolocation API, firing a locationfound event with location data on success or a locationerror event on failure, and optionally sets the map view to the user's location with respect to detection accuracy (or to the world view if geolocation failed). See Locate options for more details."
            "stopLocate" => T<unit> ^-> T<unit>
            |> WithComment "Stops watching location previously initiated by map.locate({watch: true}) and aborts resetting the map view if map.locate was called with {setView: true}."
            "remove" => T<unit> ^-> T<unit>
            |> WithComment "Destroys the map and clears all related event listeners."
            "flyTo" => LatLng * T<int> * ZoomPanOptions ^-> T<unit>
            |> WithComment "Sets the view of the map (geographical center and zoom) performing a smooth pan-zoom animation."
            "setMinZoom" => T<int> ^-> T<unit>
            |> WithComment "Sets the lower limit for the available zoom levels (see the minZoom option)."
            "setMaxZoom" => T<int> ^-> T<unit>
            |> WithComment "Sets the upper limit for the available zoom levels (see the maxZoom option)."
            "stop" => T<unit> ^-> T<unit>
            |> WithComment "Stops the currently running panTo or flyTo animation, if any."

            "getCenter" => T<unit> ^-> LatLng
            |> WithComment "Returns the geographical center of the map view."
            "getZoom" => T<unit> ^-> T<int>
            |> WithComment "Returns the current zoom of the map view."
            "getMinZoom" => T<unit> ^-> T<int>
            |> WithComment "Returns the minimum zoom level of the map."
            "getMaxZoom" => T<unit> ^-> T<int>
            |> WithComment "Returns the maximum zoom level of the map."
            "getBounds" => T<unit> ^-> LatLngBounds
            |> WithComment "Returns the LatLngBounds of the current map view."
            "getBoundsZoom" => LatLngBoundsOrCoords * !?T<bool>?inside ^-> T<int>
            |> WithComment "Returns the maximum zoom level on which the given bounds fit to the map view in its entirety. If inside (optional) is set to true, the method instead returns the minimum zoom level on which the map view fits into the given bounds in its entirety."
            "getSize" => T<unit> ^-> Point
            |> WithComment "Returns the current size of the map container."
            "getPixelBounds" => T<unit> ^-> Bounds
            |> WithComment "Returns the bounds of the current map view in projected pixel coordinates (sometimes useful in layer and overlay implementations)."
            "getPixelOrigin" => T<unit> ^-> Point
            |> WithComment "Returns the projected pixel coordinates of the top left point of the map layer (useful in custom layer and overlay implementations)."
            "getPixelWorldBounds" => T<int> ^-> Bounds
            |> WithComment "Returns the world's bounds in pixel coordinates for zoom level zoom. If zoom is omitted, the map's current zoom level is used."

            "addLayer" => ILayer ^-> T<unit>
            |> WithComment "Adds the given layer to the map. If optional insertAtTheBottom is set to true, the layer is inserted under all others (useful when switching base tile layers)."
            "removeLayer" => ILayer ^-> T<unit>
            |> WithComment "Removes the given layer from the map."
            "hasLayer" => ILayer ^-> T<bool>
            |> WithComment "Returns true if the given layer is currently added to the map."
            "eachLayer" => (ILayer ^-> T<unit>) ^-> T<unit>
            |> WithComment "Iterates over the layers of the map, optionally specifying context of the iterator function."
            "openPopup" => Popup ^-> T<unit>
            |> WithComment "Opens the specified popup while closing the previously opened (to make sure only one is opened at one time for usability)."
            "openPopup" => (T<string> + T<Element>) * LatLng * !?PopupOptions ^-> T<unit>
            |> WithComment "Creates a popup with the specified options and opens it in the given point on a map."
            "closePopup" => !?Popup ^-> T<unit>
            |> WithComment "Closes the popup previously opened with openPopup (or the given one)."
            "addControl" => IControl ^-> T<unit>
            |> WithComment "Adds the given control to the map."
            "removeControl" => IControl ^-> T<unit>
            |> WithComment "Removes the given control from the map."
            "openTooltip" => Tooltip ^-> T<unit>
            |> WithComment "Opens the specified tooltip."
            "openTooltip" => (T<string> + T<Element>) * LatLng * !? TooltipOptions ^-> T<unit>
            |> WithComment "Creates a tooltip with the specified content and options and open it."
            "closeTooltip" => Tooltip ^-> T<unit>

            "latLngToLayerPoint" => LatLngOrCoords ^-> Point
            |> WithComment "Returns the map layer point that corresponds to the given geographical coordinates (useful for placing overlays on the map)."
            "layerPointToLatLng" => PointOrCoords ^-> LatLng
            |> WithComment "Returns the geographical coordinates of a given map layer point."
            "containerPointToLayerPoint" => PointOrCoords ^-> Point
            |> WithComment "Converts the point relative to the map container to a point relative to the map layer."
            "layerPointToContainerPoint" => PointOrCoords ^-> Point
            |> WithComment "Converts the point relative to the map layer to a point relative to the map container."
            "latLngToContainerPoint" => LatLngOrCoords ^-> Point
            |> WithComment "Returns the map container point that corresponds to the given geographical coordinates."
            "containerPointToLatLng" => PointOrCoords ^-> LatLng
            |> WithComment "Returns the geographical coordinates of a given map container point."
            "project" => LatLngOrCoords * !?T<int>?zoom ^-> Point
            |> WithComment "Projects the given geographical coordinates to absolute pixel coordinates for the given zoom level (current zoom level by default)."
            "unproject" => PointOrCoords * !?T<int>?zoom ^-> LatLng
            |> WithComment "Projects the given absolute pixel coordinates to geographical coordinates for the given zoom level (current zoom level by default)."
            "mouseEventToContainerPoint" => MouseEvent ^-> Point
            |> WithComment "Returns the pixel coordinates of a mouse click (relative to the top left corner of the map) given its event object."
            "mouseEventToLayerPoint" => MouseEvent ^-> Point
            |> WithComment "Returns the pixel coordinates of a mouse click relative to the map layer given its event object."
            "mouseEventToLatLng" => MouseEvent ^-> LatLng
            |> WithComment "Returns the geographical coordinates of the point the mouse clicked on given the click's event object."
            "wrapLatLng" => LatLng ^-> LatLng
            |> WithComment "Returns a LatLng where lat and lng has been wrapped according to the map's CRS's wrapLat and wrapLng properties, if they are outside the CRS's bounds. By default this means longitude is wrapped around the dateline so its value is between -180 and +180 degrees."
            "wrapLatLngBounds" => LatLngBounds  ^-> LatLngBounds
            |> WithComment "Returns a LatLngBounds with the same size as the given one, ensuring that its center is within the CRS's bounds. By default this means the center longitude is wrapped around the dateline so its value is between -180 and +180 degrees, and the majority of the bounds overlaps the CRS's bounds."
            "distance" => LatLng * LatLng ^-> T<int>
            |> WithComment "Returns the distance between two geographical coordinates according to the map's CRS. By default this measures distance in meters."


            "addHandler" => T<string> * (T<'a> ^-> T<'a>) ^-> T<unit>
            |> WithComment "Adds a new Handler to the map, given its name and constructor function."
            "remove" => T<unit> ^-> T<unit>
            |> WithComment "Destroys the map and clears all related event listeners."
            "createPane" => T<string> * !? T<Element> ^-> T<Element>
            |> WithComment "Creates a new map pane with the given name if it doesn't exist already, then returns it. The pane is created as a child of container, or as a child of the main map pane if not set."
            "getPane" => T<string> + T<Element> ^-> T<Element>
            |> WithComment "Returns a map pane, given its name or its HTML element (its identity)."
            "getContainer" => T<unit> ^-> T<Element>
            |> WithComment "Returns the container element of the map."
            "getPanes" => T<unit> ^-> MapPanes
            |> WithComment "Returns an object with different map panes (to render overlays in)."
            "whenReady" => T<unit -> unit> * !?T<obj>?context ^-> T<unit>
            |> WithComment "Runs the given callback when the map gets initialized with a place and zoom, or immediately if it happened already, optionally passing a function context."

            "getZoomScale" => T<int> * T<int> ^-> T<int>
            |> WithComment "Returns the scale factor to be applied to a map transition from zoom level fromZoom to toZoom. Used internally to help with zoom animations."
            "getScaleZoom" => T<int> * T<int> ^-> T<int>
            |> WithComment "Returns the zoom level that the map would end up at, if it is at fromZoom level and everything is scaled by a factor of scale. Inverse of getZoomScale."
        
            //properties

            "dragging" =? IHandler
            |> WithComment "Map dragging handler (by both mouse and touch)."
            "touchZoom" =? IHandler
            |> WithComment "Touch zoom handler."
            "doubleClickZoom" =? IHandler
            |> WithComment "Double click zoom handler."
            "scrollWheelZoom" =? IHandler
            |> WithComment "Scroll wheel zoom handler."
            "boxZoom" =? IHandler
            |> WithComment "Box (shift-drag with mouse) zoom handler."
            "keyboard" =? IHandler
            |> WithComment "Keyboard navigation handler."
            "tap" =? IHandler
            |> WithComment "Mobile touch hacks (quick tap and touch hold) handler."
            "zoomControl" =? ControlZoom
            |> WithComment "Zoom control."
            "attributionControl" =? ControlAttribution
            |> WithComment "Attribution control."
        ]

    let Browser =
        Class "L.Browser"
        |+> Static [
            "ie" =? T<bool>
            |> WithComment "true for all Internet Explorer versions."
            "ie6" =? T<bool>
            |> WithComment "true for Internet Explorer 6."
            "ie7" =? T<bool>
            |> WithComment "true for Internet Explorer 7."
            "webkit" =? T<bool>
            |> WithComment "true for webkit-based browsers like Chrome and Safari (including mobile versions)."
            "webkit3d" =? T<bool>
            |> WithComment "true for webkit-based browsers that support CSS 3D transformations."
            "android" =? T<bool>
            |> WithComment "true for Android mobile browser."
            "android23" =? T<bool>
            |> WithComment "true for old Android stock browsers (2 and 3)."
            "mobile" =? T<bool>
            |> WithComment "true for modern mobile browsers (including iOS Safari and different Android browsers)."
            "mobileWebkit" =? T<bool>
            |> WithComment "true for mobile webkit-based browsers."
            "mobileOpera" =? T<bool>
            |> WithComment "true for mobile Opera."
            "touch" =? T<bool>
            |> WithComment "true for all browsers on touch devices."
            "msTouch" =? T<bool>
            |> WithComment "true for browsers with Microsoft touch model (e.g. IE10)."
            "retina" =? T<bool>
            |> WithComment "true for devices with Retina screens."
        ]

    module Res =
        let Css =
            Resource "Css" "https://cdnjs.cloudflare.com/ajax/libs/leaflet/0.7.2/leaflet.css"
        let Js =
            Resource "Js" "https://cdnjs.cloudflare.com/ajax/libs/leaflet/0.7.2/leaflet.js"
            |> Requires [Css]

    let Assembly =
        Assembly [
            Namespace "WebSharper.Leaflet.Resources" [
                Res.Css
                Res.Js
            ]
            Namespace "WebSharper.Leaflet" [
                LatLng
                LatLngBounds
                Point
                Bounds
                Icon
                DivIcon
                Transformation
                IHandler
                ILayer
                IControl
                IProjection
                ICRS
                CRS
                Projection
                Event
                MouseEvent
                LocationEvent
                ErrorEvent
                LayerEvent
                LayersControlEvent
                TileEvent
                ResizeEvent
                GeoJSONEvent
                PopupEvent
                DragEndEvent
                Popup
                TileLayer
                ImageOverlay
                Path
                Polyline
                MultiPolyline
                Polygon
                MultiPolygon
                Rectangle
                Circle
                CircleMarker
                LayerGroup
                FeatureGroup
                GeoJSON
                Control
                LocateOptions
                PanOptions
                ZoomOptions
                ZoomPanOptions
                FitBoundsOptions
                MapPanes
                Marker
                Map
                Browser
            ]
        ]
        |> Requires [Res.Js]

open WebSharper.InterfaceGenerator

[<Sealed>]
type Extension() =
    interface IExtension with
        member ext.Assembly =
            Definition.Assembly

[<assembly: Extension(typeof<Extension>)>]
do ()
