namespace WebSharper.Leaflet.Tests

open WebSharper.Html.Server
open WebSharper
open WebSharper.Sitelets

type Action =
    | Home

module Controls =

    [<Sealed>]
    type EntryPoint() =
        inherit Web.Control()

        [<JavaScript>]
        override __.Body =
            Client.Main() :> _

module Skin =
    open System.Web

    type Page =
        {
            Title : string
            Body : list<Element>
        }

    let MainTemplate =
        Content.Template<Page>("~/Main.html")
            .With("title", fun x -> x.Title)
            .With("body", fun x -> x.Body)

    let WithTemplate title body : Async<Content<Action>> =
        Content.WithTemplate MainTemplate
            {
                Title = title
                Body = body
            }

module Site =

    let HomePage ctx =
        Skin.WithTemplate "Leaflet maps"
            [
                Div [new Controls.EntryPoint()]
            ]

    let Main =
        Sitelet.Content "/" Home HomePage

[<Sealed>]
type Website() =
    interface IWebsite<Action> with
        member this.Sitelet = Site.Main
        member this.Actions = [Home]

[<assembly: Website(typeof<Website>)>]
do ()
