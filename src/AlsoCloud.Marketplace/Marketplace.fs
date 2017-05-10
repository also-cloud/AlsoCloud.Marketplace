namespace AlsoCloud

open EdIlyin.FSharp.Elm.Core.Json
open EdIlyin.FSharp.Http
open EdIlyin.FSharp.Elm.Core.Decode
open FSharp.Data


module Marketplace =
    let callbackHelper url status result =
        let body =
            ["status", Encode.string status]
                @ result
                |> Encode.object'
                |> Encode.encode
                |> HttpRequestBody.TextRequest

        let headers =
            [HttpRequestHeaders.ContentType
                "application/json; charset=UTF-8"
            ]

        let decoder = Response.statusCode 200 >>. Response.bodyText

        Http.AsyncRequest (url = url, headers = headers, body = body)
            |> Response.unpack decoder


    let callbackCompleted url arguments =
        match arguments with
            | [] -> []

            | list ->
                ["outArguments",
                    List.map
                        (fun (name, value) ->
                            Encode.object'
                                [   "Name", Encode.string name
                                    "Value", value
                                ]
                        )
                        list
                        |> Encode.list
                ]

            |> callbackHelper url "Completed"


    let callbackFailed url errorDetails =
        callbackHelper url "Failed"
            ["errorDetails", Encode.string errorDetails]
