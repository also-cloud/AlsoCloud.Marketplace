module AlsoCloud.Marketplace

open EdIlyin.FSharp.Elm.Core.Decode
open EdIlyin.FSharp.Elm.Core
open EdIlyin.FSharp.Boxcar
open EdIlyin.FSharp.Http
open FSharp.Data


type Action =
    | Create
    | Modify
    | Delete


let action decoder =
    decoder
        |> Decode.andThen
            (function
                | "CREATE" -> Ok Create
                | "MODIFY" -> Ok Modify
                | "DELETE" -> Ok Delete

                | other ->
                    sprintf "Unexpected action '%s'." other
                        |> Error

                >> Decode.fromResult
            )


let callbackHelper url status result =
    let body =
        ["status", Json.Encode.string status]
            @ result
            |> Json.Encode.object'
            |> Json.Encode.encode
            |> HttpRequestBody.TextRequest

    let headers =
        [HttpRequestHeaders.ContentType
            "application/json; charset=UTF-8"
        ]

    let decoder = Response.statusCode 200 >>. Response.bodyText
    
    // // Workaround for PreProd connection issue //
    let tls12: System.Net.SecurityProtocolType =
        LanguagePrimitives.EnumOfValue 3072

    do System.Net.ServicePointManager.SecurityProtocol <-
        tls12

    Http.AsyncRequest (url = url, headers = headers, body = body)
        |> Response.unpack decoder


let callbackCompleted url arguments =
    match arguments with
        | [] -> []

        | list ->
            ["outArguments",
                List.map
                    (fun (name, value) ->
                        Json.Encode.object'
                            [   "Name", Json.Encode.string name
                                "Value", value
                            ]
                    )
                    list
                    |> Json.Encode.list
            ]

        |> callbackHelper url "Completed"


let callbackFailed url errorDetails =
    callbackHelper url "Failed"
        ["errorDetails", Json.Encode.string errorDetails]
