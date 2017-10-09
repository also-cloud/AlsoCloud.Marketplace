module AlsoCloud.Marketplace

open FSharp.Core
open FSharp.Data
open FSharp.EdIlyin.Core
open FSharp.EdIlyin.Http
open Hopac
open System
open System.Text.RegularExpressions


module JD = FSharp.EdIlyin.Core.Json.Decode
module JE = FSharp.EdIlyin.Core.Json.Encode


type InstanceId = string


type Instance = {
    host: string
    username: string
    password: string
}


type Action =
    | Create
    | Modify
    | Delete


type CallbackUrl = string


type AccountId = int


let action decoder =
    decoder
        |> Decode.andThen
            (function
                | "CREATE" -> Ok Create
                | "MODIFY" -> Ok Modify
                | "DELETE" -> Ok Delete

                | other ->
                    sprintf "Unexpected action '%s'." other
                        |> Result.Error

                >> Decode.fromResult
            )


let authenticate username password host =
    boxcar {
        let decoder =
            JD.field "UserAccountId" JD.int
                |> Response.statusCode200Json

        let baseUrl =
            sprintf "https://%s/DataModel/service.svc/rest/" host

        let connection =
            Net.CookieContainer ()
                |> Request.connection baseUrl Seq.empty

        // Workaround for PreProd connection issue //
        let tls12: System.Net.SecurityProtocolType =
            LanguagePrimitives.EnumOfValue 3072

        do System.Net.ServicePointManager.SecurityProtocol <-
            tls12
        /////////////////////////////////////////////

        let! userAccountId =
            JE.object
                [   "userName", JE.string username
                    "password", JE.string password
                ]
                |> Request.postJson decoder
                    "IAuthorizationService/Authenticate"
                    connection

        let response =
            Request.connection baseUrl
                [HttpRequestHeaders.ContentType
                    "application/json; charset=UTF-8"
                ]
                connection.jar
                , userAccountId

        return response
    }


type SearchResult = {
    accountId: int
    accountType: string
    displayName: string
    productName: string
    productDisplayName: string
}


//{"rparentccountId":100001,"accountType":"string","displayName":{"Value": "string"},"searchField":{"Name":"string","Value":"string"}}
//[{"AccountId":1171721,"AccountType":"Product","DisplayName":"Skykick Hands On Test","ProductName":"521266_BackupOrder_91798","ProductDisplayName":"Backup Order"}]
let searchAccount connection json =
    let searchResults =
        Decode.map5
            (fun aid at dn pn pdn ->
                {   accountId = aid
                    accountType = at
                    displayName = dn
                    productName = pn
                    productDisplayName = pdn
                }
            )
            (JD.field "AccountId" JD.int)
            (JD.field "AccountType" JD.string)
            (JD.field "DisplayName" JD.string)
            (JD.field "ProductName" JD.string)
            (JD.field "ProductDisplayName" JD.string)
            |> JD.list

    let decoder = Response.statusCode200Json searchResults

    Request.postJson decoder "IAccountService/Search" connection
        json


type Account = {
    id: int
    productName: string
    values: Map<string, Chiron.Json>
}


let accountIdToJson (accountId: AccountId) = JE.int accountId


let searchAccountByName rootAccountId displayName connection =
    let json =
        JE.object
            [   "parentAccountId", accountIdToJson rootAccountId
                "accountType", JE.string "Product"
                "displayName"
                    , JE.object ["Value", JE.string displayName]
            ]

    searchAccount connection json


/// Converts Marketplace REST URL to Account URL
let accountBaseUrl (connection: Request.Connection) accountId =
    Regex.Replace
        ( connection.host
        , @"(https:\/\/.+?\/)DataModel\/service\.svc\/rest\/"
        , sprintf "$1#/Account/View/%i" accountId
        )


let searchAccountByField (connection: Request.Connection) rootAccountId fieldName fieldValue displayName =
    let json =
        JE.object
            [   "parentAccountId", accountIdToJson rootAccountId
                "accountType", JE.string "Product"
                "displayName"
                    , JE.object ["Value", JE.string displayName]
                "searchField"
                    , JE.object
                        [   "Name", JE.string fieldName
                            "Value", JE.string fieldValue
                        ]
            ]

    do rootAccountId
        |> accountBaseUrl connection
        |> printfn "Searching for %s %s at %s" displayName
            fieldValue

    searchAccount connection json


let loadProductAccountById connection accountId =
    let request = JE.object ["accountId", accountIdToJson accountId]

    let account =
        JD.at [ "Product"; "Values"; "ProductName" ]
            JD.string
            |> Decode.andThen
                (fun productName ->
                    JD.at
                        [   sprintf "Product_%s" productName
                            "Values"
                        ]
                        (JD.dict JD.value)
                        |> Decode.map (tuple productName)
                )
            |> JD.field "Views"
            |> Decode.map
                (fun (productName, values) ->
                    {   id = accountId
                        productName = productName
                        values = values
                    }
                )

    let decoder = Response.statusCode200Json account

    Request.postJson decoder "IAccountService/LoadAccountById"
        connection request


let instance =
    Decode.map3
        (fun h u p -> { host = h; username = u; password = p })
        (JD.field "host" JD.string)
        (JD.field "username" JD.string)
        (JD.field "password" JD.string)


let instances = JD.dict instance


//{"accountId":668089,"viewValues":{"Product_521266_BackupOrder_93330":{"Values":{"ExchangeSeatsOrdered":7}}}}
// 200 null
let updateProductAccount connection accountId productName values =
    let request =
        JE.object
            [   "accountId", accountIdToJson accountId
                "viewValues"
                    , JE.object
                        [sprintf "Product_%s" productName,
                            JE.object ["Values", JE.object values]
                        ]
            ]

    let parser = Response.statusCode 200

    Request.postJson parser
        "IAccountService/UpdateAccount"
        connection
        request


type PossibleProductForAccount = {
    displayName: string
    productName: string
}


let getPossibleProductsForAccount connection accountId =
    let json = JE.object ["accountId", accountIdToJson accountId]

    let possibleProductsForAccount =
        Decode.map2
            (fun dn pn -> {displayName = dn; productName = pn})
            (JD.field "DisplayName" JD.string)
            (JD.field "ProductName" JD.string)
            |> JD.list

    let decoder = Response.statusCode200Json possibleProductsForAccount

    Request.postJson decoder
        "IAccountService/GetPossibleProductsForAccount"
        connection json


type FieldDefinition = {
    name: string
    type': string
}


type DefaultAccountViewForCreate = {
    name: string
    fieldDefinitions: FieldDefinition list
    values: Map<string, Chiron.Json>
}


let fixDefaultAccountViewValues (view: DefaultAccountViewForCreate) =
    let definitionMap =
        view.fieldDefinitions
            |> List.map (fun fd -> fd.name, fd.type')
            |> Map.ofList

    view.values
        |> Map.map
            (fun fieldName fieldValue ->
                try Map.tryFind fieldName definitionMap
                        |> Option.unwrap fieldValue
                            (fun fieldType ->
                                if fieldType = "int32"
                                    then match fieldValue with
                                            | Chiron.Json.String s ->
                                                int s |> JE.int

                                            | _ -> fieldValue

                                    else fieldValue
                            )
                with | _ -> fieldValue
            )


let getDefaultAccountViewsForCreate connection accountType rootAccountId productName =
    let json =
        JE.object
            [   "accountType", JE.string accountType // "Product"
                "parentAccountId", accountIdToJson rootAccountId // 668137
                "productName", JE.string productName // "521266_MigrationOrder_65319"
                "secondaryParentId", JE.Null // null
            ]

    let defaultAccountViewsForCreate =
        Decode.map3
            (fun n fd v ->
                {name = n; fieldDefinitions = fd; values = v}
            )
            (JD.field "Name" JD.string)
            (Decode.map2 (fun n t -> {name = n; type' = t})
                (JD.field "Name" JD.string)
                (JD.field "Type" JD.string)
                |> JD.list
                |> JD.field "FieldDefinition"
            )
            (JD.dict JD.value |> JD.field "Values")
            |> JD.list

    let decoder =
        Response.statusCode200Json defaultAccountViewsForCreate

    Request.postJson decoder
        "IAccountService/GetDefaultAccountViewsForCreate"
        connection json


type CreatedAccount = {
    accountId: int
    displayName: string
}


let createAccount connection views =
    let json =
        JE.object ["account", JE.object ["Views", JE.object views]]

    let createdAccount =
        Decode.map2
            (fun aid dn -> {accountId = aid; displayName = dn})
            (JD.field "AccountId" JD.int)
            (JD.field "DisplayName" JD.string)

    let parser = Response.statusCode200Json createdAccount

    Request.postJson parser "IAccountService/CreateAccount"
        connection json


let terminateAccount connection accountId terminatorId =
    let json = // {"accountId":731680,"sendToSdp":true,"terminationDate":null,"terminatorId":175719}
        JE.object
            [   "accountId", accountIdToJson accountId
                "sendToSdp", JE.bool true
                "terminationDate", JE.Null
                "terminatorId", accountIdToJson terminatorId
            ]

    let parser = Response.statusCode 200

    Request.postJson parser "IAccountService/TerminateAccount"
        connection json


let authenticationPromise host username password =
    authenticate username password host |> Promise.queue |> Job.map Ok


// Workaround for PreProd connection issue //
let tls12: System.Net.SecurityProtocolType =
    LanguagePrimitives.EnumOfValue 3072


do System.Net.ServicePointManager.SecurityProtocol <- tls12
/////////////////////////////////////////////


let callbackHelper url status result =
    let body =
        ["status", JE.string status]
            @ result
            |> JE.object
            |> JE.encode
            |> HttpRequestBody.TextRequest

    let headers =
        [HttpRequestHeaders.ContentType
            "application/json; charset=UTF-8"
        ]

    let decoder =
        Decode.map2 second (Response.statusCode 200) Response.bodyText


    Http.AsyncRequest (url = url, headers = headers, body = body)
        |> Response.unpack decoder


let callbackCompleted url arguments =
    match arguments with
        | [] -> []

        | list ->
            ["outArguments",
                List.map
                    (fun (name, value) ->
                        JE.object
                            [   "Name", JE.string name
                                "Value", value
                            ]
                    )
                    list
                    |> JE.list
            ]

        |> callbackHelper url "Completed"


let callbackFailed url errorDetails =
    callbackHelper url "Failed"
        ["errorDetails", JE.string errorDetails]


let fieldIntBool field =
    JD.field field JD.uint16
        |> Decode.andThen
            (function
                | 0us -> Ok false
                | 1us -> Ok true
                | got -> Decode.expectingButGot "a 0 or 1" got
                >> Decode.fromDecodeResult
            )


let fieldStringBool field =
    JD.field field JD.string
        |> Decode.andThen
            (function
                | "False" -> Ok false
                | "True" -> Ok true

                | got ->
                    Decode.expectingButGot
                        "a \"False\" or \"True\"" got

                >> Decode.fromDecodeResult
            )
