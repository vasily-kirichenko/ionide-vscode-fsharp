namespace Ionide.VSCode.FSharp

open System
open Fable.Core
open Fable.Import
open Fable.Import.vscode
open Fable.Import.Node

open DTO
open Ionide.VSCode.Helpers

module CodeLens =
    let private createProvider () =

        let convertToKind code =
            match code with
            | "C" -> SymbolKind.Class     (*  CompletionItemKind.Class      *)
            | "E" -> SymbolKind.Enum      (*  CompletionItemKind.Enum       *)
            | "S" -> SymbolKind.Property      (*  CompletionItemKind.Value      *)
            | "I" -> SymbolKind.Interface     (*  CompletionItemKind.Interface  *)
            | "N" -> SymbolKind.Module      (*  CompletionItemKind.Module     *)
            | "M" -> SymbolKind.Method     (*  CompletionItemKind.Method     *)
            | "P" -> SymbolKind.Property      (*  CompletionItemKind.Property   *)
            | "F" -> SymbolKind.Variable     (*  CompletionItemKind.Field      *)
            | "T" -> SymbolKind.Class      (*  CompletionItemKind.Class      *)
            | _   -> 0 |> unbox

        let mapRes (doc : TextDocument) o =
             o.Data |> Array.map (fun syms ->
                let cl = createEmpty<CodeLens>

                cl.range <-  Range
                            ( float syms.Declaration.BodyRange.StartLine - 1.,
                                float syms.Declaration.BodyRange.StartColumn - 1.,
                                float syms.Declaration.BodyRange.EndLine - 1.,
                                float syms.Declaration.BodyRange.EndColumn - 1.)
                let cls =  syms.Nested |> Array.map (fun sym ->
                    let cl = createEmpty<CodeLens>

                    cl.range <-  Range
                                ( float sym.BodyRange.StartLine - 1.,
                                    float sym.BodyRange.StartColumn - 1.,
                                    float sym.BodyRange.EndLine - 1.,
                                    float sym.BodyRange.EndColumn - 1.)
                    cl )
                cls |> Array.append (Array.create 1 cl)) |> Array.concat


        { new CodeLensProvider
          with
            member this.provideCodeLenses(doc, ct) =
                promise {
                    let! _ = Linter.parseFile doc
                    let! o = LanguageService.declarations doc.fileName
                    let data = mapRes doc o
                    Browser.console.log("Provide", data)

                    return data |> ResizeArray
                } |> Case2
            member this.resolveCodeLens(cl, ct) =
                promise {
                    let! o = LanguageService.symbolUseProject (window.activeTextEditor.document.fileName) (int cl.range.start.line + 1) (int cl.range.start.character + 1)
                    let cmd = createEmpty<Command>
                    cmd.title <- sprintf "%d References" o.Data.Uses.Length
                    cl.command <- cmd
                    Browser.console.log("Resolve", cl, o)
                    return cl
                } |> Case2
        }

    let activate selector (disposables: Disposable[]) =
        languages.registerCodeLensProvider(selector, createProvider())
        |> ignore
        ()
