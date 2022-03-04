module TypeClass

open System
open Kind.Data.Star
open Xunit
open TypeClass.Functor
open Kind.Data.Maybe
open TypeClass
open Encoding

[<Fact>]
let ``Dimap Star`` () =
    let star = Star (fun (f:int) -> Just f)
    let star' () = dimap (fun (f:string) -> Int32.Parse(f)) (fun (b: Int32) -> (b + 3).ToString() + "") star
    let starResult = unwrap (run star 3)
    let starResult' =  unwrap (run (star'()) "3")
    Assert.Equal(Some(3), starResult)
    Assert.Equal(Some("6"), starResult')