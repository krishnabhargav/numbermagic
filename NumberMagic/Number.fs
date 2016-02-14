namespace NumberMagic

//types
type Name = 
    { firstName : string
      middleName : string
      lastName : string }

type Name with 
    static member build (str : string) = 
        let x = str.Split([| ' ' |])
        let splits = x |> Array.length
        if splits = 2 then 
            { firstName = x.[0]
              lastName = x.[1]
              middleName = "" }
        elif splits = 3 then 
            { firstName = x.[0]
              middleName = x.[1]
              lastName = x.[2] }
        else 
            { firstName = x.[0]
              middleName = ""
              lastName = "" }

type DateOfBirth = 
    { day : int
      month : int
      year : int }

type DateOfBirth with
    static member build (m, d, y) = 
        { day = d
          month = m
          year = y }

type Person = 
    { name : Name
      dob : DateOfBirth }

type Number = 
    | Single of int
    | Compound of int
    | Both of Number * Number
 
type Number with    
    static member private singleDigit i = 
        if i = 0 then 
            0 
        else
            let mod9 = i % 9
            if mod9 = 0 then 9
            else mod9
    
    static member convert (i : int) = 
        Number.Both ((Number.singleDigit i) |> Number.Single, i |> Number.Compound)
    
    static member alwaysSingle n = 
        match n with
        | Number.Compound(x) ->  x |> Number.singleDigit |> Number.Single
        | Number.Single(x) as s -> s
        | Number.Both (a, _) -> a

    static member combine f m n = 
        //printfn "%A %A" m n
        let m1, m2 =
            match m with
            | Number.Both (Number.Single(x), Number.Compound(y)) -> x, y
        let n1, n2 =
            match n with
            | Number.Both (Number.Single(x), Number.Compound(y)) -> x, y
         
        let a = f m1 n1
        let b = f m2 n2
        Number.Both(Number.Single (a) |> Number.alwaysSingle, Number.Compound (b))
        
    static member sum = Number.combine (+)
    
    static member diff = Number.combine (-)

//magic
module Numbers = 
    let cheiro c =
        match c with
        | 'a' | 'i' | 'j' | 'q' | 'y' -> 1
        | 'b' | 'k' | 'r' -> 2
        | 'c' | 'g' | 'l' | 's' -> 3
        | 'd' | 'm' | 't' -> 4
        | 'e' | 'h' | 'n' | 'x'  -> 5
        | 'u' | 'v' | 'w' -> 6
        | 'o' | 'z' -> 7
        | 'f' | 'p' -> 8
        | _ -> 0
    
    let normal c = 
        let v = c |> int
        let v1 = (v - 96) % 9
        if v1 = 0 then 9 else v1
    
    let reduce f (str : string) = 
        str.ToLower().ToCharArray()
        |> Array.filter f
        |> Array.map (cheiro)
        |> Array.sum
        |> Number.convert
    
    let destinyNumber (name : Name) = 
        let { firstName = fname; middleName = mname; lastName = lname } = name
        let f = fun x -> true
        [| fname |> reduce f
           mname |> reduce f
           lname |> reduce f |]
        |> Array.reduce (Number.sum)
    
    let nameNumber { firstName = fname; middleName = _; lastName = _ } = 
        let f = fun x -> true
        fname 
        |> reduce f
    
    let talentNumber (dob : DateOfBirth) = 
        let { day = d; month = m; year = y } = dob
        [| d |> Number.convert
           m |> Number.convert
           y |> Number.convert |]
        |> Array.reduce Number.sum
    
    let heartNumber { firstName = fname; middleName = mname; lastName = lname } = 
        let f = 
            function 
            | 'a' | 'e' | 'i' | 'o' | 'u' -> true
            | _ -> false
        [| fname |> reduce f
           mname |> reduce f
           lname |> reduce f |]
        |> Array.reduce (Number.sum)
    
    let personalityNumber { firstName = fname; middleName = mname; lastName = lname } = 
        let f = 
            function 
            | 'a' | 'e' | 'i' | 'o' | 'u' -> false
            | _ -> true
        [| fname |> reduce f
           mname |> reduce f
           lname |> reduce f |]
        |> Array.reduce (Number.sum)
    
    let ultimateNumber { name = name; dob = dob } = 
        let d = destinyNumber name
        let t = talentNumber dob
        Number.sum d t
    
    let challengeNumber { day = day; month = month; year = year } = 
        let convertSingle = (Number.convert >> Number.alwaysSingle)
        let nd = day |> convertSingle
        let nm = month |> convertSingle
        let ny = year |> convertSingle
        let fs = Number.diff nm nd 
        let sn = Number.diff nd ny 
        let th = Number.diff fs sn 
        let fr = Number.diff nm ny
        fs , sn, th, fr

(*        
module BabyNames = 
    open FSharp.Data
    
    let extractNames (url:string) = 
        HtmlDocument.Load(url)
        |> HtmlDocument.descendantsWithPath true (fun x -> HtmlNode.attributeValue "id" x = "name")
        |> Seq.map (fst)
        |> Seq.map (fun x -> HtmlNode.innerText)
            
    let private urlFormat = "http://www.nameslist.org/indian/baby-names/girl/{letter}/{number}"
    
    let forAlphabet (ah:string) = 
        let url = urlFormat.Replace("{letter}", ah.ToUpper())
        [1..100]
        |> List.map (fun x -> extractNames (url.Replace("{number}", x |> string)))
        
*)