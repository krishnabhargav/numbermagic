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
    | Karmic of int
 
type Number with    
    static member private singleDigit i = 
        if i = 0 then 
            0 
        else
            let mod9 = i % 9
            if mod9 = 0 then 9
            else mod9
    
    static member convert (i : int) = 
        match i with
        | 11 | 13 | 14 | 16 | 19 | 22 -> Number.Karmic(i)
        | _ -> (Number.singleDigit i) |> Number.Single
    
    static member liftUp (Number.Single(x)) = x |> Number.convert
    
    static member alwaysSingle n = 
        match n with
        | Number.Karmic(x) ->  x |> Number.singleDigit |> Number.Single
        | Number.Single(x) as s -> s

    static member combine f m n = 
        let inner_combine (Number.Single(x)) (Number.Single(y)) = 
            (f x y) |> Number.Single
        inner_combine (Number.alwaysSingle m) (Number.alwaysSingle n)
    
    static member sum = Number.combine (+)
    
    static member diff = Number.combine (-)

//magic
module Numbers = 
    let reduce f (str : string) = 
        str.ToLower().ToCharArray()
        |> Array.filter f
        |> Array.map (fun c -> (c |> int) - 96)
        |> Array.sum
        |> Number.convert
    
    let destinyNumber (name : Name) = 
        let { firstName = fname; middleName = mname; lastName = lname } = name
        let f = fun x -> true
        [| fname |> reduce f
           mname |> reduce f
           lname |> reduce f |]
        |> Array.reduce (Number.sum)
        |> Number.liftUp
    
    let talentNumber (dob : DateOfBirth) = 
        let { day = d; month = m; year = y } = dob
        [| d |> Number.convert
           m |> Number.convert
           y |> Number.convert |]
        |> Array.reduce Number.sum
        |> Number.liftUp
    
    let heartNumber { firstName = fname; middleName = mname; lastName = lname } = 
        let f = 
            function 
            | 'a' | 'e' | 'i' | 'o' | 'u' -> true
            | _ -> false
        [| fname |> reduce f
           mname |> reduce f
           lname |> reduce f |]
        |> Array.reduce (Number.sum)
        |> Number.liftUp
    
    let personalityNumber { firstName = fname; middleName = mname; lastName = lname } = 
        let f = 
            function 
            | 'a' | 'e' | 'i' | 'o' | 'u' -> false
            | _ -> true
        [| fname |> reduce f
           mname |> reduce f
           lname |> reduce f |]
        |> Array.reduce (Number.sum)
        |> Number.liftUp
    
    let ultimateNumber { name = name; dob = dob } = 
        let d = destinyNumber name
        let t = talentNumber dob
        Number.sum d t |> Number.liftUp
    
    let challengeNumber { day = day; month = month; year = year } = 
        let convertSingle = (Number.convert >> Number.alwaysSingle)
        let nd = day |> convertSingle
        let nm = month |> convertSingle
        let ny = year |> convertSingle
        let fs = Number.diff nm nd |> Number.liftUp
        let sn = Number.diff nd ny |> Number.liftUp
        let th = Number.diff fs sn |> Number.liftUp
        let fr = Number.diff nm ny |> Number.liftUp
        fs , sn, th, fr
