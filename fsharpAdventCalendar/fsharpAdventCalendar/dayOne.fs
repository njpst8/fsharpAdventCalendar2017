namespace FSharpAdventCalendar

  module dayOne =
    let rec doStuff (elem1, list:(int List)) = 
      printfn "%i %A" elem1 list
      match list.Tail.IsEmpty, elem1 = List.head list with
      | true, true -> elem1 + List.head list
      | true, _ -> 0
      | _, true -> elem1 + List.head list + doStuff(list.Head, list.Tail)
      | _, _ -> 0 + doStuff(list.Head, list.Tail)

    let stringToList (s:string) =
        [for c in s -> (int)(c.ToString())]

    let rec last (list:(int List)) = 
      match list.Tail with
      | [] -> list.Head
      | _ -> last(list.Tail)

    let f =
     let list = stringToList("124221")
     let result = doStuff(list.Head, list.Tail)
     let result2 = last(list)

     match list.Head = result2 with 
     | true -> result + list.Head + result2
     | _ -> result
