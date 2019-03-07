module Synthesis

let abelar a =
    (fun n -> 12 < n && n < 3097  && n % 12 =0) a
   

let area b h =
   match b < 0.0 || h < 0.0 with 
   | true -> failwith "Negative value given"
   | _ -> b/2.0*h
   
    

let zollo x =
    match x < 0 with 
    | true -> x * -1
    | _ -> x * 2

let min x y =
    match x < y with 
    | true -> x 
    | _ -> y

let max x y =
    match x > y with 
    | true -> x 
    | _ -> y

let ofTime h m s =
    h*60*60 + m*60 + s

let toTime t = 
      let h = t/3600 in 
      let m = (t-h*3600)/60 in
      let s = t - (h*3600 + m*60) 
      match t > 0 with 
      | true -> h,m,s
      | _ -> 0,0,0

let digits n =
    failwith "Not implemented"


let minmax (a,b,c,d) =
    failwith "Not implemented"

let isLeap _ =
    failwith "Not implemented"

let month _ =
    failwith "Not implemented"

let toBinary _ =
    failwith "Not implemented"

let bizFuzz _ =
    failwith "Not implemented"

let monthDay _ _ =
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"