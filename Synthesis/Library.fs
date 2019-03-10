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
    let rec count i c =
     match i/10 = 0 with
     |true -> c
     |_ -> count (i/10) (c + 1)
    count n 1
    


let minmax (a,b,c,d) =
    let x = min (min a b) (min c d)
    let y = max (max a b) (max c d)
    (x,y)  

let isLeap y =
     match y < 1582 with 
     |true -> failwith "Not a valid year" 
     |_ ->  match y%100 = 0  with 
            |true -> y%4 = 0 &&  y%400 = 0
            |_ -> y%4 = 0 
  

let month = function
    |1 -> "January", 31
    |2 -> "February", 28
    |3 -> "March", 31
    |4 -> "April", 30
    |5 -> "May", 31
    |6 -> "June", 30
    |7 -> "July", 31
    |8 -> "August", 31
    |9 -> "September", 30
    |10 -> "October", 31
    |11 -> "November", 30
    |12 -> "December", 31
    |_ -> failwith "Invalid number entered, choose a number between 1 and 12"


let toBinary n =
   let rec bin x b = 
    match x with 
    | 0 -> "0" + b
    | 1 -> "1" + b
    | _ -> match x < 0 with
           |true -> failwith "Negative number"
           |_ -> match x % 2 = 0 with 
                 |true -> bin (x/2)("0" + b )                                           
                 | _ -> bin (x/2)("1" + b)
   bin n ("")


let bizFuzz i =
    let x = i/3
    let y = i/5
    let z = i/15
    match x >= 0 && y >= 0 && z >= 0 with
    |true -> x,y,z
    |_ -> 0,0,0
    
    

let monthDay d y =
    match isLeap y with
    | true ->  match d < 367 && d <> 0 with  // number of days for leap year 366, must fall within this num
               |true -> match month (match d/30 < 1 with  
                                     | true -> 1 // if division yeilds 0 must be in first  month 
                                     | _ -> match d%30 >= 5 && d/30 < 12 with 
                                            | true -> d/30 + 1
                                            | _ -> d/30 ) with    
                        | t,_ -> t  // returns string part of month func
               |_ -> failwith "invalid date"
    | _ -> match d < 366 && d <> 0 with  // number of days for year 365, must fall within this num
           |true -> match month (match d/30 < 1 && d/30 < 12 with
                                 | true -> 1 
                                 | _ -> match d%30 >= 5 with 
                                        | true -> d/30 + 1
                                        | _ -> d/30 ) with
                    | t,_ -> t 
           |_ -> failwith "invalid date"

let coord _ =
    failwith "Not implemented"