(* Jacob Coomer *)
(* This program reads a list of numbers from user input. 
 * The numbers are passed to an object, which can execute functions on them
 * Functions are: sum, average, largest, and reverse. *)
 
(* Define the list processing class *)
class num_list n = object (self)
  val numbers = n

  (* Recursively sum the list of numbers *)
  method sum =
    let rec aux = function
      | [] -> 0
      | head :: tail -> head + aux tail
    in aux numbers

  (* Calculate the average *)
  method average =
     let sum = self#sum in 
    (float_of_int sum) /. (float_of_int (List.length numbers))

  (* Recursively find the largest number in the list *)
  method largest =
    let rec aux = function
      | [] -> min_int
      | head :: tail -> max head (aux tail)
    in aux numbers
    
  (* Using imperative .fold_left and lambda function to reverse the list *)
  method reverse =
    List.fold_left (fun acc head -> head :: acc) [] numbers
end

(* Imperative let to set input to user input *)
let read_numbers () =
  print_endline "Enter numbers separated by spaces:";
  let input = read_line () in
  (* Mapping each number to its int variant as a list *)
  List.map int_of_string (String.split_on_char ' ' input)


let rec main num_list_obj =
  print_endline "\nChoose an option:";
  print_endline "1. Find the sum of the numbers";
  print_endline "2. Find the average of the numbers";
  print_endline "3. Find the largest number";
  print_endline "4. Reverse the list of numbers";
  print_endline "5. Close";
  let choice = read_line () in
  match choice with
  | "1" ->
      Printf.printf "The sum is: %d\n"  num_list_obj#sum;
      main num_list_obj
  | "2" ->
      Printf.printf "The average is: %.2f\n"  num_list_obj#average;
      main num_list_obj
  | "3" ->
      Printf.printf "The largest number is: %d\n"  num_list_obj#largest;
      main num_list_obj
  | "4" ->
      let reversed_numbers = num_list_obj#reverse in
      Printf.printf "Reversed list: %s\n" (String.concat " " (List.map string_of_int reversed_numbers));
      main num_list_obj
  | "5" -> print_endline "Exiting!"
  | _ ->
      print_endline "Invalid option. Try again.";
      main num_list_obj

let () =
  let numbers = read_numbers () in
  let num_list_obj = new num_list numbers in
  main num_list_obj