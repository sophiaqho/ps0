(*
                         CS 51 Problem Set 0
                           Getting Started
                             Spring 2020
*)

(* This problem set is a short program that prints out a survey and,
   if you wish, posts a message to a web server so that you can see
   OCaml in action. Don't worry if you don't understand many of the
   programming constructs used here. You'll become familiar with them
   over the course of the semester.

   To receive a key to enable posting your message, visit
   http://pset0.herokuapp.com/ and log in.
 *)



(* 1. Please define these variables with the appropriate values.  Be
   sure that these statements all type-check after editing them.  You
   can do this by typing "make all" in the terminal. *)

(* 1.a. Replace FIRST and LAST with your first and last name *)
let name : (string * string) = ("FIRST", "LAST");;



(* 1.b. Replace "Other ..." in the line starting "let class_year..."
   with your current year in school, using one of the patterns shown
   in the type definition for year below. For example,

     let class_year : year = Freshman ;;

   or

     let class_year : year = Other "HKS masters' student" ;;

 *)
type year = Freshman | Sophomore | Junior | Senior | Other of string;;

let class_year : year = Other "I haven't filled it in yet";;



(* 1.c. Replace "Other ..." in took_cs_50 with whether or not you took CS50,
 * with a pattern shown in the type definition for cs50 below. *)
type cs50 = Took | DidNotTake | Other of string;;

let took_cs_50 : cs50 = Other "I haven't filled it out yet";;



(* 1.d. Replace "Other ..." in my_system the operating system
 * of your computer. *)
type system = Mac | Windows10 | Windows7 | Linux | Other of string;;

let my_system : system = Other "I haven't filled it out yet";;



(* 1.e. Replace the string below with a message to post to the problem
   set website. *)
let exciting : string = "I'm excited about ....!" ;;



(* 1.f. Set this variable to true if you want your information,
   including the message in 1.g, to appear on
   http://pset0.herokuapp.com. Posting a message to the server is
   optional, and is only available as an exercise to those students
   who have an @college.harvard.edu email address (required for
   authentication).  All other students should leave the `post` flag
   set to `false`. *)
let post : bool = false;;



(* 1.g. Set this variable to the key you got from pset0.herokuapp.com
   under the "key" tab if you want your information to appear
   there. *)
let key : string = "";;



(* 1.h. Please complete the sectioning survey for the course, available
   at section.cs51.io. When you have completed the survey, set this
   variable to true. *)
let sectioned : bool = false;;


(*======================================================================
1.i. Time estimate: Please give us an honest (if approximate) estimate
of how long (in minutes) this problem set took you to complete,
including both following the setup instructions and filling out this
file, by editing the line:

    let minutes_spent_on_pset () : int = failwith "not provided" ;;

to replace the value of the function with an approximate estimate of
how long (in minutes) the assignment took you to complete. For
example, if you spent 3 hours on this assignment, you should change
the line to:

    let minutes_spent_on_pset () : int = 180 ;;

We care about your responses and will use them to help guide us in
creating future assignments.
......................................................................*)

let minutes_spent () : int = failwith "not provided" ;;


(* You can type the command

      % make all

   to compile this file. (This command invokes "ocamlbuild", which is
   the smart OCaml compiler we use in this course -- more on that
   later!) Then type

      % ./ps0.byte

   to run the program and print the output. Make sure all the values
   look right, and then visit http://pset0.herokuapp.com/responses/ to
   see the messages. If everything looks correct, and you're done
   modifying this file, you shoould commit, push, and submit to
   /gradescope as described in the problem set writeup. Then your
   done!



                         END OF PROBLEM SET 0
======================================================================*)


(* You shouldn't change anything below this line, but feel free to
   read to the bottom of the file and try to figure out what is going
   on. *)

let print = Printf.printf ;;

let convert_survey () =
  let (first, last) = name in
  let string_year =
    match class_year with
    | Freshman -> "2023"
    | Sophomore -> "2022"
    | Junior -> "2021"
    | Senior -> "2020"
    | Other s -> "Other: " ^ s in
  let string_cs50 =
    match took_cs_50 with
    | Took -> "I took CS50"
    | DidNotTake -> "I did not take CS50"
    | Other s -> "Other: " ^ s in
  (first, last, string_year, string_cs50, exciting, key) ;;

let print_survey (first, last, year, cs50, message, _) =
  (print "----------------------------------------\n";
   print "Name: %s %s\n\n" first last;
   print "Year: %s\n\n" year;
   print "50?: %s \n\n" cs50;
   print "%s\n\n" message;
   print "----------------------------------------\n\n";)  ;;

let post_survey s =
  let (first, last, year, cs50, message, key) = s in
  let open Nethttp_client.Convenience in
  let mbody = [("first", first); ("last", last);
               ("year", year); ("cs50", cs50);
               ("message", message); "key", key] in
  let url = "http://pset0.herokuapp.com" in
  let open Yojson.Basic in
  http_post url mbody
  |> fun x -> from_string x
              |> Util.member "status"
              |> fun x -> to_string x |> print "%s\n";
                          print_survey s  ;;

let s = convert_survey () in
    (if post then post_survey else print_survey) s  ;;
