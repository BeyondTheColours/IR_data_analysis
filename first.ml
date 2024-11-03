type data_point =
  |Point of float*float
;;

let rec d_dx ds =
  match ds with
  |[] -> None
  |Point(x, y)::[] -> None
  |Point(x1, y1)::Point(x2, y2)::t ->
      match d_dx (Point(x2, y2)::t) with
      |None -> Some([Point(x2, (y2-.y1)/.(x2-.x1))])
      |Some(v) -> Some(Point(x2, (y2-.y1)/.(x2-.x1))::v)
;;

let rec write_to_csv ds x_name y_name =
  match ds with
  |[] -> x_name^","^y_name^"\n"
  |Point(x, y)::t ->
      let x' = string_of_float x in
      let y' = string_of_float y in
      x'^","^y'^"\n"^(write_to_csv t x_name y_name)
;;

let gen_data lower_bound upper_bound incr f =
  let rec aux lower_bound upper_bound incr f res =
    if lower_bound >= upper_bound then res
    else aux (lower_bound+.incr) upper_bound incr f (Point(lower_bound, f lower_bound)::res)
  in aux lower_bound upper_bound incr f []
;;
