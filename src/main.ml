open Bechamel
open Toolkit

(** Like [Benchmark.all] but handles unsupported operations. *)
let benchmark_all_with_unsupported cfg instances tests =
  let results = Hashtbl.create 32 in
  let tests = Test.elements tests in
  let unsupported = ref [] in
  List.iter
    (fun test ->
      let name = Test.Elt.name test in
      try Hashtbl.replace results name (Benchmark.run cfg instances test)
      with Bench.Unsupported -> unsupported := name :: !unsupported)
    tests;
  (results, !unsupported)

let instances = Instance.[ promoted; minor_allocated; monotonic_clock ]
let tests = Bench.merge Tests.tests

let test_names =
  List.sort_uniq String.compare
    (List.concat_map (fun (_, test) -> Test.names test) tests)

let benchmark () =
  let cfg =
    Benchmark.cfg ~limit:2000 ~stabilize:true ~quota:(Time.second 0.5) ()
  in
  List.map
    (fun (name, tests) ->
      (name, benchmark_all_with_unsupported cfg instances tests))
    tests

let analyze results =
  let ols =
    Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:[| Measure.run |]
  in
  Analyze.merge ols instances
    (List.map (fun instance -> Analyze.all ols instance results) instances)

let analyze =
  List.map (fun (name, (r, unsupported)) -> (name, analyze r, unsupported))

let unit_of_label label =
  Option.value ~default:label
    (List.find_map
       (fun instance ->
         if Measure.label instance = label then Some (Measure.unit instance)
         else None)
       instances)

(** Render the result (in ns/run) into µs/run *)
let test_result_to_str ols =
  let module O = Analyze.OLS in
  let rsq = Option.value ~default:0. (O.r_square ols) in
  let predictor = List.find_index (( = ) Measure.run) (O.predictors ols) in
  match (O.estimates ols, predictor) with
  | Some estimates, Some i ->
      let est = List.nth estimates i in
      let rsq_percent = max 0. (rsq *. 100.) in
      let precision = if est < 100. then 2 else if est < 1000. then 1 else 0 in
      let unit_r = unit_of_label (O.responder ols) in
      Format.asprintf "%.*f %s/run (r²=%.0f%%)" precision est unit_r rsq_percent
  | None, _ -> "unsupported"
  | Some _, None -> "?"

let output_csv results =
  let header_row = "" :: test_names in
  List.concat_map
    (fun instance ->
      let instance_label = Measure.label instance in
      [ "# " ^ instance_label ]
      :: header_row
      :: List.map
           (fun (name, analyzed, _unsupported) ->
             let r = Hashtbl.find analyzed instance_label in
             name
             :: List.map
                  (fun tname ->
                    Option.fold ~none:"?" ~some:test_result_to_str
                      (Hashtbl.find_opt r tname))
                  test_names)
           results)
    instances

let () =
  List.iter
    (fun instance -> Bechamel_notty.Unit.add instance (Measure.unit instance))
    instances

let img (window, results) =
  Bechamel_notty.Multiple.image_of_ols_results ~rect:window
    ~predictor:Measure.run results

let output_notty results =
  let open Notty_unix in
  let open Notty in
  let open Notty.Infix in
  let window =
    match winsize Unix.stdout with
    | Some (w, h) -> { Bechamel_notty.w; h }
    | None -> { Bechamel_notty.w = 80; h = 1 }
  in
  let img =
    List.map
      (fun (name, analyzed, unsupported) ->
        let unsupported =
          if unsupported = [] then I.empty
          else
            I.hcat
              (List.map (I.string A.empty) ("Unsupported by: " :: unsupported))
        in
        I.string A.empty name <-> img (window, analyzed) <-> unsupported)
      results
    |> I.vcat
  in
  output_image img;
  Format.printf "@\n"

let () =
  let results = benchmark () in
  List.iter
    (fun (name, (tests, _)) ->
      Hashtbl.iter (fun tname _r -> Format.printf "%s %s@\n" name tname) tests)
    results;
  let analyzed = analyze results in
  output_notty analyzed;
  let outf = "results.csv" in
  Csv.save outf (output_csv analyzed);
  Format.printf "CSV output available in %s@\n" outf
