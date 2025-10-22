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

let benchmark () =
  let cfg =
    Benchmark.cfg ~limit:2000 ~stabilize:true ~quota:(Time.second 0.5) ()
  in
  List.map
    (fun (name, tests) ->
      (name, benchmark_all_with_unsupported cfg instances tests))
    (Bench.merge Tests.tests)

let analyze results =
  let ols =
    Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:[| Measure.run |]
  in
  Analyze.merge ols instances
    (List.map (fun instance -> Analyze.all ols instance results) instances)

let () =
  List.iter
    (fun instance -> Bechamel_notty.Unit.add instance (Measure.unit instance))
    instances

let img (window, results) =
  Bechamel_notty.Multiple.image_of_ols_results ~rect:window
    ~predictor:Measure.run results

open Notty_unix

let () =
  let open Notty in
  let open Notty.Infix in
  let window =
    match winsize Unix.stdout with
    | Some (w, h) -> { Bechamel_notty.w; h }
    | None -> { Bechamel_notty.w = 80; h = 1 }
  in
  let img =
    List.map
      (fun (name, (results, unsupported)) ->
        let unsupported =
          if unsupported = [] then I.empty
          else
            I.hcat
              (List.map (I.string A.empty) ("Unsupported by: " :: unsupported))
        in
        I.string A.empty name <-> img (window, analyze results) <-> unsupported)
      (benchmark ())
    |> I.vcat
  in
  output_image img
