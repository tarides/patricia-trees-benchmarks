open Bechamel
open Toolkit

let benchmark () =
  let instances = Instance.[ monotonic_clock ] in
  let cfg =
    Benchmark.cfg ~limit:2000 ~stabilize:true ~quota:(Time.second 0.5) ()
  in
  let results = Hashtbl.create 32 in
  List.iter
    (fun tests ->
      let tests = Test.elements tests in
      List.iter
        (fun test ->
          try
            Hashtbl.replace results (Test.Elt.name test)
              (Benchmark.run cfg instances test)
          with Bench.Unsupported -> ())
        tests)
    Tests.tests;
  results

let analyze results =
  let ols =
    Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:[| Measure.run |]
  in
  let results = Analyze.all ols Instance.monotonic_clock results in
  Analyze.merge ols [ Instance.monotonic_clock ] [ results ]

let () =
  Bechamel_notty.Unit.add Instance.monotonic_clock
    (Measure.unit Instance.monotonic_clock)

let img (window, results) =
  Bechamel_notty.Multiple.image_of_ols_results ~rect:window
    ~predictor:Measure.run results

open Notty_unix

let () =
  let window =
    match winsize Unix.stdout with
    | Some (w, h) -> { Bechamel_notty.w; h }
    | None -> { Bechamel_notty.w = 80; h = 1 }
  in
  let results = benchmark () in
  let results = analyze results in
  img (window, results) |> eol |> output_image
