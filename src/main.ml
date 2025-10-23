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

let analyze =
  List.map (fun (name, (r, unsupported)) -> (name, r, analyze r, unsupported))

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
      (fun (name, _, analyzed, unsupported) ->
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
  output_image img

let output_html results =
  let open Bechamel_js in
  let ok_or_fail = function Ok () -> () | Error (`Msg msg) -> failwith msg in
  let nothing _ = Ok () in
  let js_data =
    let buf = Buffer.create 2048 in
    List.map
      (fun (name, raw, analyzed, _unsupported) ->
        Buffer.clear buf;
        ok_or_fail
        @@ emit ~dst:(Buffer buf) nothing ~x_label:Measure.run
             ~y_label:(Measure.label Instance.monotonic_clock)
             (analyzed, raw);
        (name, Buffer.contents buf))
      results
  in
  let out_fname = "results.html" in
  let _, data = List.nth js_data 0 in
  let outp =
    Unix.open_process_out
      (Filename.quote_command "bechamel-html" ~stdout:out_fname [])
  in
  Printf.fprintf outp "%s\n" data;
  Printf.printf "\nHTML output available at %s\n" out_fname

let () =
  let results = benchmark () in
  let analyzed = analyze results in
  output_notty analyzed;
  output_html analyzed
