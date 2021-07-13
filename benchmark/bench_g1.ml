open Core
open Core_bench

let t1 =
  let open Bls12_381 in
  let s = G1.Scalar.random () in
  let p = G1.random () in
  Bench.Test.create ~name:"Multiplication on G1" (fun () -> ignore (G1.mul p s))

let t2 =
  let open Bls12_381 in
  let p = G1.random () in
  Bench.Test.create ~name:"Double on G1" (fun () -> ignore (G1.double p))

let t3 =
  let open Bls12_381 in
  let p1 = G1.random () in
  let p2 = G1.random () in
  Bench.Test.create ~name:"Addition on G1" (fun () -> ignore (G1.add p1 p2))

let t4 =
  let open Bls12_381 in
  let p = G1.random () in
  Bench.Test.create ~name:"Negate on G1" (fun () -> ignore (G1.negate p))

let t5 =
  let open Bls12_381 in
  let p = G1.random () in
  let p_bytes = G1.to_bytes p in
  Bench.Test.create ~name:"of_bytes_exn on G1" (fun () ->
      ignore (G1.of_bytes_exn p_bytes))

let t6 =
  let open Bls12_381 in
  let p = G1.random () in
  Bench.Test.create ~name:"to_bytes on G1" (fun () -> ignore (G1.to_bytes p))

let t7 =
  let open Bls12_381 in
  let p = G1.random () in
  let p_bytes = G1.to_compressed_bytes p in
  Bench.Test.create ~name:"of_compressed_bytes_exn on G1" (fun () ->
      ignore (G1.of_compressed_bytes_exn p_bytes))

let t8 =
  let open Bls12_381 in
  let p = G1.random () in
  Bench.Test.create ~name:"to_compressed_bytes on G1" (fun () ->
      ignore (G1.to_compressed_bytes p))

let command = Bench.make_command [t1; t2; t3; t4; t5; t6; t7; t8]

let () = Core.Command.run command
