open OUnit2

let input = [
  "@greeting \"Hello\"";
  "@farewell \"Goodbye!\"";
  "";
  ":print_farewell";
  "  load %2 10";
  "  allc %2";
  "  stor %2 @farewell";
  "  puts %2";
  "  halt";
  "";
  ":main";
  "  load %0 1";
  "  load %1 %0";
  "  test %1 10";
  "  load %2 12";
  "  allc %2";
  "  stor %2 @greeting";
  ":loop1";
  "  brch %1 :stop_greeting";
  "  puts %2";
  "  iadd %0 1";
  "  jump :loop1";
  ":stop_greeting";
  "  call :print_farewell 0";
]

let expected_output = String.concat "\n" [
  "Hello";
  "Goodbye!";
  "01000000020000000a";
  "030000000200000000";
  "020000000200000002";
  "0e0000000200000000";
  "000000000000000000";
  "010000000000000001";
  "010000000100000000";
  "09000000010000000a";
  "01000000020000000c";
  "030000000200000000";
  "020000000200000001";
  "0a0000000100000012";
  "0e0000000200000000";
  "040000000000000001";
  "080000000e00000000";
  "0b0000000300000000";
]

let suite =
  "Assembler suite">::: [
    "assembling a valid file">::
      (fun _ ->
        let actual_output = Pasm.Assembler.assemble input in
        match actual_output with
        | Ok actual_output -> begin
            let string_output = Pasm.Output.to_string actual_output in
            assert_equal string_output expected_output
        end
        | Error _ -> assert_failure "error during assembly of input"
      );
  ]
