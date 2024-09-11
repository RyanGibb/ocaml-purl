let uriEncode = Uri.pct_encode ~component:`Query;;

type purl = {
  purl_type : string;
  namespace : string option;
  name : string;
  version : string option;
  qualifiers : (string * string) list;
  subpath : string option;
}

let encode_purl { purl_type; namespace; name; version; qualifiers; subpath } =
  let qualifier_string = qualifiers
    |> List.map (fun (k,v) -> k ^ "=" ^ uriEncode v)
    |> String.concat "&"
  in
  "pkg:" ^ purl_type
  ^ (match namespace with Some v -> "/" ^ Uri.pct_encode ~component:`Host v | None -> "")
  ^ "/" ^ uriEncode name
  ^ (match version with Some v -> "@" ^ uriEncode v | None -> "")
  ^ (if qualifier_string <> "" then "?" ^ qualifier_string else "")
  ^ (match subpath with Some s -> "#" ^ uriEncode s | None -> "")

let trim_chars ~chars s =
  let rec trim_left s =
    match s with
    | "" -> ""
    | _ when String.contains chars s.[0] -> trim_left (String.sub s 1 (String.length s - 1))
    | _ -> s
  in
  let rec trim_right s =
    match s with
    | "" -> ""
    | _ when String.contains chars s.[String.length s - 1] -> trim_right (String.sub s 0 (String.length s - 1))
    | _ -> s
  in
  trim_right (trim_left s)

let decode_purl s =
  let (let*) = Option.bind in
  let* parsed = try Some (Uri.of_string s) with _ -> None in
  let* _ = match Uri.scheme parsed with
    | Some "pkg" -> Some "pkg"
    | _ -> None
  in
  let qualifiers =
    match Uri.query parsed with
    | [] -> []
    | qs -> List.map (fun (k, v) -> String.lowercase_ascii k, (match v with x::_ -> x | [] -> "")) qs
  in
  let path = (Uri.host parsed |> Option.value ~default:"") ^ Uri.path parsed in
  let path_parts = String.split_on_char '/' path |> List.filter (fun x -> x <> "") in
  let* purl_type, rest =
    match path_parts with
    | t::rest -> Some (String.lowercase_ascii t, rest)
    | _ -> None
  in
  let* namespace, name, version =
    let split_name_version string =
      match String.index_opt string '@' with
      | None -> Some (string, None)
      | Some 0 -> None
      | Some i ->
        let n = String.sub string 0 i in
        let v = Some (String.sub string (i + 1) (String.length string - i - 1)) in
        Some (n, v)
    in
    match rest with
    | [] -> None
    | l ->
        let* n, v = List.rev l |> List.hd |> split_name_version in
        let ns = List.rev l |> List.tl |> List.rev |> function | [] -> None | l -> Some l |> Option.map (String.concat "/") in
        Some (ns, n, v)
  in
  let subpath = 
    match Uri.fragment parsed with
    | None -> None
    | Some fragment ->
        (* Remove leading and trailing slashes from fragment *)
        let trimmed = trim_chars ~chars:"/" fragment in
        Some trimmed
  in
  Some {
    purl_type = purl_type;
    namespace =
      (match purl_type with
      | "bitbucket" | "github" -> Option.map String.lowercase_ascii namespace
      | "pypi" -> Option.map (fun name -> String.lowercase_ascii name |> Str.global_replace (Str.regexp_string "_") "-") namespace
      | _ -> namespace);
    name =
      (match purl_type with
      | "bitbucket" | "github" -> String.lowercase_ascii name
      | "pypi" -> String.lowercase_ascii name |> Str.global_replace (Str.regexp_string "_") "-"
      | _ -> name);
    version = Option.map Uri.pct_decode version;
    qualifiers = qualifiers;
    subpath = subpath;
  }


open Yojson.Basic.Util

let compare_purl expected parsed =
  let check_field field expected_value parsed_value =
    if String.compare expected_value parsed_value != 0 then
      Printf.printf "  %s: Mismatch (expected: '%s', got: '%s')\n" field expected_value parsed_value
  in

  let expected_type = expected |> member "type" |> to_string in
  let expected_namespace = expected |> member "namespace" |> to_string_option in
  let expected_name = expected |> member "name" |> to_string in
  let expected_version = expected |> member "version" |> to_string_option in
  let expected_qualifiers =
    expected |> member "qualifiers"
    |> to_option to_assoc
    |> Option.value ~default:[]
    |> List.map (fun (k, v) -> k, to_string v)
  in
  let expected_subpath = expected |> member "subpath" |> to_string_option in

  check_field "Type" expected_type parsed.purl_type;
  check_field "Namespace"
    (Option.value ~default:"<none>" expected_namespace)
    (Option.value ~default:"<none>" parsed.namespace);
  check_field "Name" expected_name parsed.name;
  check_field "Version"
    (Option.value ~default:"<none>" expected_version)
    (Option.value ~default:"<none>" parsed.version);
  let qualifiers_str = List.map (fun (k, v) -> k ^ "=" ^ v) parsed.qualifiers |> String.concat ", " in
  let expected_qualifiers_str = List.map (fun (k, v) -> k ^ "=" ^ v) expected_qualifiers |> String.concat ", " in
  check_field "Qualifiers" expected_qualifiers_str qualifiers_str;
  check_field "Subpath"
    (Option.value ~default:"<none>" expected_subpath)
    (Option.value ~default:"<none>" parsed.subpath)

let test_purl () =
  Yojson.Basic.from_file "test-suite-data.json"
  |> to_list
  |> List.iter (fun test ->
    let description = test |> member "description" |> to_string in
    let purl_string = test |> member "purl" |> to_string in
    let expected_is_invalid = test |> member "is_invalid" |> to_bool in

    Printf.printf "Test: %s\n" description;
    let parsed = decode_purl purl_string in
    match parsed with
    | None ->
        if not expected_is_invalid then
          print_endline ("  Error: Failed to parse a valid purl: " ^ purl_string)
    | Some parsed ->
        if expected_is_invalid then
          print_endline ("  Error: Incorrectly parsed an invalid purl: " ^ purl_string)
        else
          let canonical = encode_purl parsed in
          let expected_canonical = test |> member "canonical_purl" |> to_string in
          if String.compare canonical expected_canonical != 0 then
            Printf.printf "  Error: Mismatch (expected: %s, got: %s)\n" expected_canonical canonical;
          compare_purl test parsed
  )

let () = 
  Printexc.record_backtrace true;
  test_purl ()
