let () =
  let env = Flow_env.mk_new () in
  let files = List.tl (Array.to_list Sys.argv) in
  files |> List.iter (fun file ->
    let cmi = Cmi_format.read_cmi file in
    let sigs = cmi.Cmi_format.cmi_sign in
    let module_name =
      file
        |> Filename.basename
        |> Filename.chop_extension
        |> String.capitalize
    in
    Flow_env.enter_module env module_name;
    let code = Flow.print_signature env sigs in
    Flow_env.leave_module env;
    let oc = open_out ((Filename.chop_extension file) ^ ".js.flow") in
    Printf.fprintf oc "%s" code;
    close_out oc;
  )
