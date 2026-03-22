module E = Util.Env
module SO = Semobj

let stdlib () = E.init_env () |> E.insert "nil" (SO.List { vs = [] })
