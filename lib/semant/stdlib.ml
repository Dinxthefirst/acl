module Tast = Asts.Tast
module TE = Util.Env

let stdlib () =
  TE.init_env ()
  |> TE.insert
       "nil"
       (Tast.TPoly { var = "t"; tps = Tau (Tast.TList { tp = TyVar "t" }) })
  |> TE.insert
       "cons"
       (Tast.TPoly
          { var = "t"
          ; tps =
              Tau
                (Tast.TFunc
                   { intp = TyVar "t"
                   ; outtp =
                       Tast.TFunc
                         { intp = Tast.TList { tp = TyVar "t" }
                         ; outtp = Tast.TList { tp = TyVar "t" }
                         }
                   })
          })
  |> TE.insert
       "rev"
       (Tast.TPoly
          { var = "t"
          ; tps =
              Tau
                (Tast.TFunc
                   { intp = Tast.TList { tp = TyVar "t" }
                   ; outtp = Tast.TList { tp = TyVar "t" }
                   })
          })
  |> TE.insert
       "hd"
       (Tast.TPoly
          { var = "t"
          ; tps =
              Tau
                (Tast.TFunc
                   { intp = Tast.TList { tp = TyVar "t" }
                   ; outtp = TyVar "t"
                   })
          })
  |> TE.insert
       "tl"
       (Tast.TPoly
          { var = "t"
          ; tps =
              Tau
                (Tast.TFunc
                   { intp = Tast.TList { tp = TyVar "t" }
                   ; outtp = Tast.TList { tp = TyVar "t" }
                   })
          })
;;
