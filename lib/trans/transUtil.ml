include Util

 (* Just to avoid name collisions such as Id and Subst.Id *)
module ModuleWrapper = struct
  module Id      = Id
  module Type    = Type
  module Arith   = Arith
  module Formula = Formula
  module Hflz    = Hflz
end

let log_src = Logs.Src.create ~doc:"Transform" "Trans"
module Log = (val Logs.src_log log_src)
