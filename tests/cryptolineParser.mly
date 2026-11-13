%{

(*
 * Use raise_at to raise an exception if the line number can be determined.
 * Raise ParseError otherwise.
 *)


  open Ast.Cryptoline
  open Typecheck.Std
  open Common

  exception ParseError of string

  let main = "main"
  let vars_expansion_infix = "_"

  type func =
    { fname : string;
      fargs : var list;
      fouts : var list;
      fvm : Ast.Cryptoline.var SM.t; (* a map from a name to a variable (including carry variables) *)
      fym : Ast.Cryptoline.var SM.t; (* a map from a name to a carry variable *)
      fgm : Ast.Cryptoline.var SM.t; (* a map from a name to a ghost variable *)
      fbody : lined_program;
      fpre : bexp;
      fpost : bexp;
      fepwss : prove_with_spec list;
      frpwss : prove_with_spec list }

  let (--) i j =
    let rec aux n acc =
      if n < i then acc else aux (n-1) (n :: acc)
    in aux j []

  let raise_at lno msg = raise (ParseError ("Parse failure at line " ^ string_of_int lno ^ ". " ^ msg))

  let vm_of_list (vs : var list) = List.fold_left (fun m v -> SM.add v.vname v m) SM.empty vs
  let vm_of_vs (vs : VS.t) = vm_of_list (VS.elements vs)
  let vs_of_vm vm = SM.fold (fun _ v vs -> VS.add v vs) vm VS.empty

  let _some_pass reasons =
    let rec helper r reasons =
      match reasons with
      | [] -> r
      | None::_ -> None
      | hd::tl -> helper hd tl in
    helper None reasons

  let to_uint ty = Tuint (size_of_typ ty)
  let to_double_size ty =
    match ty with
    | Tuint w -> Tuint (w * 2)
    | Tsint w -> Tsint (w * 2)

  (*
  let check_at lno reasons =
    match chain_reasons reasons with
    | None -> ()
    | Some r -> raise_at lno r
   *)

  (*
  type lv_token_t = Z.t SM.t -> var SM.t -> var SM.t -> var SM.t -> typ option -> (var SM.t * var SM.t * var SM.t * var)
   *)
(*   type atomic_token_t = Z.t SM.t -> var SM.t -> var SM.t -> var SM.t -> atomic *)

  type lv_prim_t = {
    lvtyphint: typ option;
    lvname: string;
  }

  type avar_prim_t = {
    atmtyphint: typ option;
    atmname: string;
  }

  type aconst_prim_t = {
    atmtyp: typ;
    (* FIXME *)
    atmvalue: Z.t SM.t -> Z.t;
  }

  (*
  type lv_resolved_t = {
    lvcxt: var SM.t * var SM.t * var SM.t;
    lvvar: var;
  }
  *)

  (*
  type lv_token_t = [
    | `LVPLAIN of lv_prim_t
    | `LVCARRY of lv_prim_t
    | `LV of lv_prim_t
  ]
   *)

  type atomic_t = [
    | `AVAR of avar_prim_t
    | `ACONST of aconst_prim_t
  ]

  let num_two = Z.of_int 2

  let num_of_bit b =
    if b = '0'
    then Z.zero
    else if b = '1'
         then Z.one
         else failwith ("Unrecognized bit " ^ Char.escaped b)

  let num_of_bits bits =
    let rec helper res bits =
      match bits with
      | [] -> res
      | hd::tl -> helper (Z.add (Z.mul res num_two) (num_of_bit hd)) tl in
    helper Z.zero bits

  let parse_typed_const lno ty n_token =
    fun cm _vm _ym _gm ->
    let n = n_token cm in
    let size = size_of_typ ty in
    (* Normalize the number: convert to non-negative integer *)
    let n =
      match ty with
      | Tuint _ -> if Z.lt n Z.zero
                   then raise_at lno ("The number " ^ Z.to_string n ^ " is expected to be non-negative")
                   else n
      | Tsint w -> if Z.lt n Z.zero
                   then let n = Z.add n (Z.pow num_two w) in
                        if Z.lt n Z.zero
                        then raise_at lno ("The number " ^ Z.to_string n ^ " does not fit into " ^ string_of_typ ty)
                        else n
                   else n in
    (* Normalize the number: convert to bit vector *)
    let bits = List.of_seq (String.to_seq (Z.format ("%0" ^ string_of_int size ^ "b") n)) in
    let _ = if List.length bits > size then raise_at lno ("The number " ^ Z.to_string n ^ " does not fit into " ^ string_of_typ ty) in
    (* Normalize the number: convert back to integer *)
    let n =
      match ty with
      | Tuint _ -> num_of_bits bits
      | Tsint w ->
         if w = 0 then Z.zero
         else
           begin
             match bits with
             | [] -> Z.zero
             | sign::rest -> let n = num_of_bits rest in
                             if sign = '1' then Z.sub n (Z.pow num_two (w - 1))
                             else n
           end
    in
    Aconst (ty, n)

  let resolve_var_with lno (`AVAR {atmtyphint; atmname}) _cm vm _ym gm =
    let v =
      try
        SM.find atmname vm
      with Not_found ->
        begin
          try
            SM.find atmname gm
          with Not_found ->
            raise_at lno ("Variable " ^ atmname ^ " is undefined.")
        end in
    let _ = match atmtyphint with
    | None -> ()
    | Some hinted_ty ->
        if v.vtyp <> hinted_ty then
          raise_at lno ("The type of variable " ^ atmname ^ " is inconsistent")
        else () in
    v

  let resolve_atomic_with lno (a: atomic_t) cm vm ym gm =
    match a with
    (* FIXME *)
    | `ACONST c -> parse_typed_const lno c.atmtyp c.atmvalue cm vm ym gm
    | `AVAR v -> Avar (resolve_var_with lno (`AVAR v) cm vm ym gm)

  let resolve_lv_with lno {lvname; lvtyphint} _cm vm ym gm ty_opt =
    if SM.mem lvname gm then
      raise_at lno ("The program variable " ^ lvname ^
                    " has been defined as a ghost variable.")
    else
      let ty = (match (ty_opt, lvtyphint) with
      | (None, None) ->
         raise_at lno ("Failed to determine the type of " ^ lvname)
      | (None, Some hinted_ty) -> hinted_ty
      | (Some determined_ty, None) -> determined_ty
      | (Some determined_ty, Some hinted_ty) ->
        if determined_ty <> hinted_ty then
          raise_at lno (Printf.sprintf "The specified type %s is not equal to the determined type %s."
                          (string_of_typ hinted_ty)
                          (string_of_typ determined_ty))
        else determined_ty) in
      let v = mkvar lvname ty in
      (* It is possible that the lval is actually a bit variable *)
      (if var_is_bit v then
        (SM.add lvname v vm, SM.add lvname v ym, gm, v)
      else
        (SM.add lvname v vm, SM.remove lvname ym, gm, v))

  let resolve_lcarry_with lno {lvname; lvtyphint} _cm vm ym gm =
    if SM.mem lvname gm then
      raise_at lno ("The carry variable " ^ lvname ^ " has been defined as a ghost variable.")
    else
      let _ = (match lvtyphint with
      | None -> ()
      | Some hinted_ty ->
        if hinted_ty <> bit_t then
          raise_at lno ("The type of a carry variable should be \""
                        ^ string_of_typ bit_t
                        ^ "\", specified "
                        ^ string_of_typ hinted_ty)
        else ()) in
      let v = mkvar lvname bit_t in
      (SM.add lvname v vm, SM.add lvname v ym, gm, v)

  let resolve_lv_or_lcarry_with lno {lvname; lvtyphint} _cm vm ym gm =
    if SM.mem lvname gm then
      raise_at lno ("The program variable " ^ lvname ^
                    " has been defined as a ghost variable.")
    else
      match lvtyphint with
      | None -> raise_at lno ("Failed to determine the type of " ^ lvname)
      | Some ty ->
         let v = mkvar lvname ty in
         if var_is_bit v then (SM.add lvname v vm, SM.add lvname v ym, gm, v)
         else (SM.add lvname v vm, SM.remove lvname ym, gm, v)

  let parse_imov_at lno dest src =
    fun _fm cm vm ym gm ->
      let a = resolve_atomic_with lno src cm vm ym gm in
      let ty = typ_of_atomic a in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Imov (v, a)])

  let parse_ishl_at lno dest src num =
    fun _fm cm vm ym gm ->
      let a = resolve_atomic_with lno src cm vm ym gm in
      let ty = typ_of_atomic a in
      let n = num cm in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      let _ =
        let w = size_of_var v in
        if Z.leq n Z.zero || Z.geq n (Z.of_int w) then
          raise_at lno ("An shl instruction expects an offset between 0 and the " ^ string_of_int w ^ " (both excluding)."
                        ^ " An offset not in the range is found: " ^ Z.to_string n ^ ".")
      in
      (vm, ym, gm, [lno, Ishl (v, a, n)])

  let parse_cshl_at lno destH destL src1 src2 num =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let n = num cm in
      let (vm, ym, gm, vh) =
        resolve_lv_with lno destH cm vm ym gm (Some ty) in
      let (vm, ym, gm, vl) =
        resolve_lv_with lno destL cm vm ym gm (Some (to_uint ty)) in
      (vm, ym, gm, [lno, Icshl (vh, vl, a1, a2, n)])

  let parse_set_at lno dest =
    fun _fm cm vm ym gm ->
      let (vm, ym, gm, c) = resolve_lcarry_with lno dest cm vm ym gm in
      (vm, ym, gm, [lno, Imov (c, Aconst (bit_t, Z.one)) ])

  let parse_clear_at lno dest =
    fun _fm cm vm ym gm ->
      let (vm, ym, gm, c) = resolve_lcarry_with lno dest cm vm ym gm in
      (vm, ym, gm, [lno, Imov (c, Aconst (bit_t, Z.zero))])

  let parse_nondet_at lno dest =
    fun _fm cm vm ym gm ->
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm None in
      (vm, ym, gm, [lno, Inondet v])

  let parse_cmov_at lno dest carry src1 src2 =
    fun _fm cm vm ym gm ->
      let c = resolve_atomic_with lno carry cm vm ym gm in
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Icmov (v, c, a1, a2)])

  let parse_add_at lno dest src1 src2 =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Iadd (v, a1, a2)])

  let parse_adds_at lno flag dest src1 src2 =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, c) = resolve_lcarry_with lno flag cm vm ym gm in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Iadds (c, v, a1, a2)])

  let parse_addr_at lno flag dest src1 src2 =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, c) = resolve_lcarry_with lno flag cm vm ym gm in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Iaddr (c, v, a1, a2)])

  let parse_adc_at lno dest src1 src2 (carry : atomic_t) =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let y = resolve_atomic_with lno carry cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Iadc (v, a1, a2, y)])

  let parse_adcs_at lno flag dest src1 src2 carry =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let y = resolve_atomic_with lno carry cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, c) = resolve_lcarry_with lno flag cm vm ym gm in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Iadcs (c, v, a1, a2, y)])

  let parse_adcr_at lno flag dest src1 src2 carry =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let y = resolve_atomic_with lno carry cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, c) = resolve_lcarry_with lno flag cm vm ym gm in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Iadcr (c, v, a1, a2, y)])

  let parse_sub_at lno dest src1 src2 =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Isub (v, a1, a2)])

  let parse_subc_at lno flag dest src1 src2 =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, c) = resolve_lcarry_with lno flag cm vm ym gm in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Isubc (c, v, a1, a2)])

  let parse_subb_at lno flag dest src1 src2 =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, c) = resolve_lcarry_with lno flag cm vm ym gm in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Isubb (c, v, a1, a2)])

  let parse_subr_at lno flag dest src1 src2 =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, c) = resolve_lcarry_with lno flag cm vm ym gm in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Isubr (c, v, a1, a2)])

  let parse_sbc_at lno dest src1 src2 carry =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let y = resolve_atomic_with lno carry cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Isbc (v, a1, a2, y)])

  let parse_sbcs_at lno flag dest src1 src2 carry =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let y = resolve_atomic_with lno carry cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, c) = resolve_lcarry_with lno flag cm vm ym gm in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Isbcs (c, v, a1, a2, y)])

  let parse_sbcr_at lno flag dest src1 src2 carry =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let y = resolve_atomic_with lno carry cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, c) = resolve_lcarry_with lno flag cm vm ym gm in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Isbcr (c, v, a1, a2, y)])

  let parse_sbb_at lno dest src1 src2 carry =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let y = resolve_atomic_with lno carry cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Isbb (v, a1, a2, y)])

  let parse_sbbs_at lno flag dest src1 src2 carry =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let y = resolve_atomic_with lno carry cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, c) = resolve_lcarry_with lno flag cm vm ym gm in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Isbbs (c, v, a1, a2, y)])

  let parse_sbbr_at lno flag dest src1 src2 carry =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let y = resolve_atomic_with lno carry cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, c) = resolve_lcarry_with lno flag cm vm ym gm in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Isbbr (c, v, a1, a2, y)])

  let parse_mul_at lno dest src1 src2 =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Imul (v, a1, a2)])

  let parse_muls_at lno flag dest src1 src2 =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, c) = resolve_lcarry_with lno flag cm vm ym gm in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Imuls (c, v, a1, a2)])

  let parse_mulr_at lno flag dest src1 src2 =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, c) = resolve_lcarry_with lno flag cm vm ym gm in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Imulr (c, v, a1, a2)])

  let parse_mull_at lno destH destL src1 src2 =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, vh) = resolve_lv_with lno destH cm vm ym gm (Some ty) in
      let (vm, ym, gm, vl) =
        resolve_lv_with lno destL cm vm ym gm (Some (to_uint ty)) in
      (vm, ym, gm, [lno, Imull (vh, vl, a1, a2)])

  let parse_mulj_at lno dest src1 src2 =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, v) =
        resolve_lv_with lno dest cm vm ym gm (Some (to_double_size ty)) in
      (vm, ym, gm, [lno, Imulj (v, a1, a2)])

  let parse_split_at lno destH destL src num =
    fun _fm cm vm ym gm ->
      let a = resolve_atomic_with lno src cm vm ym gm in
      let n = num cm in
      let ty = typ_of_atomic a in
      let (vm, ym, gm, vh) = resolve_lv_with lno destH cm vm ym gm (Some ty) in
      let (vm, ym, gm, vl) =
        resolve_lv_with lno destL cm vm ym gm (Some (to_uint ty)) in
      let _ =
        let w = size_of_var vl in
        if Z.leq n Z.zero || Z.geq n (Z.of_int w) then
          raise_at lno ("The position of a split should be in between 0 and " ^ string_of_int w ^ " (both excluded)")
      in
      (vm, ym, gm, [lno, Isplit (vh, vl, a, n)])

  let parse_uadd_at lno dest src1 src2 =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Iadd (v, a1, a2)])

  let parse_uadds_at lno flag dest src1 src2 =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, c) = resolve_lcarry_with lno flag cm vm ym gm in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Iadds (c, v, a1, a2)])

  let parse_uaddr_at lno flag dest src1 src2 =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, c) = resolve_lcarry_with lno flag cm vm ym gm in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Iaddr (c, v, a1, a2)])

  let parse_uadc_at lno dest src1 src2 carry =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let y = resolve_atomic_with lno carry cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Iadc (v, a1, a2, y)])

  let parse_uadcs_at lno flag dest src1 src2 carry =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let y = resolve_atomic_with lno carry cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, c) = resolve_lcarry_with lno flag cm vm ym gm in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Iadcs (c, v, a1, a2, y)])

  let parse_uadcr_at lno flag dest src1 src2 carry =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let y = resolve_atomic_with lno carry cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, c) = resolve_lcarry_with lno flag cm vm ym gm in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Iadcr (c, v, a1, a2, y)])

  let parse_usub_at lno dest src1 src2 =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Isub (v, a1, a2)])

  let parse_usubc_at lno flag dest src1 src2 =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, c) = resolve_lcarry_with lno flag cm vm ym gm in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Isubc (c, v, a1, a2)])

  let parse_usubb_at lno flag dest src1 src2 =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, c) = resolve_lcarry_with lno flag cm vm ym gm in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Isubb (c, v, a1, a2)])

  let parse_usubr_at lno flag dest src1 src2 =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, c) = resolve_lcarry_with lno flag cm vm ym gm in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Isubr (c, v, a1, a2)])

  let parse_usbc_at lno dest src1 src2 carry =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let y = resolve_atomic_with lno carry cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Isbc (v, a1, a2, y)])

  let parse_usbcs_at lno flag dest src1 src2 carry =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let y = resolve_atomic_with lno carry cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, c) = resolve_lcarry_with lno flag cm vm ym gm in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Isbcs (c, v, a1, a2, y)])

  let parse_usbcr_at lno flag dest src1 src2 carry =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let y = resolve_atomic_with lno carry cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, c) = resolve_lcarry_with lno flag cm vm ym gm in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Isbcr (c, v, a1, a2, y)])

  let parse_usbb_at lno dest src1 src2 carry =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let y = resolve_atomic_with lno carry cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Isbb (v, a1, a2, y)])

  let parse_usbbs_at lno flag dest src1 src2 carry =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let y = resolve_atomic_with lno carry cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, c) = resolve_lcarry_with lno flag cm vm ym gm in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Isbbs (c, v, a1, a2, y)])

  let parse_usbbr_at lno flag dest src1 src2 carry =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let y = resolve_atomic_with lno carry cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, c) = resolve_lcarry_with lno flag cm vm ym gm in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Isbbr (c, v, a1, a2, y)])

  let parse_umul_at lno dest src1 src2 =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Imul (v, a1, a2)])

  let parse_umuls_at lno flag dest src1 src2 =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, c) = resolve_lcarry_with lno flag cm vm ym gm in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Imuls (c, v, a1, a2)])

  let parse_umulr_at lno flag dest src1 src2 =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, c) = resolve_lcarry_with lno flag cm vm ym gm in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Imulr (c, v, a1, a2)])

  let parse_umull_at lno destH destL src1 src2 =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, vh) = resolve_lv_with lno destH cm vm ym gm (Some ty) in
      let (vm, ym, gm, vl) = resolve_lv_with lno destL cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Imull (vh, vl, a1, a2)])

  let parse_umulj_at lno dest src1 src2 =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, v) =
        resolve_lv_with lno dest cm vm ym gm (Some (to_double_size ty)) in
      (vm, ym, gm, [lno, Imulj (v, a1, a2)])

  let parse_usplit_at lno destH destL src num =
    fun _fm cm vm ym gm ->
      let a = resolve_atomic_with lno src cm vm ym gm in
      let n = num cm in
      let ty = typ_of_atomic a in
      let (vm, ym, gm, vh) = resolve_lv_with lno destH cm vm ym gm (Some ty) in
      let (vm, ym, gm, vl) = resolve_lv_with lno destL cm vm ym gm (Some ty) in
      let _ =
        let w = size_of_var vl in
        if Z.leq n Z.zero || Z.geq n (Z.of_int w) then
          raise_at lno ("The position of a split should be in between 0 and " ^ string_of_int w ^ " (both excluded)")
      in
      (vm, ym, gm, [lno, Isplit (vh, vl, a, n)])

  let parse_sadd_at lno dest src1 src2 =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Iadd (v, a1, a2)])

  let parse_sadds_at lno flag dest src1 src2 =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, c) = resolve_lcarry_with lno flag cm vm ym gm in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Iadds (c, v, a1, a2)])

  let parse_saddr_at lno flag dest src1 src2 =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, c) = resolve_lcarry_with lno flag cm vm ym gm in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Iaddr (c, v, a1, a2)])

  let parse_sadc_at lno dest src1 src2 carry =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let y = resolve_atomic_with lno carry cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Iadc (v, a1, a2, y)])

  let parse_sadcs_at lno flag dest src1 src2 carry =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let y = resolve_atomic_with lno carry cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, c) = resolve_lcarry_with lno flag cm vm ym gm in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Iadcs (c, v, a1, a2, y)])

  let parse_sadcr_at lno flag dest src1 src2 carry =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let y = resolve_atomic_with lno carry cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, c) = resolve_lcarry_with lno flag cm vm ym gm in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Iadcr (c, v, a1, a2, y)])

  let parse_ssub_at lno dest src1 src2 =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Isub (v, a1, a2)])

  let parse_ssubc_at lno flag dest src1 src2 =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, c) = resolve_lcarry_with lno flag cm vm ym gm in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Isubc (c, v, a1, a2)])

  let parse_ssubb_at lno flag dest src1 src2 =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, c) = resolve_lcarry_with lno flag cm vm ym gm in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Isubb (c, v, a1, a2)])

  let parse_ssubr_at lno flag dest src1 src2 =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, c) = resolve_lcarry_with lno flag cm vm ym gm in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Isubr (c, v, a1, a2)])

  let parse_ssbc_at lno dest src1 src2 carry =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let y = resolve_atomic_with lno carry cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Isbc (v, a1, a2, y)])

  let parse_ssbcs_at lno flag dest src1 src2 carry =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let y = resolve_atomic_with lno carry cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, c) = resolve_lcarry_with lno flag cm vm ym gm in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Isbcs (c, v, a1, a2, y)])

  let parse_ssbcr_at lno flag dest src1 src2 carry =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let y = resolve_atomic_with lno carry cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, c) = resolve_lcarry_with lno flag cm vm ym gm in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Isbcr (c, v, a1, a2, y)])

  let parse_ssbb_at lno dest src1 src2 carry =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let y = resolve_atomic_with lno carry cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Isbb (v, a1, a2, y)])

  let parse_ssbbs_at lno flag dest src1 src2 carry =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let y = resolve_atomic_with lno carry cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, c) = resolve_lcarry_with lno flag cm vm ym gm in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Isbbs (c, v, a1, a2, y)])

  let parse_ssbbr_at lno flag dest src1 src2 carry =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let y = resolve_atomic_with lno carry cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, c) = resolve_lcarry_with lno flag cm vm ym gm in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Isbbr (c, v, a1, a2, y)])

  let parse_smul_at lno dest src1 src2 =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Imul (v, a1, a2)])

  let parse_smuls_at lno flag dest src1 src2 =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, c) = resolve_lcarry_with lno flag cm vm ym gm in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Imuls (c, v, a1, a2)])

  let parse_smulr_at lno flag dest src1 src2 =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, c) = resolve_lcarry_with lno flag cm vm ym gm in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm (Some ty) in
      (vm, ym, gm, [lno, Imulr (c, v, a1, a2)])

  let parse_smull_at lno destH destL src1 src2 =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, vh) = resolve_lv_with lno destH cm vm ym gm (Some ty) in
      let (vm, ym, gm, vl) =
        resolve_lv_with lno destL cm vm ym gm (Some (to_uint ty)) in
      (vm, ym, gm, [lno, Imull (vh, vl, a1, a2)])

  let parse_smulj_at lno dest src1 src2 =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let ty = typ_of_atomic a1 in
      let (vm, ym, gm, v) =
        resolve_lv_with lno dest cm vm ym gm (Some (to_double_size ty)) in
      (vm, ym, gm, [lno, Imulj (v, a1, a2)])

  let parse_ssplit_at lno destH destL src num =
    fun _fm cm vm ym gm ->
      let a = resolve_atomic_with lno src cm vm ym gm in
      let n = num cm in
      let ty = typ_of_atomic a in
      let (vm, ym, gm, vh) = resolve_lv_with lno destH cm vm ym gm (Some ty) in
      let (vm, ym, gm, vl) =
        resolve_lv_with lno destL cm vm ym gm (Some (to_uint ty)) in
      let _ =
        let w = size_of_var vl in
        if Z.leq n Z.zero || Z.geq n (Z.of_int w) then
          raise_at lno ("The position of a split should be in between 0 and " ^ string_of_int w ^ " (both excluded)")
      in
      (vm, ym, gm, [lno, Isplit (vh, vl, a, n)])

  let parse_and_at lno dest src1 src2 =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm None in
      (vm, ym, gm, [lno, Iand (v, a1, a2)])

  let parse_or_at lno dest src1 src2 =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm None in
      (vm, ym, gm, [lno, Ior (v, a1, a2)])

  let parse_xor_at lno dest src1 src2 =
    fun _fm cm vm ym gm ->
      let a1 = resolve_atomic_with lno src1 cm vm ym gm in
      let a2 = resolve_atomic_with lno src2 cm vm ym gm in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm None in
      (vm, ym, gm, [lno, Ixor (v, a1, a2)])

  let parse_not_at lno dest src =
    fun _fm cm vm ym gm ->
      let a = resolve_atomic_with lno src cm vm ym gm in
      let (vm, ym, gm, v) = resolve_lv_with lno dest cm vm ym gm None in
      (vm, ym, gm, [lno, Inot (v, a)])

  let parse_cast_at lno optlv dest src =
    fun _fm cm vm ym gm ->
      let a = resolve_atomic_with lno src cm vm ym gm in
      let (vm, ym, gm, v) =
        resolve_lv_or_lcarry_with lno dest cm vm ym gm in
	  (* determine the type of the discarded part *)
      let od_typ =
	match typ_of_var v, typ_of_atomic a with
	| Tuint wv, Tuint wa -> Some (Tuint (abs (wa - wv)))
	| Tuint wv, Tsint wa -> if wv >= wa then Some (Tuint 1)
                                else Some (Tsint (wa - wv))
	| Tsint wv, Tuint wa -> if wv > wa then Some (Tsint (wv - wa))
                                else if wv = wa then Some bit_t
                                else Some (Tuint (wa - wv))
	| Tsint wv, Tsint wa -> if wv >= wa then Some (Tsint (wv - wa))
                                else Some (Tsint (wa - wv + 1)) in
      let (vm, ym, gm, od) =
		match optlv with
		| None -> (vm, ym, gm, None)
		| Some (`LV od) ->
		   let (vm, ym, gm, d) =
                     resolve_lv_with lno od cm vm ym gm od_typ in
		   (vm, ym, gm, Some d) in
	  (* the discarded part must be a ghost variable *)
	  let gm = apply_to_option (fun d -> SM.add d.vname d gm) gm od in
      (vm, ym, gm, [lno, Icast (od, v, a)])

  let parse_vpc_at lno dest src =
    fun _fm cm vm ym gm ->
      let a = resolve_atomic_with lno src cm vm ym gm in
      let (vm, ym, gm, v) = resolve_lv_or_lcarry_with lno dest cm vm ym gm in
      (vm, ym, gm, [lno, Ivpc (v, a)])

  let parse_join_at lno dest srcH srcL =
    fun _fm cm vm ym gm ->
      let ah = resolve_atomic_with lno srcH cm vm ym gm in
      let al = resolve_atomic_with lno srcL cm vm ym gm in
      let ty = typ_of_atomic ah in
      let (vm, ym, gm, v) =
        resolve_lv_with lno dest cm vm ym gm (Some (to_double_size ty)) in
      (vm, ym, gm, [lno, Ijoin (v, ah, al)])

  let parse_assert_at lno bexp_token =
    fun _fm cm vm ym gm ->
      (vm, ym, gm, [lno, Iassert (bexp_token cm vm ym gm)])

  let parse_assume_at lno bexp_token =
    fun _fm cm vm ym gm ->
      (vm, ym, gm, [lno, Iassume (bexp_token cm vm ym gm)])

  let parse_cut_at lno bexp_prove_with_token =
    fun _fm cm vm ym gm ->
      let ((e, r), epwss, rpwss) = bexp_prove_with_token cm vm ym gm in
      (vm, ym, gm, [lno, Iecut (e, epwss); lno, Ircut (r, rpwss)])

  let parse_ecut_at lno ebexp_prove_with_token =
    fun _fm cm vm ym gm ->
    let (e, epwss) = ebexp_prove_with_token cm vm ym gm in
    (vm, ym, gm, [lno, Iecut (e, epwss)])

  let parse_rcut_at lno rbexp_prove_with_token =
    fun _fm cm vm ym gm ->
      let (r, rpwss) = rbexp_prove_with_token cm vm ym gm in
      (vm, ym, gm, [lno, Ircut (r, rpwss)])

  let parse_ghost_at lno gvars_token bexp_token =
    fun _fm cm vm ym gm ->
      let gvars = gvars_token cm vm ym gm in
      let gm = vm_of_vs (VS.union gvars (vs_of_vm gm)) in
      let e = bexp_token cm vm ym gm in
      let bad_ebexps = List.filter (fun e -> not (eq_ebexp e etrue) && VS.is_empty (VS.inter gvars (vars_ebexp e))) (split_eand (eqn_bexp e)) in
      let bad_rbexps = List.filter (fun e -> not (eq_rbexp e rtrue) && VS.is_empty (VS.inter gvars (vars_rbexp e))) (split_rand (rng_bexp e)) in
      if List.length bad_ebexps > 0 then raise_at lno ("The algebraic expression " ^ string_of_ebexp (List.hd bad_ebexps) ^ " is defined without using any ghost variable.")
      else if List.length bad_rbexps > 0 then raise_at lno ("The range expression " ^ string_of_rbexp (List.hd bad_rbexps) ^ " is defined without using any ghost variable.")
      else (vm, ym, gm, [lno, Ighost (gvars, e)])

  let is_type_compatible formal actual =
    match actual with
    | Avar v -> formal.vtyp = v.vtyp
    | Aconst (ty, _n) -> formal.vtyp = ty

  let parse_call_at lno fname_token actuals_token =
    fun fm cm vm ym gm ->
      (* The function name *)
      let fname = fname_token in
      (* The function definition *)
      let f =
        try
          SM.find fname fm
        with Not_found ->
          raise_at lno ("Call an undefined function '" ^ fname ^ "'.") in
      (* The actual paramaters, the types of formal arguments are requried to parse actual parameters *)
      let actuals = actuals_token (List.map typ_of_var f.fargs, List.map typ_of_var f.fouts) cm vm ym gm in
      (* Rename local variables *)
      let (fbody, fargs, fouts, fvs, fys, fgs) =
        if !Options.Std.rename_local then
          let rename_var v = mkvar (fname ^ "_local_" ^ v.vname) v.vtyp in
          let fargs = List.map rename_var f.fargs in
          let fouts = List.map rename_var f.fouts in
          let fvs = VS.map rename_var (vs_of_vm f.fvm) in
          let fys = VS.map rename_var (vs_of_vm f.fym) in
          let fgs = VS.map rename_var (vs_of_vm f.fgm) in
          let local_renamer = object (* (self) *)
                                inherit nop_visitor
                                method! vvar v = ChangeTo (rename_var v)
                              end in
          let fbody = visit_lined_program local_renamer f.fbody in
          (fbody, fargs, fouts, fvs, fys, fgs)
        else
          (f.fbody, f.fargs, f.fouts, vs_of_vm f.fvm, vs_of_vm f.fym, vs_of_vm f.fgm) in
      (* Formal parameters after renaming *)
      let inputs = fargs in
      let outputs = fouts in
      let formals = inputs@outputs in
      (* Check the number of actual parameters *)
      let _ =
        if List.length actuals != List.length formals then
          raise_at lno ("Failed to call the function " ^ fname ^ ": numbers of arguments mismatch.") in
      (* Check types of actual parameters, this should be done in parsing actual parameters *)
      let _ =
        List.iter2 (fun formal actual ->
                     if not (is_type_compatible formal actual) then
                       raise_at lno ("The type of the actual parameter " ^ string_of_atomic actual
                                     ^ " is not compatible with the type of the formal parameter " ^ string_of_var formal))
                   formals actuals in
      (* Check naming conflicts *)
      let _ =
        let (vsclash, ysclash, gsclash) =
          let actvars = List.fold_left (fun res a -> VS.union res (vars_atomic a)) VS.empty actuals in
          (* variables not replaced by formal parameters cannot have the same name as the name of any actual parameter *)
          let unrenamed_vs = VS.diff fvs (VS.of_list formals) in
          let unrenamed_ys = VS.diff fys (VS.of_list formals) in
          let unrenamed_gs = VS.diff fgs (VS.of_list formals) in
          let vsclash = VS.inter actvars unrenamed_vs in
          let ysclash = VS.inter actvars unrenamed_ys in
          let gsclash = VS.inter actvars unrenamed_gs in
          (vsclash, ysclash, gsclash) in
        if not (VS.is_empty gsclash) then
          let v = VS.choose gsclash in
          raise_at lno ("Naming conflict: the actual argument " ^ string_of_var v ^ " is already defined as a ghost variable in function " ^ fname ^ ".")
        else if not (VS.is_empty ysclash) then
          let v = VS.choose ysclash in
          raise_at lno ("Naming conflict: the actual argument " ^ string_of_var v ^ " is already defined as a carry in function " ^ fname ^ ".")
        else if not (VS.is_empty vsclash) then
          let v = VS.choose vsclash in
          raise_at lno ("Naming conflict: the actual argument " ^ string_of_var v ^ " is already defined in function " ^ fname ^ ".")
      in
      let pats = List.combine formals actuals in
      let vs = vs_of_vm vm in
      let ys = vs_of_vm ym in
      let gs = vs_of_vm gm in
      (* Check undefined variables *)
      let _ =
        let undefined =
          List.flatten (List.map (fun (formal, actual) ->
                                   match actual with
                                   | Avar v -> if mem_var formal inputs && not (VS.mem v vs) then [v] else []
                                   | _ -> []
                                 ) pats) in
        if List.length undefined > 0 then
          raise_at lno ("Undefined variable: " ^ string_of_var (List.hd undefined))
      in
      let p = subst_lined_program pats fbody in
      (* Update variable types *)
      let subst_varmap vm =
        (*
         * subst_lval fails if the corresponding actual parameter of a formal parameter is a constant.
         * In this case, no variable type will be updated.
         *)
        VS.of_list (List.flatten (List.map (fun v -> try [subst_lval pats v] with _ -> []) (VS.elements vm))) in
      let update_varset vs vsp =
        (* To update variable types, we need to remove variables with new types first. *)
        VS.union (VS.diff vs vsp) vsp in
      let vsp = subst_varmap fvs in
      let ysp = subst_varmap fys in
      let gsp = subst_varmap fgs in
      (vm_of_vs (update_varset vs vsp), vm_of_vs (update_varset ys ysp), vm_of_vs (update_varset gs gsp), p)

  let recognize_instr_at lno instr fm cm vm ym gm =
      match instr with
      | `MOV (`LVPLAIN dest, src) ->
         parse_imov_at lno dest src fm cm vm ym gm
      | `SHL (`LVPLAIN dest, src, num) ->
         parse_ishl_at lno dest src num fm cm vm ym gm
      | `CSHL (`LVPLAIN destH, `LVPLAIN destL, src1, src2, num) ->
         parse_cshl_at lno destH destL src1 src2 num fm cm vm ym gm
      | `SET (`LVCARRY dest) ->
         parse_set_at lno dest fm cm vm ym gm
      | `CLEAR (`LVCARRY dest) ->
         parse_clear_at lno dest fm cm vm ym gm
      | `NONDET (`LVPLAIN dest) ->
         parse_nondet_at lno dest fm cm vm ym gm
      | `CMOV (`LVPLAIN dest, carry, src1, src2) ->
         parse_cmov_at lno dest carry src1 src2 fm cm vm ym gm
      | `ADD (`LVPLAIN dest, src1, src2) ->
         parse_add_at lno dest src1 src2 fm cm vm ym gm
      | `ADDS (`LVCARRY flag, `LVPLAIN dest, src1, src2) ->
         parse_adds_at lno flag dest src1 src2 fm cm vm ym gm
      | `ADDR (`LVCARRY flag, `LVPLAIN dest, src1, src2) ->
         parse_addr_at lno flag dest src1 src2 fm cm vm ym gm
      | `ADC (`LVPLAIN dest, src1, src2, carry) ->
         parse_adc_at lno dest src1 src2 carry fm cm vm ym gm
      | `ADCS (`LVCARRY flag, `LVPLAIN dest, src1, src2, carry) ->
         parse_adcs_at lno flag dest src1 src2 carry fm cm vm ym gm
      | `ADCR (`LVCARRY flag, `LVPLAIN dest, src1, src2, carry) ->
         parse_adcr_at lno flag dest src1 src2 carry fm cm vm ym gm
      | `SUB (`LVPLAIN dest, src1, src2) ->
         parse_sub_at lno dest src1 src2 fm cm vm ym gm
      | `SUBC (`LVCARRY flag, `LVPLAIN dest, src1, src2) ->
         parse_subc_at lno flag dest src1 src2 fm cm vm ym gm
      | `SUBB (`LVCARRY flag, `LVPLAIN dest, src1, src2) ->
         parse_subb_at lno flag dest src1 src2 fm cm vm ym gm
      | `SUBR (`LVCARRY flag, `LVPLAIN dest, src1, src2) ->
         parse_subr_at lno flag dest src1 src2 fm cm vm ym gm
      | `SBC (`LVPLAIN dest, src1, src2, carry) ->
         parse_sbc_at lno dest src1 src2 carry fm cm vm ym gm
      | `SBCS (`LVCARRY flag, `LVPLAIN dest, src1, src2, carry) ->
         parse_sbcs_at lno flag dest src1 src2 carry fm cm vm ym gm
      | `SBCR (`LVCARRY flag, `LVPLAIN dest, src1, src2, carry) ->
         parse_sbcr_at lno flag dest src1 src2 carry fm cm vm ym gm
      | `SBB (`LVPLAIN dest, src1, src2, carry) ->
         parse_sbb_at lno dest src1 src2 carry fm cm vm ym gm
      | `SBBS (`LVCARRY flag, `LVPLAIN dest, src1, src2, carry) ->
         parse_sbbs_at lno flag dest src1 src2 carry fm cm vm ym gm
      | `SBBR (`LVCARRY flag, `LVPLAIN dest, src1, src2, carry) ->
         parse_sbbr_at lno flag dest src1 src2 carry fm cm vm ym gm
      | `MUL (`LVPLAIN dest, src1, src2) ->
         parse_mul_at lno dest src1 src2 fm cm vm ym gm
      | `MULS (`LVCARRY flag, `LVPLAIN dest, src1, src2) ->
         parse_muls_at lno flag dest src1 src2 fm cm vm ym gm
      | `MULR (`LVCARRY flag, `LVPLAIN dest, src1, src2) ->
         parse_mulr_at lno flag dest src1 src2 fm cm vm ym gm
      | `MULL (`LVPLAIN destH, `LVPLAIN destL, src1, src2) ->
         parse_mull_at lno destH destL src1 src2 fm cm vm ym gm
      | `MULJ (`LVPLAIN dest, src1, src2) ->
         parse_mulj_at lno dest src1 src2 fm cm vm ym gm
      | `SPLIT (`LVPLAIN destH, `LVPLAIN destL, src, num) ->
         parse_split_at lno destH destL src num fm cm vm ym gm
      | `UADD (`LVPLAIN dest, src1, src2) ->
         parse_uadd_at lno dest src1 src2 fm cm vm ym gm
      | `UADDS (`LVCARRY flag, `LVPLAIN dest, src1, src2) ->
         parse_uadds_at lno flag dest src1 src2 fm cm vm ym gm
      | `UADDR (`LVCARRY flag, `LVPLAIN dest, src1, src2) ->
         parse_uaddr_at lno flag dest src1 src2 fm cm vm ym gm
      | `UADC (`LVPLAIN dest, src1, src2, carry) ->
         parse_uadc_at lno dest src1 src2 carry fm cm vm ym gm
      | `UADCS (`LVCARRY flag, `LVPLAIN dest, src1, src2, carry) ->
         parse_uadcs_at lno flag dest src1 src2 carry fm cm vm ym gm
      | `UADCR (`LVCARRY flag, `LVPLAIN dest, src1, src2, carry) ->
         parse_uadcr_at lno flag dest src1 src2 carry fm cm vm ym gm
      | `USUB (`LVPLAIN dest, src1, src2) ->
         parse_usub_at lno dest src1 src2 fm cm vm ym gm
      | `USUBC (`LVCARRY flag, `LVPLAIN dest, src1, src2) ->
         parse_usubc_at lno flag dest src1 src2 fm cm vm ym gm
      | `USUBB (`LVCARRY flag, `LVPLAIN dest, src1, src2) ->
         parse_usubb_at lno flag dest src1 src2 fm cm vm ym gm
      | `USUBR (`LVCARRY flag, `LVPLAIN dest, src1, src2) ->
         parse_usubr_at lno flag dest src1 src2 fm cm vm ym gm
      | `USBC (`LVPLAIN dest, src1, src2, carry) ->
         parse_usbc_at lno dest src1 src2 carry fm cm vm ym gm
      | `USBCS (`LVCARRY flag, `LVPLAIN dest, src1, src2, carry) ->
         parse_usbcs_at lno flag dest src1 src2 carry fm cm vm ym gm
      | `USBCR (`LVCARRY flag, `LVPLAIN dest, src1, src2, carry) ->
         parse_usbcr_at lno flag dest src1 src2 carry fm cm vm ym gm
      | `USBB (`LVPLAIN dest, src1, src2, carry) ->
         parse_usbb_at lno dest src1 src2 carry fm cm vm ym gm
      | `USBBS (`LVCARRY flag, `LVPLAIN dest, src1, src2, carry) ->
         parse_usbbs_at lno flag dest src1 src2 carry fm cm vm ym gm
      | `USBBR (`LVCARRY flag, `LVPLAIN dest, src1, src2, carry) ->
         parse_usbbr_at lno flag dest src1 src2 carry fm cm vm ym gm
      | `UMUL (`LVPLAIN dest, src1, src2) ->
         parse_umul_at lno dest src1 src2 fm cm vm ym gm
      | `UMULS (`LVCARRY flag, `LVPLAIN dest, src1, src2) ->
         parse_umuls_at lno flag dest src1 src2 fm cm vm ym gm
      | `UMULR (`LVCARRY flag, `LVPLAIN dest, src1, src2) ->
         parse_umulr_at lno flag dest src1 src2 fm cm vm ym gm
      | `UMULL (`LVPLAIN destH, `LVPLAIN destL, src1, src2) ->
         parse_umull_at lno destH destL src1 src2 fm cm vm ym gm
      | `UMULJ (`LVPLAIN dest, src1, src2) ->
         parse_umulj_at lno dest src1 src2 fm cm vm ym gm
      | `USPLIT (`LVPLAIN destH, `LVPLAIN destL, src, num) ->
         parse_usplit_at lno destH destL src num fm cm vm ym gm
      | `SADD (`LVPLAIN dest, src1, src2) ->
         parse_sadd_at lno dest src1 src2 fm cm vm ym gm
      | `SADDS (`LVCARRY flag, `LVPLAIN dest, src1, src2) ->
         parse_sadds_at lno flag dest src1 src2 fm cm vm ym gm
      | `SADDR (`LVCARRY flag, `LVPLAIN dest, src1, src2) ->
         parse_saddr_at lno flag dest src1 src2 fm cm vm ym gm
      | `SADC (`LVPLAIN dest, src1, src2, carry) ->
         parse_sadc_at lno dest src1 src2 carry fm cm vm ym gm
      | `SADCS (`LVCARRY flag, `LVPLAIN dest, src1, src2, carry) ->
         parse_sadcs_at lno flag dest src1 src2 carry fm cm vm ym gm
      | `SADCR (`LVCARRY flag, `LVPLAIN dest, src1, src2, carry) ->
         parse_sadcr_at lno flag dest src1 src2 carry fm cm vm ym gm
      | `SSUB (`LVPLAIN dest, src1, src2) ->
         parse_ssub_at lno dest src1 src2 fm cm vm ym gm
      | `SSUBC (`LVCARRY flag, `LVPLAIN dest, src1, src2) ->
         parse_ssubc_at lno flag dest src1 src2 fm cm vm ym gm
      | `SSUBB (`LVCARRY flag, `LVPLAIN dest, src1, src2) ->
         parse_ssubb_at lno flag dest src1 src2 fm cm vm ym gm
      | `SSUBR (`LVCARRY flag, `LVPLAIN dest, src1, src2) ->
         parse_ssubr_at lno flag dest src1 src2 fm cm vm ym gm
      | `SSBC (`LVPLAIN dest, src1, src2, carry) ->
         parse_ssbc_at lno dest src1 src2 carry fm cm vm ym gm
      | `SSBCS (`LVCARRY flag, `LVPLAIN dest, src1, src2, carry) ->
         parse_ssbcs_at lno flag dest src1 src2 carry fm cm vm ym gm
      | `SSBCR (`LVCARRY flag, `LVPLAIN dest, src1, src2, carry) ->
         parse_ssbcr_at lno flag dest src1 src2 carry fm cm vm ym gm
      | `SSBB (`LVPLAIN dest, src1, src2, carry) ->
         parse_ssbb_at lno dest src1 src2 carry fm cm vm ym gm
      | `SSBBS (`LVCARRY flag, `LVPLAIN dest, src1, src2, carry) ->
         parse_ssbbs_at lno flag dest src1 src2 carry fm cm vm ym gm
      | `SSBBR (`LVCARRY flag, `LVPLAIN dest, src1, src2, carry) ->
         parse_ssbbr_at lno flag dest src1 src2 carry fm cm vm ym gm
      | `SMUL (`LVPLAIN dest, src1, src2) ->
         parse_smul_at lno dest src1 src2 fm cm vm ym gm
      | `SMULS (`LVCARRY flag, `LVPLAIN dest, src1, src2) ->
         parse_smuls_at lno flag dest src1 src2 fm cm vm ym gm
      | `SMULR (`LVCARRY flag, `LVPLAIN dest, src1, src2) ->
         parse_smulr_at lno flag dest src1 src2 fm cm vm ym gm
      | `SMULL (`LVPLAIN destH, `LVPLAIN destL, src1, src2) ->
         parse_smull_at lno destH destL src1 src2 fm cm vm ym gm
      | `SMULJ (`LVPLAIN dest, src1, src2) ->
         parse_smulj_at lno dest src1 src2 fm cm vm ym gm
      | `SSPLIT (`LVPLAIN destH, `LVPLAIN destL, src, num) ->
         parse_ssplit_at lno destH destL src num fm cm vm ym gm
      | `AND (`LVPLAIN dest, src1, src2) ->
         parse_and_at lno dest src1 src2 fm cm vm ym gm
      | `OR (`LVPLAIN dest, src1, src2) ->
         parse_or_at lno dest src1 src2 fm cm vm ym gm
      | `XOR (`LVPLAIN dest, src1, src2) ->
         parse_xor_at lno dest src1 src2 fm cm vm ym gm
      | `NOT (`LVPLAIN dest, src) ->
         parse_not_at lno dest src fm cm vm ym gm
      | `CAST (optlv, `LV dest, src) ->
         parse_cast_at lno optlv dest src fm cm vm ym gm
      | `VPC (`LV dest, src) ->
         parse_vpc_at lno dest src fm cm vm ym gm
      | `JOIN (`LVPLAIN dest, srcH, srcL) ->
         parse_join_at lno dest srcH srcL fm cm vm ym gm
      | `ASSERT bexp ->
         parse_assert_at lno bexp fm cm vm ym gm
      | `ASSUME bexp ->
         parse_assume_at lno bexp fm cm vm ym gm
      | `CUT bexp_prove_with ->
         parse_cut_at lno bexp_prove_with fm cm vm ym gm
      | `ECUT ebexp_prove_with ->
         parse_ecut_at lno ebexp_prove_with fm cm vm ym gm
      | `RCUT rbexp_prove_with ->
         parse_rcut_at lno rbexp_prove_with fm cm vm ym gm
      | `GHOST (gvars, bexp) ->
         parse_ghost_at lno gvars bexp fm cm vm ym gm
      | `CALL (id, actuals) ->
         parse_call_at lno id actuals fm cm vm ym gm
      | `NOP -> (vm, ym, gm, [])

  let parse_instrs instrs fm cm vm ym gm =
    let helper (vm0, ym0, gm0, prog_rev) (lno, instr0) =
      let (vm1, ym1, gm1, prog) =
        recognize_instr_at lno instr0 fm cm vm0 ym0 gm0 in
      (vm1, ym1, gm1, List.rev_append prog prog_rev) in
    let (vm, ym, gm, p') = List.fold_left helper (vm, ym, gm, []) instrs in
    (vm, ym, gm, List.rev p')

%}

%token <string> COMMENT
%token <Z.t> NUM
%token <string> ID
%token <int> UINT SINT
%token BIT
%token LBRAC RBRAC LPAR RPAR LSQUARE RSQUARE COMMA SEMICOLON DOT DOTDOT VBAR COLON
/* Instructions */
%token CONST MOV
%token ADD ADDS ADDR ADC ADCS ADCR SUB SUBC SUBB SUBR SBC SBCS SBCR SBB SBBS SBBR MUL MULS MULR MULL MULJ SPLIT
%token UADD UADDS UADDR UADC UADCS UADCR USUB USUBC USUBB USUBR USBC USBCS USBCR USBB USBBS USBBR UMUL UMULS UMULR UMULL UMULJ USPLIT
%token SADD SADDS SADDR SADC SADCS SADCR SSUB SSUBC SSUBB SSUBR SSBC SSBCS SSBCR SSBB SSBBS SSBBR SMUL SMULS SMULR SMULL SMULJ SSPLIT
%token SHL CSHL SET CLEAR NONDET CMOV AND OR NOT CAST VPC JOIN ASSERT ASSUME GHOST
%token CUT ECUT RCUT NOP
/* Logical Expressions */
%token VARS NEG SQ EXT UEXT SEXT MOD UMOD SREM SMOD XOR ULT ULE UGT UGE SLT SLE SGT SGE SHR SAR
/* Predicates */
%token TRUE EQ EQMOD EQUMOD EQSMOD EQSREM
/* Operators */
%token ADDOP SUBOP MULOP POWOP ULEOP ULTOP UGEOP UGTOP SLEOP SLTOP SGEOP SGTOP EQOP NEGOP MODOP LANDOP LOROP NOTOP ANDOP OROP XOROP SHLOP SHROP SAROP
/* Others */
%token AT PROC CALL ULIMBS SLIMBS PROVE WITH ALL CUTS ASSUMES GHOSTS PRECONDITION DEREFOP
%token EOF

%left LOROP
%left LANDOP
%nonassoc EQOP ULTOP ULEOP UGTOP UGEOP SLTOP SLEOP SGTOP SGEOP
%left OROP
%left XOROP
%left ANDOP
%left SHLOP SHROP SAROP
%left ADDOP SUBOP
%left MULOP
%left POWOP
%right NEGOP NOTOP
%left MODOP
%nonassoc VAR CONST NEG ADD SUB MUL SQ UMOD SREM SMOD NOT AND OR XOR ULT ULE UGT UGE SLT SLE SGT SGE SHL SHR SAR
%nonassoc EQ EQMOD
%nonassoc UMINUS

%start spec
%start prog
%type <(Ast.Cryptoline.VS.t * Typecheck.Std.spec)> spec
%type <Ast.Cryptoline.lined_program> prog

%%

spec:
  procs EOF
  {
    (*
     * fm: a map from a name to a function
     * cm: a map from a name to a constant
     *)
    let (fm, _cm) = $1 SM.empty SM.empty in
    try
      let m = SM.find main fm in
      (VS.of_list m.fargs, { spre = m.fpre; sprog = m.fbody; spost = m.fpost; sepwss = m.fepwss; srpwss = m.frpwss })
    with Not_found ->
      raise (ParseError "A main function is required.")
  }
;

procs:
  proc procs
  {
    fun fm cm ->
      let (fm, cm) = $1 fm cm in
      let (fm, cm) = $2 fm cm in
      (fm, cm)
  }
  | { fun fm cm -> (fm, cm) }
;

proc:
  PROC ID LPAR formals RPAR EQOP pre program post
  {
    let lno = !lnum in
    fun fm cm ->
      let fname = $2 in
      if SM.mem fname fm then raise_at lno ("The procedure " ^ fname ^ " is redefined.")
      else
        (* A map of defined variables *)
        let (args, outs) = $4 lno in
        let vm = vm_of_list args in
        (* A map of carry variables *)
        let ym = SM.empty in
        (* A map of ghost variables *)
        let gm = SM.empty in
        let f =
          match $7 cm vm ym gm with
          | None -> btrue
          | Some e -> e in
        let (vm, ym, gm, p) = parse_instrs $8 fm cm vm ym gm in
        let (g, epwss, rpwss) =
          match $9 cm vm ym gm with
          | None -> (btrue, [], [])
          | Some e -> e in
        (SM.add fname { fname = fname;
                        fargs = args;
                        fouts = outs;
                        fvm = vm;
                        fym = ym;
                        fgm = gm;
                        fbody = p;
                        fpre = f;
                        fpost = g;
                        fepwss = epwss;
                        frpwss = rpwss } fm, cm)
  }
  | CONST ID EQOP const
  {
    let lno = !lnum in
    fun fm cm ->
      let v = $2 in
      let n = $4 cm in
      if SM.mem v cm
      then raise_at lno ("Redefined constant: " ^ v)
      else
        let _ = if v = "wordsize" then Options.Std.wordsize := Z.to_int n in
        (fm, SM.add v n cm)
  }
;

pre:
    LBRAC bexp RBRAC                              { fun cm vm ym gm -> Some ($2 cm vm ym gm) }
  |                                               { fun _cm _vm _ym _gm -> None }
;

post:
    LBRAC bexp_prove_with RBRAC                   { fun cm vm ym gm -> Some ($2 cm vm ym gm) }
  |                                               { fun _cm _vm _ym _gm -> None }
;

formals:
    fvars                                         { fun lno -> ($1 lno, []) }
  | fvars SEMICOLON fvars                         { fun lno -> ($1 lno, $3 lno) }
  |                                               { fun _lno -> ([], []) }
;

fvars:
    fvar                                          { fun _lno -> $1 }
  | fvar COMMA fvars
  {
    fun lno ->
      let fvs1 = $1 in
      let fvs2 = $3 lno in
      let duplicates = List.filter (fun v -> mem_var v fvs2) fvs1 in
      if List.length duplicates > 0
      then raise_at lno ("Duplicate argument: " ^ string_of_var (List.hd duplicates))
      else fvs1@fvs2
  }
;

fvar:
    typ ID                                        { [mkvar $2 $1] }
  | ID AT typ                                     { [mkvar $1 $3] }
  | typ ID OROP NUM DOTDOT NUM
  {
    let ty = $1 in
    let prefix = $2 in
    let st = $4 in
    let ed = $6 in
    List.map (fun i -> mkvar (prefix ^ vars_expansion_infix ^ string_of_int i) ty) ((Z.to_int st)--(Z.to_int ed))
  }
  | ID AT typ OROP NUM DOTDOT NUM
  {
    let ty = $3 in
    let prefix = $1 in
    let st = $5 in
    let ed = $7 in
    List.map (fun i -> mkvar (prefix ^ vars_expansion_infix ^ string_of_int i) ty) ((Z.to_int st)--(Z.to_int ed))
  }
;

prog:
  program EOF
  {
    let fm = SM.empty in
    let cm = SM.empty in
    (* A map of defined variables *)
    let vm = SM.empty in
    (* A map of carry variables *)
    let ym = SM.empty in
    (* A map of ghost variables *)
    let gm = SM.empty in
    let (_vm, _ym, _gm, p) = parse_instrs $1 fm cm vm ym gm in
    p
  }
;

program:
  instrs                                          { $1 }
;

instrs:
    instr SEMICOLON instrs                        { $1 :: $3 }
  | instr SEMICOLON                               { [$1] }
  | instr SEMICOLON error                         { raise_at !lnum ("Unrecognized instruction.") }
;

instr:
    MOV lval atomic                           { (!lnum, `MOV ($2, $3)) }
  | lhs EQOP atomic                           { (!lnum, `MOV  (`LVPLAIN $1, $3)) }
  | SHL lval atomic const                     { (!lnum, `SHL ($2, $3, $4)) }
  | lhs EQOP SHL atomic const                 { (!lnum, `SHL (`LVPLAIN $1, $4, $5)) }
  | CSHL lval lval atomic atomic const        { (!lnum, `CSHL ($2, $3, $4, $5, $6)) }
  | lhs DOT lhs EQOP CSHL atomic atomic const { (!lnum, `CSHL (`LVPLAIN $1, `LVPLAIN $3, $6, $7, $8)) }
  | SET lcarry                                { (!lnum, `SET $2) }
  | CLEAR lcarry                              { (!lnum, `CLEAR $2) }
  | NONDET lval                               { (!lnum, `NONDET $2) }
  | CMOV lval carry atomic atomic             { (!lnum, `CMOV ($2, $3, $4, $5)) }
  | lhs EQOP CMOV carry atomic atomic         { (!lnum, `CMOV (`LVPLAIN $1, $4, $5, $6)) }
  | ADD lval atomic atomic                    { (!lnum, `ADD ($2, $3, $4)) }
  | lhs EQOP ADD atomic atomic                { (!lnum, `ADD (`LVPLAIN $1, $4, $5)) }
  | ADDS lcarry lval atomic atomic            { (!lnum, `ADDS ($2, $3, $4, $5)) }
  | lhs DOT lhs EQOP ADDS atomic atomic       { (!lnum, `ADDS (`LVCARRY $1, `LVPLAIN $3, $6, $7)) }
  | ADDR lcarry lval atomic atomic            { (!lnum, `ADDR ($2, $3, $4, $5)) }
  | lhs DOT lhs EQOP ADDR atomic atomic       { (!lnum, `ADDR (`LVCARRY $1, `LVPLAIN $3, $6, $7)) }
  | ADC lval atomic atomic carry              { (!lnum, `ADC ($2, $3, $4, $5)) }
  | lhs EQOP ADC atomic atomic carry          { (!lnum, `ADC (`LVPLAIN $1, $4, $5, $6)) }
  | ADCS lcarry lval atomic atomic carry      { (!lnum, `ADCS ($2, $3, $4, $5, $6)) }
  | lhs DOT lhs EQOP ADCS atomic atomic carry { (!lnum, `ADCS (`LVCARRY $1, `LVPLAIN $3, $6, $7, $8)) }
  | ADCR lcarry lval atomic atomic carry      { (!lnum, `ADCR ($2, $3, $4, $5, $6)) }
  | lhs DOT lhs EQOP ADCR atomic atomic carry { (!lnum, `ADCR (`LVCARRY $1, `LVPLAIN $3, $6, $7, $8)) }
  | SUB lval atomic atomic                    { (!lnum, `SUB ($2, $3, $4)) }
  | lhs EQOP SUB atomic atomic                { (!lnum, `SUB (`LVPLAIN $1, $4, $5)) }
  | SUBC lcarry lval atomic atomic            { (!lnum, `SUBC ($2, $3, $4, $5)) }
  | lhs DOT lhs EQOP SUBC atomic atomic       { (!lnum, `SUBC (`LVCARRY $1, `LVPLAIN $3, $6, $7)) }
  | SUBB lcarry lval atomic atomic            { (!lnum, `SUBB ($2, $3, $4, $5)) }
  | lhs DOT lhs EQOP SUBB atomic atomic       { (!lnum, `SUBB (`LVCARRY $1, `LVPLAIN $3, $6, $7)) }
  | SUBR lcarry lval atomic atomic            { (!lnum, `SUBR ($2, $3, $4, $5)) }
  | lhs DOT lhs EQOP SUBR atomic atomic       { (!lnum, `SUBR (`LVCARRY $1, `LVPLAIN $3, $6, $7)) }
  | SBC lval atomic atomic carry              { (!lnum, `SBC ($2, $3, $4, $5)) }
  | lhs EQOP SBC atomic atomic carry          { (!lnum, `SBC (`LVPLAIN $1, $4, $5, $6)) }
  | SBCS lcarry lval atomic atomic carry      { (!lnum, `SBCS ($2, $3, $4, $5, $6)) }
  | lhs DOT lhs EQOP SBCS atomic atomic carry { (!lnum, `SBCS (`LVCARRY $1, `LVPLAIN $3, $6, $7, $8)) }
  | SBCR lcarry lval atomic atomic carry      { (!lnum, `SBCR ($2, $3, $4, $5, $6)) }
  | lhs DOT lhs EQOP SBCR atomic atomic carry { (!lnum, `SBCR (`LVCARRY $1, `LVPLAIN $3, $6, $7, $8)) }
  | SBB lval atomic atomic carry              { (!lnum, `SBB ($2, $3, $4, $5)) }
  | lhs EQOP SBB atomic atomic carry          { (!lnum, `SBB (`LVPLAIN $1, $4, $5, $6)) }
  | SBBS lcarry lval atomic atomic carry      { (!lnum, `SBBS ($2, $3, $4, $5, $6)) }
  | lhs DOT lhs EQOP SBBS atomic atomic carry { (!lnum, `SBBS (`LVCARRY $1, `LVPLAIN $3, $6, $7, $8)) }
  | SBBR lcarry lval atomic atomic carry      { (!lnum, `SBBR ($2, $3, $4, $5, $6)) }
  | lhs DOT lhs EQOP SBBR atomic atomic carry { (!lnum, `SBBR (`LVCARRY $1, `LVPLAIN $3, $6, $7, $8)) }
  | MUL lval atomic atomic                    { (!lnum, `MUL ($2, $3, $4)) }
  | lhs EQOP MUL atomic atomic                { (!lnum, `MUL (`LVPLAIN $1, $4, $5)) }
  | MULS lcarry lval atomic atomic            { (!lnum, `MULS ($2, $3, $4, $5)) }
  | lhs DOT lhs EQOP MULS atomic atomic       { (!lnum, `MULS (`LVCARRY $1, `LVPLAIN $3, $6, $7)) }
  | MULR lcarry lval atomic atomic            { (!lnum, `MULR ($2, $3, $4, $5)) }
  | lhs DOT lhs EQOP MULR atomic atomic       { (!lnum, `MULR (`LVCARRY $1, `LVPLAIN $3, $6, $7)) }
  | MULL lval lval atomic atomic              { (!lnum, `MULL ($2, $3, $4, $5)) }
  | lhs DOT lhs EQOP MULL atomic atomic       { (!lnum, `MULL (`LVPLAIN $1, `LVPLAIN $3, $6, $7)) }
  | MULJ lval atomic atomic                   { (!lnum, `MULJ ($2, $3, $4)) }
  | lhs EQOP MULJ atomic atomic               { (!lnum, `MULJ (`LVPLAIN $1, $4, $5)) }
  | SPLIT lval lval atomic const              { (!lnum, `SPLIT ($2, $3, $4, $5)) }
  | lhs DOT lhs EQOP SPLIT atomic const       { (!lnum, `SPLIT (`LVPLAIN $1, `LVPLAIN $3, $6, $7)) }
  | UADD lval atomic atomic                   { (!lnum, `UADD ($2, $3, $4)) }
  | lhs EQOP UADD atomic atomic               { (!lnum, `UADD (`LVPLAIN $1, $4, $5)) }
  | UADDS lcarry lval atomic atomic           { (!lnum, `UADDS ($2, $3, $4, $5)) }
  | lhs DOT lhs EQOP UADDS atomic atomic      { (!lnum, `UADDS (`LVCARRY $1, `LVPLAIN $3, $6, $7)) }
  | UADDR lcarry lval atomic atomic           { (!lnum, `UADDR ($2, $3, $4, $5)) }
  | lhs DOT lhs EQOP UADDR atomic atomic      { (!lnum, `UADDR (`LVCARRY $1, `LVPLAIN $3, $6, $7)) }
  | UADC lval atomic atomic carry             { (!lnum, `UADC ($2, $3, $4, $5)) }
  | lhs EQOP UADC atomic atomic carry         { (!lnum, `UADC (`LVPLAIN $1, $4, $5, $6)) }
  | UADCS lcarry lval atomic atomic carry     { (!lnum, `UADCS ($2, $3, $4, $5, $6)) }
  | lhs DOT lhs EQOP UADCS atomic atomic carry{ (!lnum, `UADCS (`LVCARRY $1, `LVPLAIN $3, $6, $7, $8)) }
  | UADCR lcarry lval atomic atomic carry     { (!lnum, `UADCR ($2, $3, $4, $5, $6)) }
  | lhs DOT lhs EQOP UADCR atomic atomic carry{ (!lnum, `UADCR (`LVCARRY $1, `LVPLAIN $3, $6, $7, $8)) }
  | USUB lval atomic atomic                   { (!lnum, `USUB ($2, $3, $4)) }
  | lhs EQOP USUB atomic atomic               { (!lnum, `USUB (`LVPLAIN $1, $4, $5)) }
  | USUBC lcarry lval atomic atomic           { (!lnum, `USUBC ($2, $3, $4, $5)) }
  | lhs DOT lhs EQOP USUBC atomic atomic      { (!lnum, `USUBC (`LVCARRY $1, `LVPLAIN $3, $6, $7)) }
  | USUBB lcarry lval atomic atomic           { (!lnum, `USUBB ($2, $3, $4, $5)) }
  | lhs DOT lhs EQOP USUBB atomic atomic      { (!lnum, `USUBB (`LVCARRY $1, `LVPLAIN $3, $6, $7)) }
  | USUBR lcarry lval atomic atomic           { (!lnum, `USUBR ($2, $3, $4, $5)) }
  | lhs DOT lhs EQOP USUBR atomic atomic      { (!lnum, `USUBR (`LVCARRY $1, `LVPLAIN $3, $6, $7)) }
  | USBC lval atomic atomic carry             { (!lnum, `USBC ($2, $3, $4, $5)) }
  | lhs EQOP USBC atomic atomic carry         { (!lnum, `USBC (`LVPLAIN $1, $4, $5, $6)) }
  | USBCS lcarry lval atomic atomic carry     { (!lnum, `USBCS ($2, $3, $4, $5, $6)) }
  | lhs DOT lhs EQOP USBCS atomic atomic carry{ (!lnum, `USBCS (`LVCARRY $1, `LVPLAIN $3, $6, $7, $8)) }
  | USBCR lcarry lval atomic atomic carry     { (!lnum, `USBCR ($2, $3, $4, $5, $6)) }
  | lhs DOT lhs EQOP USBCR atomic atomic carry{ (!lnum, `USBCR (`LVCARRY $1, `LVPLAIN $3, $6, $7, $8)) }
  | USBB lval atomic atomic carry             { (!lnum, `USBB ($2, $3, $4, $5)) }
  | lhs EQOP USBB atomic atomic carry         { (!lnum, `USBB (`LVPLAIN $1, $4, $5, $6)) }
  | USBBS lcarry lval atomic atomic carry     { (!lnum, `USBBS ($2, $3, $4, $5, $6)) }
  | lhs DOT lhs EQOP USBBS atomic atomic carry{ (!lnum, `USBBS (`LVCARRY $1, `LVPLAIN $3, $6, $7, $8)) }
  | USBBR lcarry lval atomic atomic carry     { (!lnum, `USBBR ($2, $3, $4, $5, $6)) }
  | lhs DOT lhs EQOP USBBR atomic atomic carry{ (!lnum, `USBBR (`LVCARRY $1, `LVPLAIN $3, $6, $7, $8)) }
  | UMUL lval atomic atomic                   { (!lnum, `UMUL ($2, $3, $4)) }
  | lhs EQOP UMUL atomic atomic               { (!lnum, `UMUL (`LVPLAIN $1, $4, $5)) }
  | UMULS lcarry lval atomic atomic           { (!lnum, `UMULS ($2, $3, $4, $5)) }
  | lhs DOT lhs EQOP UMULS atomic atomic      { (!lnum, `UMULS (`LVCARRY $1, `LVPLAIN $3, $6, $7)) }
  | UMULR lcarry lval atomic atomic           { (!lnum, `UMULR ($2, $3, $4, $5)) }
  | lhs DOT lhs EQOP UMULR atomic atomic      { (!lnum, `UMULR (`LVCARRY $1, `LVPLAIN $3, $6, $7)) }
  | UMULL lval lval atomic atomic             { (!lnum, `UMULL ($2, $3, $4, $5)) }
  | lhs DOT lhs EQOP UMULL atomic atomic      { (!lnum, `UMULL (`LVPLAIN $1, `LVPLAIN $3, $6, $7)) }
  | UMULJ lval atomic atomic                  { (!lnum, `UMULJ ($2, $3, $4)) }
  | lhs EQOP UMULJ atomic atomic              { (!lnum, `UMULJ (`LVPLAIN $1, $4, $5)) }
  | USPLIT lval lval atomic const             { (!lnum, `USPLIT ($2, $3, $4, $5)) }
  | lhs DOT lhs EQOP USPLIT atomic const      { (!lnum, `USPLIT (`LVPLAIN $1, `LVPLAIN $3, $6, $7)) }
  | SADD lval atomic atomic                   { (!lnum, `SADD ($2, $3, $4)) }
  | lhs EQOP SADD atomic atomic               { (!lnum, `SADD (`LVPLAIN $1, $4, $5)) }
  | SADDS lcarry lval atomic atomic           { (!lnum, `SADDS ($2, $3, $4, $5)) }
  | lhs DOT lhs EQOP SADDS atomic atomic      { (!lnum, `SADDS (`LVCARRY $1, `LVPLAIN $3, $6, $7)) }
  | SADDR lcarry lval atomic atomic           { (!lnum, `SADDR ($2, $3, $4, $5)) }
  | lhs DOT lhs EQOP SADDR atomic atomic      { (!lnum, `SADDR (`LVCARRY $1, `LVPLAIN $3, $6, $7)) }
  | SADC lval atomic atomic carry             { (!lnum, `SADC ($2, $3, $4, $5)) }
  | lhs EQOP SADC atomic atomic carry         { (!lnum, `SADC (`LVPLAIN $1, $4, $5, $6)) }
  | SADCS lcarry lval atomic atomic carry     { (!lnum, `SADCS ($2, $3, $4, $5, $6)) }
  | lhs DOT lhs EQOP SADCS atomic atomic carry{ (!lnum, `SADCS (`LVCARRY $1, `LVPLAIN $3, $6, $7, $8)) }
  | SADCR lcarry lval atomic atomic carry     { (!lnum, `SADCR ($2, $3, $4, $5, $6)) }
  | lhs DOT lhs EQOP SADCR atomic atomic carry{ (!lnum, `SADCR (`LVCARRY $1, `LVPLAIN $3, $6, $7, $8)) }
  | SSUB lval atomic atomic                   { (!lnum, `SSUB ($2, $3, $4)) }
  | lhs EQOP SSUB atomic atomic               { (!lnum, `SSUB (`LVPLAIN $1, $4, $5)) }
  | SSUBC lcarry lval atomic atomic           { (!lnum, `SSUBC ($2, $3, $4, $5)) }
  | lhs DOT lhs EQOP SSUBC atomic atomic      { (!lnum, `SSUBC (`LVCARRY $1, `LVPLAIN $3, $6, $7)) }
  | SSUBB lcarry lval atomic atomic           { (!lnum, `SSUBB ($2, $3, $4, $5)) }
  | lhs DOT lhs EQOP SSUBB atomic atomic      { (!lnum, `SSUBB (`LVCARRY $1, `LVPLAIN $3, $6, $7)) }
  | SSUBR lcarry lval atomic atomic           { (!lnum, `SSUBR ($2, $3, $4, $5)) }
  | lhs DOT lhs EQOP SSUBR atomic atomic      { (!lnum, `SSUBR (`LVCARRY $1, `LVPLAIN $3, $6, $7)) }
  | SSBC lval atomic atomic carry             { (!lnum, `SSBC ($2, $3, $4, $5)) }
  | lhs EQOP SSBC atomic atomic carry         { (!lnum, `SSBC (`LVPLAIN $1, $4, $5, $6)) }
  | SSBCS lcarry lval atomic atomic carry     { (!lnum, `SSBCS ($2, $3, $4, $5, $6)) }
  | lhs DOT lhs EQOP SSBCS atomic atomic carry{ (!lnum, `SSBCS (`LVCARRY $1, `LVPLAIN $3, $6, $7, $8)) }
  | SSBCR lcarry lval atomic atomic carry     { (!lnum, `SSBCR ($2, $3, $4, $5, $6)) }
  | lhs DOT lhs EQOP SSBCR atomic atomic carry{ (!lnum, `SSBCR (`LVCARRY $1, `LVPLAIN $3, $6, $7, $8)) }
  | SSBB lval atomic atomic carry             { (!lnum, `SSBB ($2, $3, $4, $5)) }
  | lhs EQOP SSBB atomic atomic carry         { (!lnum, `SSBB (`LVPLAIN $1, $4, $5, $6)) }
  | SSBBS lcarry lval atomic atomic carry     { (!lnum, `SSBBS ($2, $3, $4, $5, $6)) }
  | lhs DOT lhs EQOP SSBBS atomic atomic carry{ (!lnum, `SSBBS (`LVCARRY $1, `LVPLAIN $3, $6, $7, $8)) }
  | SSBBR lcarry lval atomic atomic carry     { (!lnum, `SSBBR ($2, $3, $4, $5, $6)) }
  | lhs DOT lhs EQOP SSBBR atomic atomic carry{ (!lnum, `SSBBR (`LVCARRY $1, `LVPLAIN $3, $6, $7, $8)) }
  | SMUL lval atomic atomic                   { (!lnum, `SMUL ($2, $3, $4) )}
  | lhs EQOP SMUL atomic atomic               { (!lnum, `SMUL (`LVPLAIN $1, $4, $5) )}
  | SMULS lcarry lval atomic atomic           { (!lnum, `SMULS ($2, $3, $4, $5)) }
  | lhs DOT lhs EQOP SMULS atomic atomic      { (!lnum, `SMULS (`LVCARRY $1, `LVPLAIN $3, $6, $7)) }
  | SMULR lcarry lval atomic atomic           { (!lnum, `SMULR ($2, $3, $4, $5)) }
  | lhs DOT lhs EQOP SMULR atomic atomic      { (!lnum, `SMULR (`LVCARRY $1, `LVPLAIN $3, $6, $7)) }
  | SMULL lval lval atomic atomic             { (!lnum, `SMULL ($2, $3, $4, $5)) }
  | lhs DOT lhs EQOP SMULL atomic atomic      { (!lnum, `SMULL (`LVPLAIN $1, `LVPLAIN $3, $6, $7)) }
  | SMULJ lval atomic atomic                  { (!lnum, `SMULJ ($2, $3, $4)) }
  | lhs EQOP SMULJ atomic atomic              { (!lnum, `SMULJ (`LVPLAIN $1, $4, $5)) }
  | SSPLIT lval lval atomic const             { (!lnum, `SSPLIT ($2, $3, $4, $5)) }
  | lhs DOT lhs EQOP SSPLIT atomic const      { (!lnum, `SSPLIT (`LVPLAIN $1, `LVPLAIN $3, $6, $7)) }
  | AND lval atomic atomic                    { (!lnum, `AND ($2, $3, $4)) }
  | lhs EQOP AND atomic atomic                { (!lnum, `AND (`LVPLAIN $1, $4, $5)) }
  | OR lval atomic atomic                     { (!lnum, `OR ($2, $3, $4)) }
  | lhs EQOP OR atomic atomic                 { (!lnum, `OR (`LVPLAIN $1, $4, $5)) }
  | XOR lval atomic atomic                    { (!lnum, `XOR ($2, $3, $4)) }
  | lhs EQOP XOR atomic atomic                { (!lnum, `XOR (`LVPLAIN $1, $4, $5)) }
  | NOT lval atomic                           { (!lnum, `NOT ($2, $3)) }
  | lhs EQOP NOT atomic                       { (!lnum, `NOT (`LVPLAIN $1, $4)) }
  | CAST lval_or_lcarry atomic                { (!lnum, `CAST (None, $2, $3)) }
  | CAST LSQUARE lval_or_lcarry RSQUARE lval_or_lcarry atomic
                                              { (!lnum, `CAST (Some $3, $5, $6)) }
  | lhs EQOP CAST atomic                      { (!lnum, `CAST (None, `LV $1, $4)) }
  | VPC lval_or_lcarry atomic                 { (!lnum, `VPC ($2, $3)) }
  | lhs EQOP VPC atomic                       { (!lnum, `VPC (`LV $1, $4)) }
  | JOIN lval atomic atomic                   { (!lnum, `JOIN ($2, $3, $4)) }
  | lhs EQOP JOIN atomic atomic               { (!lnum, `JOIN (`LVPLAIN $1, $4, $5)) }
  | ASSERT bexp                               { (!lnum, `ASSERT $2) }
  | ASSUME bexp                               { (!lnum, `ASSUME $2) }
  | CUT bexp_prove_with                       { (!lnum, `CUT $2) }
  | ECUT ebexp_prove_with                     { (!lnum, `ECUT $2) }
  | RCUT rbexp_prove_with                     { (!lnum, `RCUT $2) }
  | GHOST gvars COLON bexp                    { (!lnum, `GHOST ($2, $4)) }
  /* Extensions */
  | CALL ID LPAR actuals RPAR                 { (!lnum, `CALL ($2, $4)) }
  | NOP                                       { (!lnum, `NOP) }
  /* Errors */
  | MOV error                                 { raise_at !lnum ("Bad mov instruction") }
  | ADD error                                 { raise_at !lnum ("Bad add instruction") }
  | ADDS error                                { raise_at !lnum ("Bad adds instruction") }
  | ADC error                                 { raise_at !lnum ("Bad adc instruction") }
  | ADCS error                                { raise_at !lnum ("Bad adcs instruction") }
  | SUB error                                 { raise_at !lnum ("Bad sub instruction") }
  | SUBC error                                { raise_at !lnum ("Bad subc instruction") }
  | SUBB error                                { raise_at !lnum ("Bad subb instruction") }
  | SUBR error                                { raise_at !lnum ("Bad subr instruction") }
  | SBC error                                 { raise_at !lnum ("Bad sbc instruction") }
  | SBCS error                                { raise_at !lnum ("Bad sbcs instruction") }
  | SBCR error                                { raise_at !lnum ("Bad sbcr instruction") }
  | SBB error                                 { raise_at !lnum ("Bad sbb instruction") }
  | SBBS error                                { raise_at !lnum ("Bad sbbs instruction") }
  | SBBR error                                { raise_at !lnum ("Bad sbbr instruction") }
  | MUL error                                 { raise_at !lnum ("Bad mul instruction") }
  | MULL error                                { raise_at !lnum ("Bad mull instruction") }
  | SPLIT error                               { raise_at !lnum ("Bad split instruction") }
  | UADD error                                { raise_at !lnum ("Bad uadd instruction") }
  | UADDS error                               { raise_at !lnum ("Bad uadds instruction") }
  | UADC error                                { raise_at !lnum ("Bad uadc instruction") }
  | UADCS error                               { raise_at !lnum ("Bad uadcs instruction") }
  | USUB error                                { raise_at !lnum ("Bad usub instruction") }
  | USUBC error                               { raise_at !lnum ("Bad usubc instruction") }
  | USUBB error                               { raise_at !lnum ("Bad usubb instruction") }
  | USUBR error                               { raise_at !lnum ("Bad usubr instruction") }
  | USBC error                                { raise_at !lnum ("Bad usbc instruction") }
  | USBCS error                               { raise_at !lnum ("Bad usbcs instruction") }
  | USBCR error                               { raise_at !lnum ("Bad usbcr instruction") }
  | USBB error                                { raise_at !lnum ("Bad usbb instruction") }
  | USBBS error                               { raise_at !lnum ("Bad usbbs instruction") }
  | USBBR error                               { raise_at !lnum ("Bad usbbr instruction") }
  | UMUL error                                { raise_at !lnum ("Bad umul instruction") }
  | UMULL error                               { raise_at !lnum ("Bad umull instruction") }
  | USPLIT error                              { raise_at !lnum ("Bad usplit instruction") }
  | SADD error                                { raise_at !lnum ("Bad sadd instruction") }
  | SADDS error                               { raise_at !lnum ("Bad sadds instruction") }
  | SADC error                                { raise_at !lnum ("Bad sadc instruction") }
  | SADCS error                               { raise_at !lnum ("Bad sadcs instruction") }
  | SSUB error                                { raise_at !lnum ("Bad ssub instruction") }
  | SSUBC error                               { raise_at !lnum ("Bad ssubc instruction") }
  | SSUBB error                               { raise_at !lnum ("Bad ssubb instruction") }
  | SSUBR error                               { raise_at !lnum ("Bad ssubr instruction") }
  | SSBC error                                { raise_at !lnum ("Bad ssbc instruction") }
  | SSBCS error                               { raise_at !lnum ("Bad ssbcs instruction") }
  | SSBCR error                               { raise_at !lnum ("Bad ssbcr instruction") }
  | SSBB error                                { raise_at !lnum ("Bad ssbb instruction") }
  | SSBBS error                               { raise_at !lnum ("Bad ssbbs instruction") }
  | SSBBR error                               { raise_at !lnum ("Bad ssbbr instruction") }
  | SMUL error                                { raise_at !lnum ("Bad smul instruction") }
  | SMULL error                               { raise_at !lnum ("Bad smull instruction") }
  | SSPLIT error                              { raise_at !lnum ("Bad ssplit instruction") }
  | SHL error                                 { raise_at !lnum ("Bad shl instruction") }
  | CSHL error                                { raise_at !lnum ("Bad cshl instruction") }
  | NONDET error                              { raise_at !lnum ("Bad nondet instruction") }
  | CALL ID LPAR error                        { raise_at !lnum (("Invalid actuals in the call instruction: " ^ $2)) }
  | CALL error                                { raise_at !lnum ("Bad call instruction") }
;

ebexp_prove_with:
  ebexp                                           { fun cm vm ym gm -> ($1 cm vm ym gm, []) }
| ebexp PROVE WITH prove_with_specs               { fun cm vm ym gm -> ($1 cm vm ym gm, $4) }
;

rbexp_prove_with:
  rbexp                                           { fun cm vm ym gm -> ($1 cm vm ym gm, []) }
| rbexp PROVE WITH prove_with_specs               { fun cm vm ym gm -> ($1 cm vm ym gm, $4) }
;

bexp_prove_with:
    TRUE                                          { fun _cm _vm _ym _gm -> (btrue, [], []) }
  | ebexp_prove_with VBAR rbexp_prove_with        { fun cm vm ym gm ->
                                                      let (e, epwss) = $1 cm vm ym gm in
                                                      let (r, rpwss) = $3 cm vm ym gm in
                                                      ((e, r), epwss, rpwss) }
  | ebexp_prove_with VBAR error                   { let lno = !lnum in
                                                    fun cm vm ym gm ->
                                                      raise_at lno ("Invalid range predicate after '" ^ string_of_ebexp (fst ($1 cm vm ym gm)) ^ "'.")
                                                  }
  | ebexp_prove_with error                        { raise_at !lnum ("Please use '&&' to separate algebraic predicates and range predicates.") }
;

prove_with_specs:
    prove_with_spec                               { [$1] }
  | prove_with_spec COMMA prove_with_specs        { $1::$3 }
;

prove_with_spec:
    PRECONDITION                                  { Precondition }
  | ALL CUTS                                      { AllCuts }
  | ALL ASSUMES                                   { AllAssumes }
  | ALL GHOSTS                                    { AllGhosts }
;

bexp:
    TRUE                                          { fun _cm _vm _ym _gm -> btrue }
  | ebexp VBAR rbexp                              { fun cm vm ym gm -> ($1 cm vm ym gm, $3 cm vm ym gm) }
  | ebexp VBAR error                              { let lno = !lnum in
                                                    fun cm vm ym gm ->
                                                      raise_at lno ("Invalid range predicate after '" ^ string_of_ebexp ($1 cm vm ym gm) ^ "'.")
                                                  }
  | ebexp error                                   { raise_at !lnum ("Please use '&&' to separate algebraic predicates and range predicates.") }
;

ebexp:
  ebexp_atomic LANDOP ebexp                       { fun cm vm ym gm -> Eand($1 cm vm ym gm, $3 cm vm ym gm) }
  | ebexp_atomic                                  { fun cm vm ym gm -> $1 cm vm ym gm }
;

ebexp_atomic:
    TRUE                                          { fun _cm _vm _ym _gm -> Etrue }
  | EQ eexp eexp_no_unary                         { fun cm vm ym gm -> Eeq ($2 cm vm ym gm, $3 cm vm ym gm) }
  | EQMOD eexp eexp_no_unary eexp_no_unary        { fun cm vm ym gm -> Eeqmod ($2 cm vm ym gm, $3 cm vm ym gm, [ $4 cm vm ym gm ]) }
  | EQMOD eexp eexp_no_unary LSQUARE eexp_no_unarys RSQUARE
                                                  { fun cm vm ym gm -> Eeqmod ($2 cm vm ym gm, $3 cm vm ym gm, $5 cm vm ym gm) }
  | AND ebexp_atomic_without_eqmod ebexp_atomic   { fun cm vm ym gm -> Eand ($2 cm vm ym gm, $3 cm vm ym gm) }
  | LPAR ebexp RPAR                               { fun cm vm ym gm -> $2 cm vm ym gm }
  | eexp EQOP eexp eq_suffix                      { fun cm vm ym gm ->
                                                      match $4 cm vm ym gm with
                                                      | None -> Eeq ($1 cm vm ym gm, $3 cm vm ym gm)
                                                      | Some ms -> Eeqmod ($1 cm vm ym gm, $3 cm vm ym gm, ms)
                                                  }
  | AND LSQUARE ebexps RSQUARE                    { fun cm vm ym gm -> eands ($3 cm vm ym gm) }
  | LANDOP LSQUARE ebexps RSQUARE                 { fun cm vm ym gm -> eands ($3 cm vm ym gm) }
  /* Errors */
/*  | ID error                                      { raise_at !lnum ("Invalid algebraic predicate after " ^ $1 ^ ".") }*/
;

ebexp_atomic_without_eqmod:
    TRUE                                          { fun _cm _vm _ym _gm -> Etrue }
  | EQ eexp eexp_no_unary                         { fun cm vm ym gm -> Eeq ($2 cm vm ym gm, $3 cm vm ym gm) }
  | EQMOD eexp eexp_no_unary eexp_no_unary        { fun cm vm ym gm -> Eeqmod ($2 cm vm ym gm, $3 cm vm ym gm, [ $4 cm vm ym gm ]) }
  | EQMOD eexp eexp_no_unary LSQUARE eexp_no_unarys RSQUARE
                                                  { fun cm vm ym gm -> Eeqmod ($2 cm vm ym gm, $3 cm vm ym gm, $5 cm vm ym gm) }
  | AND ebexp_atomic_without_eqmod ebexp_atomic_without_eqmod
                                                  { fun cm vm ym gm -> Eand ($2 cm vm ym gm, $3 cm vm ym gm) }
  | LPAR ebexp RPAR                               { fun cm vm ym gm -> $2 cm vm ym gm }
  | eexp EQOP eexp                                { fun cm vm ym gm -> Eeq ($1 cm vm ym gm, $3 cm vm ym gm) }
  | AND LSQUARE ebexps RSQUARE                    { fun cm vm ym gm -> eands ($3 cm vm ym gm) }
  | LANDOP LSQUARE ebexps RSQUARE                 { fun cm vm ym gm -> eands ($3 cm vm ym gm) }
  /* Errors */
/*  | ID error                                      { raise_at !lnum ("Invalid algebraic predicate after " ^ $1 ^ ".") }*/
;

eq_suffix:
                                                  { fun _cm _vm _ym _gm -> None }
  | LPAR MOD eexp RPAR                            { fun cm vm ym gm -> Some [ $3 cm vm ym gm ] }
  | LPAR MOD LSQUARE eexps RSQUARE RPAR           { fun cm vm ym gm -> Some ($4 cm vm ym gm) }
;

ebexps:
    ebexp COMMA ebexps                            { fun cm vm ym gm -> ($1 cm vm ym gm)::($3 cm vm ym gm) }
  | ebexp                                         { fun cm vm ym gm -> [$1 cm vm ym gm] }
  | ebexp error                                   { let lno = !lnum in
                                                    fun cm vm ym gm ->
                                                      raise_at lno ("Failed to parse the algebra predicate after '" ^ string_of_ebexp ($1 cm vm ym gm) ^ "'.")
                                                  }
;

cmpop_infix:
    ULTOP                                         { Rult }
  | ULEOP                                         { Rule }
  | UGTOP                                         { Rugt }
  | UGEOP                                         { Ruge }
  | SLTOP                                         { Rslt }
  | SLEOP                                         { Rsle }
  | SGTOP                                         { Rsgt }
  | SGEOP                                         { Rsge }
;

eexp:
    defined_var                                   { fun cm vm ym gm -> Evar (resolve_var_with !lnum $1 cm vm ym gm) }
  | simple_const                                  { fun cm _vm _ym _gm -> Econst ($1 cm) }
  | LPAR eexp RPAR                                { fun cm vm ym gm -> $2 cm vm ym gm }
  /* Extensions */
  | NEG eexp                                      { fun cm vm ym gm -> eneg ($2 cm vm ym gm) }
  | ADD eexp eexp_no_unary                        { fun cm vm ym gm -> eadd ($2 cm vm ym gm) ($3 cm vm ym gm) }
  | SUB eexp eexp_no_unary                        { fun cm vm ym gm -> esub ($2 cm vm ym gm) ($3 cm vm ym gm) }
  | MUL eexp eexp_no_unary                        { fun cm vm ym gm -> emul ($2 cm vm ym gm) ($3 cm vm ym gm) }
  | SQ eexp                                       { fun cm vm ym gm -> esq ($2 cm vm ym gm) }
  | ADD LSQUARE eexps RSQUARE                     { fun cm vm ym gm -> eadds ($3 cm vm ym gm) }
  | MUL LSQUARE eexps RSQUARE                     { fun cm vm ym gm -> emuls ($3 cm vm ym gm) }
  | SUBOP eexp %prec UMINUS                       { fun cm vm ym gm -> eneg ($2 cm vm ym gm) }
  | eexp ADDOP eexp                               { fun cm vm ym gm -> eadd ($1 cm vm ym gm) ($3 cm vm ym gm) }
  | eexp SUBOP eexp                               { fun cm vm ym gm -> esub ($1 cm vm ym gm) ($3 cm vm ym gm) }
  | eexp MULOP eexp                               { fun cm vm ym gm -> emul ($1 cm vm ym gm) ($3 cm vm ym gm) }
  | eexp POWOP const                              { fun cm vm ym gm ->
                                                      let e = $1 cm vm ym gm in
                                                      let i = Z.to_int ($3 cm) in
                                                      match e with
                                                      | Econst n -> Econst (Z.pow n i)
                                                      | _ ->
                                                        let rec helper j =
                                                          if j = 0 then Econst Z.one
                                                          else if j = 1 then e
                                                          else if j = 2 then esq e
                                                          else emul (helper (j - 1)) e in
                                                        helper i
                                                  }
  | ULIMBS const LSQUARE eexps RSQUARE            { fun cm vm ym gm -> limbs (Z.to_int ($2 cm)) ($4 cm vm ym gm) }
;

eexp_no_unarys:
    eexp_no_unary COMMA eexp_no_unarys            { fun cm vm ym gm -> ($1 cm vm ym gm)::($3 cm vm ym gm) }
  | eexp_no_unary                                 { fun cm vm ym gm -> [ $1 cm vm ym gm ] }
;

eexp_no_unary:
    defined_var                                   { fun cm vm ym gm -> Evar (resolve_var_with !lnum $1 cm vm ym gm) }
  | simple_const                                  { fun cm _vm _ym _gm -> Econst ($1 cm) }
  | LPAR eexp RPAR                                { fun cm vm ym gm -> $2 cm vm ym gm }
  /* Extensions */
  | NEG eexp                                      { fun cm vm ym gm -> eneg ($2 cm vm ym gm) }
  | ADD eexp eexp_no_unary                        { fun cm vm ym gm -> eadd ($2 cm vm ym gm) ($3 cm vm ym gm) }
  | SUB eexp eexp_no_unary                        { fun cm vm ym gm -> esub ($2 cm vm ym gm) ($3 cm vm ym gm) }
  | MUL eexp eexp_no_unary                        { fun cm vm ym gm -> emul ($2 cm vm ym gm) ($3 cm vm ym gm) }
  | SQ eexp                                       { fun cm vm ym gm -> esq ($2 cm vm ym gm) }
  | ADD LSQUARE eexps RSQUARE                     { fun cm vm ym gm -> eadds ($3 cm vm ym gm) }
  | MUL LSQUARE eexps RSQUARE                     { fun cm vm ym gm -> emuls ($3 cm vm ym gm) }
  | eexp_no_unary ADDOP eexp                      { fun cm vm ym gm -> eadd ($1 cm vm ym gm) ($3 cm vm ym gm) }
  | eexp_no_unary SUBOP eexp                      { fun cm vm ym gm -> esub ($1 cm vm ym gm) ($3 cm vm ym gm) }
  | eexp_no_unary MULOP eexp                      { fun cm vm ym gm -> emul ($1 cm vm ym gm) ($3 cm vm ym gm) }
  | eexp_no_unary POWOP const                     { fun cm vm ym gm ->
                                                      let e = $1 cm vm ym gm in
                                                      let i = Z.to_int ($3 cm) in
                                                      match e with
                                                      | Econst n -> Econst (Z.pow n i)
                                                      | _ ->
                                                        let rec helper j =
                                                          if j = 0 then Econst Z.one
                                                          else if j = 1 then e
                                                          else if j = 2 then esq e
                                                          else emul (helper (j - 1)) e in
                                                        helper i
                                                  }
  | ULIMBS const LSQUARE eexps RSQUARE            { fun cm vm ym gm -> limbs (Z.to_int ($2 cm)) ($4 cm vm ym gm) }
;

eexps:
    eexp COMMA eexps                              { fun cm vm ym gm -> ($1 cm vm ym gm)::($3 cm vm ym gm) }
  | eexp                                          { fun cm vm ym gm -> [$1 cm vm ym gm] }
  | VARS var_expansion                            { fun cm vm ym gm -> List.map (fun v -> Evar v) ($2 cm vm ym gm) }
;

rbexp:
    rbexp_and LOROP rbexp                         { fun cm vm ym gm -> Ror ($1 cm vm ym gm, $3 cm vm ym gm) }
  | rbexp_and                                     { fun cm vm ym gm -> $1 cm vm ym gm }
;

rbexp_and:
    rbexp_atomic LANDOP rbexp_and                 { fun cm vm ym gm -> Rand ($1 cm vm ym gm, $3 cm vm ym gm) }
  | rbexp_atomic                                  { fun cm vm ym gm -> $1 cm vm ym gm }
;

rbexp_atomic:
    TRUE                                          { fun _cm _vm _ym _gm -> Rtrue }
  | EQ rexp rexp                                  { let lno = !lnum in
                                                    fun cm vm ym gm ->
                                                      let e1 = $2 cm vm ym gm in
                                                      let e2 = $3 cm vm ym gm in
                                                      let w1 = size_of_rexp e1 in
                                                      let w2 = size_of_rexp e2 in
                                                      if w1 != w2 then raise_at lno ("Widths of range expressions mismatch: "
                                                                                     ^ string_of_rexp e1 ^ " (width " ^ string_of_int w1 ^ "), "
                                                                                     ^ string_of_rexp e2 ^ " (width " ^ string_of_int w2 ^ ")")
                                                      else Req (w1, e1, e2)
                                                  }
  | NEG rbexp_atomic                              { fun cm vm ym gm -> Rneg ($2 cm vm ym gm) }
  | NEGOP rbexp_atomic                            { fun cm vm ym gm -> Rneg ($2 cm vm ym gm) }
  | AND rbexp_atomic_without_eqmod rbexp_atomic
                                                  { fun cm vm ym gm -> Rand ($2 cm vm ym gm, $3 cm vm ym gm) }
  | OR rbexp_atomic_without_eqmod rbexp_atomic    { fun cm vm ym gm -> Ror ($2 cm vm ym gm, $3 cm vm ym gm) }
  | LPAR rbexp RPAR                               { fun cm vm ym gm -> $2 cm vm ym gm }
  /* Extensions */
  | rexp EQOP rexp req_suffix                     { let lno = !lnum in
                                                    fun cm vm ym gm ->
                                                      let e1 = $1 cm vm ym gm in
                                                      let e2 = $3 cm vm ym gm in
                                                      let w1 = size_of_rexp e1 in
                                                      let w2 = size_of_rexp e2 in
                                                      if w1 != w2 then raise_at lno ("Widths of range expressions mismatch: "
                                                                                     ^ string_of_rexp e1 ^ " (width " ^ string_of_int w1 ^ "), "
                                                                                     ^ string_of_rexp e2 ^ " (width " ^ string_of_int w2 ^ ")")
                                                      else
                                                        match $4 cm vm ym gm with
                                                        | None -> Req (w1, e1, e2)
                                                        | Some (f, m) ->
                                                          let wm = size_of_rexp m in
                                                          begin
                                                            if w1 != wm then raise_at lno ("Widths of range expressions mismatch: "
                                                                                           ^ string_of_rexp e1 ^ " (width " ^ string_of_int w1 ^ "), "
                                                                                           ^ string_of_rexp m ^ " (width " ^ string_of_int wm ^ ")")
                                                            else
                                                              f w1 e1 e2 m
                                                         end
                                                  }
  | rexp cmpop_infix rexp                         { let lno = !lnum in
                                                    fun cm vm ym gm ->
                                                      let e1 = $1 cm vm ym gm in
                                                      let e2 = $3 cm vm ym gm in
                                                      let op = $2 in
                                                      let w1 = size_of_rexp e1 in
                                                      let w2 = size_of_rexp e2 in
                                                      if w1 != w2 then raise_at lno ("Widths of range expressions mismatch: "
                                                                                     ^ string_of_rexp e1 ^ " (width " ^ string_of_int w1 ^ "), "
                                                                                     ^ string_of_rexp e2 ^ " (width " ^ string_of_int w2 ^ ")")
                                                      else Rcmp(w1, op, e1, e2)
                                                  }
  | cmpop_prefix rexp rexp                        { let lno = !lnum in
                                                    fun cm vm ym gm ->
                                                      let e1 = $2 cm vm ym gm in
                                                      let e2 = $3 cm vm ym gm in
                                                      let w1 = size_of_rexp e1 in
                                                      let w2 = size_of_rexp e2 in
                                                      if w1 != w2 then raise_at lno ("Widths of range expressions mismatch: "
                                                                                     ^ string_of_rexp e1 ^ " (width " ^ string_of_int w1 ^ "), "
                                                                                     ^ string_of_rexp e2 ^ " (width " ^ string_of_int w2 ^ ")")
                                                      else Rcmp (w1, $1, e1, e2)
                                                  }
  | AND LSQUARE rbexps RSQUARE                    { fun cm vm ym gm -> rands ($3 cm vm ym gm) }
  | LANDOP LSQUARE rbexps RSQUARE                 { fun cm vm ym gm -> rands ($3 cm vm ym gm) }
  | OR LSQUARE rbexps RSQUARE                     { fun cm vm ym gm -> rors ($3 cm vm ym gm) }
  | LOROP LSQUARE rbexps RSQUARE                  { fun cm vm ym gm -> rors ($3 cm vm ym gm) }
  | EQMOD rexp rexp rexp                          { let lno = !lnum in
                                                    fun cm vm ym gm ->
                                                      let e1 = $2 cm vm ym gm in
                                                      let e2 = $3 cm vm ym gm in
                                                      let m = $4 cm vm ym gm in
                                                      let w1 = size_of_rexp e1 in
                                                      let w2 = size_of_rexp e2 in
                                                      let wm = size_of_rexp m in
                                                      if w1 != w2 then raise_at lno ("Widths of range expressions mismatch: "
                                                                                     ^ string_of_rexp e1 ^ " (width " ^ string_of_int w1 ^ "), "
                                                                                     ^ string_of_rexp e2 ^ " (width " ^ string_of_int w2 ^ ")")
                                                      else if w1 != wm then raise_at lno ("Widths of range expressions mismatch: "
                                                                                     ^ string_of_rexp e1 ^ " (width " ^ string_of_int w1 ^ "), "
                                                                                     ^ string_of_rexp m ^ " (width " ^ string_of_int wm ^ ")")
                                                      else
                                                        reqmod w1 e1 e2 m
                                                  }
  | EQUMOD rexp rexp rexp                         { let lno = !lnum in
                                                    fun cm vm ym gm ->
                                                      let e1 = $2 cm vm ym gm in
                                                      let e2 = $3 cm vm ym gm in
                                                      let m = $4 cm vm ym gm in
                                                      let w1 = size_of_rexp e1 in
                                                      let w2 = size_of_rexp e2 in
                                                      let wm = size_of_rexp m in
                                                      if w1 != w2 then raise_at lno ("Widths of range expressions mismatch: "
                                                                                     ^ string_of_rexp e1 ^ " (width " ^ string_of_int w1 ^ "), "
                                                                                     ^ string_of_rexp e2 ^ " (width " ^ string_of_int w2 ^ ")")
                                                      else if w1 != wm then raise_at lno ("Widths of range expressions mismatch: "
                                                                                     ^ string_of_rexp e1 ^ " (width " ^ string_of_int w1 ^ "), "
                                                                                     ^ string_of_rexp m ^ " (width " ^ string_of_int wm ^ ")")
                                                      else
                                                        reqmod w1 e1 e2 m
                                                  }
  | EQSMOD rexp rexp rexp                         { let lno = !lnum in
                                                    fun cm vm ym gm ->
                                                      let e1 = $2 cm vm ym gm in
                                                      let e2 = $3 cm vm ym gm in
                                                      let m = $4 cm vm ym gm in
                                                      let w1 = size_of_rexp e1 in
                                                      let w2 = size_of_rexp e2 in
                                                      let wm = size_of_rexp m in
                                                      if w1 != w2 then raise_at lno ("Widths of range expressions mismatch: "
                                                                                     ^ string_of_rexp e1 ^ " (width " ^ string_of_int w1 ^ "), "
                                                                                     ^ string_of_rexp e2 ^ " (width " ^ string_of_int w2 ^ ")")
                                                      else if w1 != wm then raise_at lno ("Widths of range expressions mismatch: "
                                                                                     ^ string_of_rexp e1 ^ " (width " ^ string_of_int w1 ^ "), "
                                                                                     ^ string_of_rexp m ^ " (width " ^ string_of_int wm ^ ")")
                                                      else
                                                        reqsmod w1 e1 e2 m
                                                  }
  | EQSREM rexp rexp rexp                         { let lno = !lnum in
                                                    fun cm vm ym gm ->
                                                      let e1 = $2 cm vm ym gm in
                                                      let e2 = $3 cm vm ym gm in
                                                      let m = $4 cm vm ym gm in
                                                      let w1 = size_of_rexp e1 in
                                                      let w2 = size_of_rexp e2 in
                                                      let wm = size_of_rexp m in
                                                      if w1 != w2 then raise_at lno ("Widths of range expressions mismatch: "
                                                                                     ^ string_of_rexp e1 ^ " (width " ^ string_of_int w1 ^ "), "
                                                                                     ^ string_of_rexp e2 ^ " (width " ^ string_of_int w2 ^ ")")
                                                      else if w1 != wm then raise_at lno ("Widths of range expressions mismatch: "
                                                                                     ^ string_of_rexp e1 ^ " (width " ^ string_of_int w1 ^ "), "
                                                                                     ^ string_of_rexp m ^ " (width " ^ string_of_int wm ^ ")")
                                                      else
                                                        reqsrem w1 e1 e2 m
                                                  }
;

rbexp_atomic_without_eqmod:
    TRUE                                          { fun _cm _vm _ym _gm -> Rtrue }
  | EQ rexp rexp                                  { let lno = !lnum in
                                                    fun cm vm ym gm ->
                                                      let e1 = $2 cm vm ym gm in
                                                      let e2 = $3 cm vm ym gm in
                                                      let w1 = size_of_rexp e1 in
                                                      let w2 = size_of_rexp e2 in
                                                      if w1 != w2 then raise_at lno ("Widths of range expressions mismatch: "
                                                                                     ^ string_of_rexp e1 ^ " (width " ^ string_of_int w1 ^ "), "
                                                                                     ^ string_of_rexp e2 ^ " (width " ^ string_of_int w2 ^ ")")
                                                      else Req (w1, e1, e2)
                                                  }
  | NEG rbexp_atomic_without_eqmod                { fun cm vm ym gm -> Rneg ($2 cm vm ym gm) }
  | AND rbexp_atomic_without_eqmod rbexp_atomic_without_eqmod
                                                  { fun cm vm ym gm -> Rand ($2 cm vm ym gm, $3 cm vm ym gm) }
  | OR rbexp_atomic_without_eqmod rbexp_atomic_without_eqmod
                                                  { fun cm vm ym gm -> Ror ($2 cm vm ym gm, $3 cm vm ym gm) }
  | LPAR rbexp RPAR                               { fun cm vm ym gm -> $2 cm vm ym gm }
  /* Extensions */
  | rexp EQOP rexp                                { let lno = !lnum in
                                                    fun cm vm ym gm ->
                                                      let e1 = $1 cm vm ym gm in
                                                      let e2 = $3 cm vm ym gm in
                                                      let w1 = size_of_rexp e1 in
                                                      let w2 = size_of_rexp e2 in
                                                      if w1 != w2 then raise_at lno ("Widths of range expressions mismatch: "
                                                                                     ^ string_of_rexp e1 ^ " (width " ^ string_of_int w1 ^ "), "
                                                                                     ^ string_of_rexp e2 ^ " (width " ^ string_of_int w2 ^ ")")
                                                      else Req (w1, e1, e2)
                                                  }
  | rexp cmpop_infix rexp                         { let lno = !lnum in
                                                    fun cm vm ym gm ->
                                                      let e1 = $1 cm vm ym gm in
                                                      let e2 = $3 cm vm ym gm in
                                                      let op = $2 in
                                                      let w1 = size_of_rexp e1 in
                                                      let w2 = size_of_rexp e2 in
                                                      if w1 != w2 then raise_at lno ("Widths of range expressions mismatch: "
                                                                                     ^ string_of_rexp e1 ^ " (width " ^ string_of_int w1 ^ "), "
                                                                                     ^ string_of_rexp e2 ^ " (width " ^ string_of_int w2 ^ ")")
                                                      else Rcmp(w1, op, e1, e2) }
  | cmpop_prefix rexp rexp                        { let lno = !lnum in
                                                    fun cm vm ym gm ->
                                                      let e1 = $2 cm vm ym gm in
                                                      let e2 = $3 cm vm ym gm in
                                                      let w1 = size_of_rexp e1 in
                                                      let w2 = size_of_rexp e2 in
                                                      if w1 != w2 then raise_at lno ("Widths of range expressions mismatch: "
                                                                                     ^ string_of_rexp e1 ^ " (width " ^ string_of_int w1 ^ "), "
                                                                                     ^ string_of_rexp e2 ^ " (width " ^ string_of_int w2 ^ ")")
                                                      else Rcmp (w1, $1, e1, e2) }
  | AND LSQUARE rbexps RSQUARE                    { fun cm vm ym gm -> rands ($3 cm vm ym gm) }
  | OR LSQUARE rbexps RSQUARE                     { fun cm vm ym gm -> rors ($3 cm vm ym gm) }
  | EQMOD rexp rexp rexp                          { let lno = !lnum in
                                                    fun cm vm ym gm ->
                                                      let e1 = $2 cm vm ym gm in
                                                      let e2 = $3 cm vm ym gm in
                                                      let m = $4 cm vm ym gm in
                                                      let w1 = size_of_rexp e1 in
                                                      let w2 = size_of_rexp e2 in
                                                      let wm = size_of_rexp m in
                                                      if w1 != w2 then raise_at lno ("Widths of range expressions mismatch: "
                                                                                     ^ string_of_rexp e1 ^ " (width " ^ string_of_int w1 ^ "), "
                                                                                     ^ string_of_rexp e2 ^ " (width " ^ string_of_int w2 ^ ")")
                                                      else if w1 != wm then raise_at lno ("Widths of range expressions mismatch: "
                                                                                     ^ string_of_rexp e1 ^ " (width " ^ string_of_int w1 ^ "), "
                                                                                     ^ string_of_rexp m ^ " (width " ^ string_of_int wm ^ ")")
                                                      else
                                                        reqmod w1 e1 e2 m
                                                  }
  | EQUMOD rexp rexp rexp                         { let lno = !lnum in
                                                    fun cm vm ym gm ->
                                                      let e1 = $2 cm vm ym gm in
                                                      let e2 = $3 cm vm ym gm in
                                                      let m = $4 cm vm ym gm in
                                                      let w1 = size_of_rexp e1 in
                                                      let w2 = size_of_rexp e2 in
                                                      let wm = size_of_rexp m in
                                                      if w1 != w2 then raise_at lno ("Widths of range expressions mismatch: "
                                                                                     ^ string_of_rexp e1 ^ " (width " ^ string_of_int w1 ^ "), "
                                                                                     ^ string_of_rexp e2 ^ " (width " ^ string_of_int w2 ^ ")")
                                                      else if w1 != wm then raise_at lno ("Widths of range expressions mismatch: "
                                                                                     ^ string_of_rexp e1 ^ " (width " ^ string_of_int w1 ^ "), "
                                                                                     ^ string_of_rexp m ^ " (width " ^ string_of_int wm ^ ")")
                                                      else
                                                        reqmod w1 e1 e2 m
                                                  }
  | EQSMOD rexp rexp rexp                         { let lno = !lnum in
                                                    fun cm vm ym gm ->
                                                      let e1 = $2 cm vm ym gm in
                                                      let e2 = $3 cm vm ym gm in
                                                      let m = $4 cm vm ym gm in
                                                      let w1 = size_of_rexp e1 in
                                                      let w2 = size_of_rexp e2 in
                                                      let wm = size_of_rexp m in
                                                      if w1 != w2 then raise_at lno ("Widths of range expressions mismatch: "
                                                                                     ^ string_of_rexp e1 ^ " (width " ^ string_of_int w1 ^ "), "
                                                                                     ^ string_of_rexp e2 ^ " (width " ^ string_of_int w2 ^ ")")
                                                      else if w1 != wm then raise_at lno ("Widths of range expressions mismatch: "
                                                                                     ^ string_of_rexp e1 ^ " (width " ^ string_of_int w1 ^ "), "
                                                                                     ^ string_of_rexp m ^ " (width " ^ string_of_int wm ^ ")")
                                                      else
                                                        reqsmod w1 e1 e2 m
                                                  }
  | EQSREM rexp rexp rexp                         { let lno = !lnum in
                                                    fun cm vm ym gm ->
                                                      let e1 = $2 cm vm ym gm in
                                                      let e2 = $3 cm vm ym gm in
                                                      let m = $4 cm vm ym gm in
                                                      let w1 = size_of_rexp e1 in
                                                      let w2 = size_of_rexp e2 in
                                                      let wm = size_of_rexp m in
                                                      if w1 != w2 then raise_at lno ("Widths of range expressions mismatch: "
                                                                                     ^ string_of_rexp e1 ^ " (width " ^ string_of_int w1 ^ "), "
                                                                                     ^ string_of_rexp e2 ^ " (width " ^ string_of_int w2 ^ ")")
                                                      else if w1 != wm then raise_at lno ("Widths of range expressions mismatch: "
                                                                                     ^ string_of_rexp e1 ^ " (width " ^ string_of_int w1 ^ "), "
                                                                                     ^ string_of_rexp m ^ " (width " ^ string_of_int wm ^ ")")
                                                      else
                                                        reqsrem w1 e1 e2 m
                                                  }
;

req_suffix:
                                                  { fun _cm _vm _ym _gm -> None }
  | LPAR MOD rexp RPAR                            { fun cm vm ym gm -> Some (reqmod, $3 cm vm ym gm) }
  | LPAR UMOD rexp RPAR                           { fun cm vm ym gm -> Some (reqmod, $3 cm vm ym gm) }
  | LPAR SMOD rexp RPAR                           { fun cm vm ym gm -> Some (reqsmod, $3 cm vm ym gm) }
  | LPAR SREM rexp RPAR                           { fun cm vm ym gm -> Some (reqsrem, $3 cm vm ym gm) }
;

cmpop_prefix:
    ULT                                           { Rult }
  | ULE                                           { Rule }
  | UGT                                           { Rugt }
  | UGE                                           { Ruge }
  | SLT                                           { Rslt }
  | SLE                                           { Rsle }
  | SGT                                           { Rsgt }
  | SGE                                           { Rsge }
;

rbexps:
    rbexp COMMA rbexps                            { fun cm vm ym gm -> ($1 cm vm ym gm)::($3 cm vm ym gm) }
  | rbexp                                         { fun cm vm ym gm -> [$1 cm vm ym gm] }
  | rbexp COMMA error                             { raise_at !lnum ("Invalid range predicates.") }
  | rbexp error                                   { raise_at !lnum ("A ',' is used to separate range predicates") }
;

rexp:
    defined_var                                   { fun cm vm ym gm -> Rvar (resolve_var_with !lnum $1 cm vm ym gm) }
  | CONST const const                             { fun cm _vm _ym _gm ->
                                                      let w = Z.to_int ($2 cm) in
                                                      let n = $3 cm in
                                                      Rconst (w, n) }
  | const AT const                                { fun cm _vm _ym _gm ->
                                                      let w = Z.to_int ($3 cm) in
                                                      let n = $1 cm in
                                                      Rconst (w, n) }
  | UEXT rexp const                               { fun cm vm ym gm ->
                                                      let e = $2 cm vm ym gm in
                                                      let i = Z.to_int ($3 cm) in
                                                      let w = size_of_rexp e in
                                                      Ruext (w, e, i) }
  | SEXT rexp const                               { fun cm vm ym gm ->
                                                      let e = $2 cm vm ym gm in
                                                      let i = Z.to_int ($3 cm) in
                                                      let w = size_of_rexp e in
                                                      Rsext (w, e, i) }
  | LPAR rexp RPAR                                { fun cm vm ym gm -> $2 cm vm ym gm }
  /* Extensions */
  | NEG rexp                                      { fun cm vm ym gm ->
                                                      let e = $2 cm vm ym gm in
                                                      let w = size_of_rexp e in
                                                      Runop (w, Rnegb, e) }
  | NEGOP rexp                                    { fun cm vm ym gm ->
                                                      let e = $2 cm vm ym gm in
                                                      let w = size_of_rexp e in
                                                      Runop (w, Rnegb, e) }
  | NOT rexp                                      { fun cm vm ym gm ->
                                                      let e = $2 cm vm ym gm in
                                                      let w = size_of_rexp e in
                                                      Runop (w, Rnotb, e) }
  | NOTOP rexp                                    { fun cm vm ym gm ->
                                                      let e = $2 cm vm ym gm in
                                                      let w = size_of_rexp e in
                                                      Runop (w, Rnotb, e) }
  | ADD rexp rexp                                 { let lno = !lnum in
                                                    fun cm vm ym gm ->
                                                      let e1 = $2 cm vm ym gm in
                                                      let e2 = $3 cm vm ym gm in
                                                      let w1 = size_of_rexp e1 in
                                                      let w2 = size_of_rexp e2 in
                                                      if w1 != w2 then raise_at lno ("Widths of range expressions mismatch: "
                                                                                     ^ string_of_rexp e1 ^ " (width " ^ string_of_int w1 ^ "), "
                                                                                     ^ string_of_rexp e2 ^ " (width " ^ string_of_int w2 ^ ")")
                                                      else
                                                       radd w1 e1 e2 }
  | SUB rexp rexp                                 { let lno = !lnum in
                                                    fun cm vm ym gm ->
                                                      let e1 = $2 cm vm ym gm in
                                                      let e2 = $3 cm vm ym gm in
                                                      let w1 = size_of_rexp e1 in
                                                      let w2 = size_of_rexp e2 in
                                                      if w1 != w2 then raise_at lno ("Widths of range expressions mismatch: "
                                                                                     ^ string_of_rexp e1 ^ " (width " ^ string_of_int w1 ^ "), "
                                                                                     ^ string_of_rexp e2 ^ " (width " ^ string_of_int w2 ^ ")")
                                                      else
                                                       rsub w1 e1 e2 }
  | MUL rexp rexp                                 { let lno = !lnum in
                                                    fun cm vm ym gm ->
                                                      let e1 = $2 cm vm ym gm in
                                                      let e2 = $3 cm vm ym gm in
                                                      let w1 = size_of_rexp e1 in
                                                      let w2 = size_of_rexp e2 in
                                                      if w1 != w2 then raise_at lno ("Widths of range expressions mismatch: "
                                                                                     ^ string_of_rexp e1 ^ " (width " ^ string_of_int w1 ^ "), "
                                                                                     ^ string_of_rexp e2 ^ " (width " ^ string_of_int w2 ^ ")")
                                                      else
                                                       rmul w1 e1 e2 }
  | SQ rexp                                       { fun cm vm ym gm ->
                                                      let e = $2 cm vm ym gm in
                                                      rsq (size_of_rexp e) e }
  | UMOD rexp rexp                                { let lno = !lnum in
                                                    fun cm vm ym gm ->
                                                      let e1 = $2 cm vm ym gm in
                                                      let e2 = $3 cm vm ym gm in
                                                      let w1 = size_of_rexp e1 in
                                                      let w2 = size_of_rexp e2 in
                                                      if w1 != w2 then raise_at lno ("Widths of range expressions mismatch: "
                                                                                     ^ string_of_rexp e1 ^ " (width " ^ string_of_int w1 ^ "), "
                                                                                     ^ string_of_rexp e2 ^ " (width " ^ string_of_int w2 ^ ")")
                                                      else
                                                       rumod w1 e1 e2 }
  | SREM rexp rexp                                { let lno = !lnum in
                                                    fun cm vm ym gm ->
                                                      let e1 = $2 cm vm ym gm in
                                                      let e2 = $3 cm vm ym gm in
                                                      let w1 = size_of_rexp e1 in
                                                      let w2 = size_of_rexp e2 in
                                                      if w1 != w2 then raise_at lno ("Widths of range expressions mismatch: "
                                                                                     ^ string_of_rexp e1 ^ " (width " ^ string_of_int w1 ^ "), "
                                                                                     ^ string_of_rexp e2 ^ " (width " ^ string_of_int w2 ^ ")")
                                                      else
                                                       rsrem w1 e1 e2 }
  | SMOD rexp rexp                                { let lno = !lnum in
                                                    fun cm vm ym gm ->
                                                      let e1 = $2 cm vm ym gm in
                                                      let e2 = $3 cm vm ym gm in
                                                      let w1 = size_of_rexp e1 in
                                                      let w2 = size_of_rexp e2 in
                                                      if w1 != w2 then raise_at lno ("Widths of range expressions mismatch: "
                                                                                     ^ string_of_rexp e1 ^ " (width " ^ string_of_int w1 ^ "), "
                                                                                     ^ string_of_rexp e2 ^ " (width " ^ string_of_int w2 ^ ")")
                                                      else
                                                       rsmod w1 e1 e2 }
  | AND rexp rexp                                 { let lno = !lnum in
                                                    fun cm vm ym gm ->
                                                      let e1 = $2 cm vm ym gm in
                                                      let e2 = $3 cm vm ym gm in
                                                      let w1 = size_of_rexp e1 in
                                                      let w2 = size_of_rexp e2 in
                                                      if w1 != w2 then raise_at lno ("Widths of range expressions mismatch: "
                                                                                     ^ string_of_rexp e1 ^ " (width " ^ string_of_int w1 ^ "), "
                                                                                     ^ string_of_rexp e2 ^ " (width " ^ string_of_int w2 ^ ")")
                                                      else
                                                        Rbinop (w1, Randb, e1, e2) }
  | OR rexp rexp                                  { let lno = !lnum in
                                                    fun cm vm ym gm ->
                                                      let e1 = $2 cm vm ym gm in
                                                      let e2 = $3 cm vm ym gm in
                                                      let w1 = size_of_rexp e1 in
                                                      let w2 = size_of_rexp e2 in
                                                      if w1 != w2 then raise_at lno ("Widths of range expressions mismatch: "
                                                                                     ^ string_of_rexp e1 ^ " (width " ^ string_of_int w1 ^ "), "
                                                                                     ^ string_of_rexp e2 ^ " (width " ^ string_of_int w2 ^ ")")
                                                      else
                                                        Rbinop (w1, Rorb, e1, e2) }
  | XOR rexp rexp                                 { let lno = !lnum in
                                                    fun cm vm ym gm ->
                                                      let e1 = $2 cm vm ym gm in
                                                      let e2 = $3 cm vm ym gm in
                                                      let w1 = size_of_rexp e1 in
                                                      let w2 = size_of_rexp e2 in
                                                      if w1 != w2 then raise_at lno ("Widths of range expressions mismatch: "
                                                                                     ^ string_of_rexp e1 ^ " (width " ^ string_of_int w1 ^ "), "
                                                                                     ^ string_of_rexp e2 ^ " (width " ^ string_of_int w2 ^ ")")
                                                      else
                                                        Rbinop (w1, Rxorb, e1, e2) }

  | SHL rexp rexp                                 { let lno = !lnum in
                                                    fun cm vm ym gm ->
                                                      let e1 = $2 cm vm ym gm in
                                                      let e2 = $3 cm vm ym gm in
                                                      let w1 = size_of_rexp e1 in
                                                      let w2 = size_of_rexp e2 in
                                                      if w1 != w2 then raise_at lno ("Widths of range expressions mismatch: "
                                                                                     ^ string_of_rexp e1 ^ " (width " ^ string_of_int w1 ^ "), "
                                                                                     ^ string_of_rexp e2 ^ " (width " ^ string_of_int w2 ^ ")")
                                                      else
                                                        Rbinop (w1, Rshl, e1, e2) }
  | SHR rexp rexp                                 { let lno = !lnum in
                                                    fun cm vm ym gm ->
                                                      let e1 = $2 cm vm ym gm in
                                                      let e2 = $3 cm vm ym gm in
                                                      let w1 = size_of_rexp e1 in
                                                      let w2 = size_of_rexp e2 in
                                                      if w1 != w2 then raise_at lno ("Widths of range expressions mismatch: "
                                                                                     ^ string_of_rexp e1 ^ " (width " ^ string_of_int w1 ^ "), "
                                                                                     ^ string_of_rexp e2 ^ " (width " ^ string_of_int w2 ^ ")")
                                                      else
                                                        Rbinop (w1, Rlshr, e1, e2) }
  | SAR rexp rexp                                 { let lno = !lnum in
                                                    fun cm vm ym gm ->
                                                      let e1 = $2 cm vm ym gm in
                                                      let e2 = $3 cm vm ym gm in
                                                      let w1 = size_of_rexp e1 in
                                                      let w2 = size_of_rexp e2 in
                                                      if w1 != w2 then raise_at lno ("Widths of range expressions mismatch: "
                                                                                     ^ string_of_rexp e1 ^ " (width " ^ string_of_int w1 ^ "), "
                                                                                     ^ string_of_rexp e2 ^ " (width " ^ string_of_int w2 ^ ")")
                                                      else
                                                        Rbinop (w1, Rashr, e1, e2) }
  | ADD LSQUARE rexps RSQUARE                     { let lno = !lnum in
                                                    fun cm vm ym gm ->
                                                      let es = $3 cm vm ym gm in
                                                      match es with
                                                      | [] -> raise_at lno ("No range expression is passed to add.")
                                                      | hd::_tl -> radds (size_of_rexp hd) es }
  | MUL LSQUARE rexps RSQUARE                     { let lno = !lnum in
                                                    fun cm vm ym gm ->
                                                      let es = $3 cm vm ym gm in
                                                      match es with
                                                      | [] -> raise_at lno ("No range expression is passed to mul.")
                                                      | hd::_tl -> rmuls (size_of_rexp hd) es }
  | ULIMBS const LSQUARE rexps RSQUARE            { fun cm vm ym gm ->
                                                      let w = Z.to_int ($2 cm) in
                                                      let es = $4 cm vm ym gm in
                                                      let tw = List.fold_left (fun w1 w2 -> max w1 w2)
                                                                              0
                                                                              (List.mapi (fun i e -> size_of_rexp e + i * w) es) in
                                                      let es = List.map (fun e ->
                                                                          let ew = size_of_rexp e in
                                                                          Ruext (ew, e, tw - ew)) es in
                                                      let rec helper i es =
                                                        match es with
                                                        | [] -> Rconst (tw, Z.zero)
                                                        | hd::[] -> rmul tw hd (Rconst (tw, Z.pow z_two (i*w)))
                                                        | hd::tl -> radd tw (rmul tw hd (Rconst (tw, Z.pow z_two (i*w)))) (helper (i+1) tl) in
                                                      let res = helper 0 es in
                                                      res }
  | SLIMBS const LSQUARE rexps RSQUARE            { fun cm vm ym gm ->
                                                      let w = Z.to_int ($2 cm) in
                                                      let es = $4 cm vm ym gm in
                                                      let tw = List.fold_left (fun w1 w2 -> max w1 w2)
                                                                              0
                                                                              (List.mapi (fun i e -> size_of_rexp e + i * w) es) in
                                                      let last = List.length es - 1 in
                                                      let es = List.mapi (fun i e ->
                                                                            let ew = size_of_rexp e in
                                                                            if i == last then Rsext (ew, e, tw - ew)
                                                                            else Ruext (ew, e, tw - ew)) es in
                                                      let rec helper i es =
                                                        match es with
                                                        | [] -> Rconst (tw, Z.zero)
                                                        | hd::[] -> rmul tw hd (Rconst (tw, Z.pow z_two (i*w)))
                                                        | hd::tl -> radd tw (rmul tw hd (Rconst (tw, Z.pow z_two (i*w)))) (helper (i+1) tl) in
                                                      let res = helper 0 es in
                                                      res }
  | rexp ADDOP rexp                               { let lno = !lnum in
                                                    fun cm vm ym gm ->
                                                      let e1 = $1 cm vm ym gm in
                                                      let e2 = $3 cm vm ym gm in
                                                      let w1 = size_of_rexp e1 in
                                                      let w2 = size_of_rexp e2 in
                                                      if w1 != w2 then raise_at lno ("Widths of range expressions mismatch: "
                                                                                     ^ string_of_rexp e1 ^ " (width " ^ string_of_int w1 ^ "), "
                                                                                     ^ string_of_rexp e2 ^ " (width " ^ string_of_int w2 ^ ")")
                                                      else
                                                        Rbinop (w1, Radd, e1, e2) }
  | rexp SUBOP rexp                               { let lno = !lnum in
                                                    fun cm vm ym gm ->
                                                      let e1 = $1 cm vm ym gm in
                                                      let e2 = $3 cm vm ym gm in
                                                      let w1 = size_of_rexp e1 in
                                                      let w2 = size_of_rexp e2 in
                                                      if w1 != w2 then raise_at lno ("Widths of range expressions mismatch: "
                                                                                     ^ string_of_rexp e1 ^ " (width " ^ string_of_int w1 ^ "), "
                                                                                     ^ string_of_rexp e2 ^ " (width " ^ string_of_int w2 ^ ")")
                                                      else
                                                        Rbinop (w1, Rsub, e1, e2) }
  | rexp MULOP rexp                               { let lno = !lnum in
                                                    fun cm vm ym gm ->
                                                      let e1 = $1 cm vm ym gm in
                                                      let e2 = $3 cm vm ym gm in
                                                      let w1 = size_of_rexp e1 in
                                                      let w2 = size_of_rexp e2 in
                                                      if w1 != w2 then raise_at lno ("Widths of range expressions mismatch: "
                                                                                     ^ string_of_rexp e1 ^ " (width " ^ string_of_int w1 ^ "), "
                                                                                     ^ string_of_rexp e2 ^ " (width " ^ string_of_int w2 ^ ")")
                                                      else
                                                        Rbinop (w1, Rmul, e1, e2) }
  | rexp ANDOP rexp                               { let lno = !lnum in
                                                    fun cm vm ym gm ->
                                                      let e1 = $1 cm vm ym gm in
                                                      let e2 = $3 cm vm ym gm in
                                                      let w1 = size_of_rexp e1 in
                                                      let w2 = size_of_rexp e2 in
                                                      if w1 != w2 then raise_at lno ("Widths of range expressions mismatch: "
                                                                                     ^ string_of_rexp e1 ^ " (width " ^ string_of_int w1 ^ "), "
                                                                                     ^ string_of_rexp e2 ^ " (width " ^ string_of_int w2 ^ ")")
                                                      else
                                                        Rbinop (w1, Randb, e1, e2) }
  | rexp OROP rexp                                { let lno = !lnum in
                                                    fun cm vm ym gm ->
                                                      let e1 = $1 cm vm ym gm in
                                                      let e2 = $3 cm vm ym gm in
                                                      let w1 = size_of_rexp e1 in
                                                      let w2 = size_of_rexp e2 in
                                                      if w1 != w2 then raise_at lno ("Widths of range expressions mismatch: "
                                                                                     ^ string_of_rexp e1 ^ " (width " ^ string_of_int w1 ^ "), "
                                                                                     ^ string_of_rexp e2 ^ " (width " ^ string_of_int w2 ^ ")")
                                                      else
                                                        Rbinop (w1, Rorb, e1, e2) }
  | rexp XOROP rexp                               { let lno = !lnum in
                                                    fun cm vm ym gm ->
                                                      let e1 = $1 cm vm ym gm in
                                                      let e2 = $3 cm vm ym gm in
                                                      let w1 = size_of_rexp e1 in
                                                      let w2 = size_of_rexp e2 in
                                                      if w1 != w2 then raise_at lno ("Widths of range expressions mismatch: "
                                                                                     ^ string_of_rexp e1 ^ " (width " ^ string_of_int w1 ^ "), "
                                                                                     ^ string_of_rexp e2 ^ " (width " ^ string_of_int w2 ^ ")")
                                                      else
                                                        Rbinop (w1, Rxorb, e1, e2) }
  | rexp SHLOP rexp                               { let lno = !lnum in
                                                    fun cm vm ym gm ->
                                                      let e1 = $1 cm vm ym gm in
                                                      let e2 = $3 cm vm ym gm in
                                                      let w1 = size_of_rexp e1 in
                                                      let w2 = size_of_rexp e2 in
                                                      if w1 != w2 then raise_at lno ("Widths of range expressions mismatch: "
                                                                                     ^ string_of_rexp e1 ^ " (width " ^ string_of_int w1 ^ "), "
                                                                                     ^ string_of_rexp e2 ^ " (width " ^ string_of_int w2 ^ ")")
                                                      else
                                                        Rbinop (w1, Rshl, e1, e2) }
  | rexp SHROP rexp                               { let lno = !lnum in
                                                    fun cm vm ym gm ->
                                                      let e1 = $1 cm vm ym gm in
                                                      let e2 = $3 cm vm ym gm in
                                                      let w1 = size_of_rexp e1 in
                                                      let w2 = size_of_rexp e2 in
                                                      if w1 != w2 then raise_at lno ("Widths of range expressions mismatch: "
                                                                                     ^ string_of_rexp e1 ^ " (width " ^ string_of_int w1 ^ "), "
                                                                                     ^ string_of_rexp e2 ^ " (width " ^ string_of_int w2 ^ ")")
                                                      else
                                                        Rbinop (w1, Rlshr, e1, e2) }
  | rexp SAROP rexp                               { let lno = !lnum in
                                                    fun cm vm ym gm ->
                                                      let e1 = $1 cm vm ym gm in
                                                      let e2 = $3 cm vm ym gm in
                                                      let w1 = size_of_rexp e1 in
                                                      let w2 = size_of_rexp e2 in
                                                      if w1 != w2 then raise_at lno ("Widths of range expressions mismatch: "
                                                                                     ^ string_of_rexp e1 ^ " (width " ^ string_of_int w1 ^ "), "
                                                                                     ^ string_of_rexp e2 ^ " (width " ^ string_of_int w2 ^ ")")
                                                      else
                                                        Rbinop (w1, Rashr, e1, e2) }
  /* Errors */
  | CONST const error                             { raise_at !lnum "Please specify the bit-width of a constant in range predicates" }
  | const error                                   { raise_at !lnum "Please specify the bit-width of a constant in range predicates" }
;

rexps:
    rexp COMMA rexps
  {
    let lno = !lnum in
    fun cm vm ym gm ->
      let e = $1 cm vm ym gm in
      let es = $3 cm vm ym gm in
      if List.length es > 0 && size_of_rexp e != size_of_rexp (List.hd es) then
        raise_at lno ("Widths of range expressions mismatch: "
                      ^ string_of_rexp e ^ " (width " ^ string_of_int (size_of_rexp e) ^ ")"
                      ^ ", "
                      ^ string_of_rexp (List.hd es) ^ " (width " ^ string_of_int (size_of_rexp (List.hd es)) ^ ")")
      else
        e::es
  }
  | rexp                                          { fun cm vm ym gm -> [$1 cm vm ym gm] }
  | VARS var_expansion                            { fun cm vm ym gm -> List.map (fun v -> Rvar v) ($2 cm vm ym gm) }
;

lval:
    ID                                            { `LVPLAIN { lvname = $1; lvtyphint = None; } }
  | ID AT typ                                     { `LVPLAIN { lvname = $1; lvtyphint = Some $3; } }
  | typ ID                                        { `LVPLAIN { lvname = $2; lvtyphint = Some $1; } }
  | ID AT error                                   { raise_at !lnum ("Invalid type of variable " ^ $1) }
;

lcarry:
    ID                                            { `LVCARRY { lvname = $1; lvtyphint = None; } }
  | ID AT typ                                     { `LVCARRY { lvname = $1; lvtyphint = Some $3; } }
  | typ ID                                        { `LVCARRY { lvname = $2; lvtyphint = Some $1; } }
;

lval_or_lcarry:
    ID                                            { `LV { lvname = $1; lvtyphint = None; } }
  | ID AT typ                                     { `LV { lvname = $1; lvtyphint = Some $3; } }
  | typ ID                                        { `LV { lvname = $2; lvtyphint = Some $1; } } 
;

lhs:
    ID                                            { { lvname = $1; lvtyphint = None; } }
  | ID AT typ                                     { { lvname = $1; lvtyphint = Some $3; } }
  | typ ID                                        { { lvname = $2; lvtyphint = Some $1; } }
  | ID AT error                                   { raise_at !lnum ("Invalid type of variable " ^ $1) }
;

actuals:
    actual_atomics                                { fun tys cm vm ym gm -> $1 tys cm vm ym gm }
  |                                               { fun _tys _cm _vm _ym _gm -> [] }
;

actual_atomics:
    actual_atomic                                 { fun tys cm vm ym gm ->
                                                      let (_tys, vs) = $1 tys cm vm ym gm in
                                                      vs
                                                  }
  | actual_atomic COMMA actual_atomics            { fun tys cm vm ym gm ->
                                                      let (tys, vs) = $1 tys cm vm ym gm in
                                                      vs@($3 tys cm vm ym gm)
                                                  }
;

/* We don't check if the actual variables are defined or not because they may just be variable names of procedure outputs. */
actual_atomic:
    const                                         { let lno = !lnum in
                                                    fun tys cm vm ym gm ->
                                                      match tys with
                                                      | (ty::argtys, outtys) ->
                                                         ((argtys, outtys), [parse_typed_const lno ty $1 cm vm ym gm])
                                                      | ([], _ty::_) -> raise_at lno ("The corresponding formal parameter is an output variable. "
                                                                                     ^ "The actual parameter must be a variable.")
                                                      | _ -> raise_at lno ("The number of actual parameters does not match the number of formal parameters.") }
  | const AT typ                                  { let lno = !lnum in
                                                    fun tys cm vm ym gm ->
                                                      match tys with
                                                      | (ty::argtys, outtys) ->
                                                         if ty = $3 then ((argtys, outtys), [parse_typed_const lno $3 $1 cm vm ym gm])
                                                         else raise_at lno ("The specified type is not compatible to the type of the corresponding formal parameter")
                                                      | ([], _ty::_) -> raise_at lno ("The corresponding formal parameter is an output variable. "
                                                                                     ^ "The actual parameter must be a variable.")
                                                      | _ -> raise_at lno ("The number of actual parameters does not match the number of formal parameters.") }
  | typ const                                     { let lno = !lnum in
                                                    fun tys cm vm ym gm ->
                                                      match tys with
                                                      | (ty::argtys, outtys) ->
                                                         if ty = $1 then ((argtys, outtys), [parse_typed_const lno $1 $2 cm vm ym gm])
                                                         else raise_at lno ("The specified type is not compatible to the type of the corresponding formal parameter")
                                                      | ([], _ty::_) -> raise_at lno ("The corresponding formal parameter is an output variable. "
                                                                                     ^ "The actual parameter must be a variable.")
                                                      | _ -> raise_at lno ("The number of actual parameters does not match the number of formal parameters.") }
  | ID                                            { let lno = !lnum in
                                                    fun tys _cm vm _ym _gm ->
                                                      match tys with
                                                      | (ty::argtys, outtys) ->
                                                         (try
                                                            let v = SM.find $1 vm in
                                                            if v.vtyp = ty then ((argtys, outtys), [Avar v])
                                                            else raise_at lno ("The variable type of "
                                                                               ^ $1
                                                                               ^ " is not compatible to the type of the corresponding formal parameter")
                                                          with Not_found ->
                                                            raise_at lno ("Failed to determine the type of " ^ $1)
                                                         )
                                                      | ([], ty::outtys) -> (([], outtys), [Avar (mkvar $1 ty)])
                                                      | _ -> raise_at lno ("The number of actual parameters does not match the number of formal parameters.")
                                                  }
  | ID OROP NUM DOTDOT NUM                        { let lno = !lnum in
                                                      fun tys _cm vm _ym _gm ->
                                                      let prefix = $1 in
                                                      let st = $3 in
                                                      let ed = $5 in
                                                      let (tys, vars_rev) =
                                                        List.fold_left (fun (tys, vars_rev) i ->
                                                                         let vname = prefix ^ vars_expansion_infix ^ string_of_int i in
                                                                         match tys with
                                                                         | (ty::argtys, outtys) ->
                                                                            (try
                                                                               let v = SM.find vname vm in
                                                                               if v.vtyp = ty then ((argtys, outtys), (Avar v)::vars_rev)
                                                                               else raise_at lno ("The variable type of "
                                                                                                  ^ vname
                                                                                                  ^ " is not compatible to the type of the corresponding formal parameter")
                                                                             with Not_found ->
                                                                               raise_at lno ("Failed to determine the type of " ^ vname)
                                                                            )
                                                                         | ([], ty::outtys) -> (([], outtys), (Avar (mkvar vname ty))::vars_rev)
                                                                         | _ -> raise_at lno ("The number of actual parameters does not match the number of formal parameters.")
                                                                       )
                                                                       (tys, [])
                                                                       ((Z.to_int st)--(Z.to_int ed)) in
                                                      (tys, List.rev vars_rev)
                                                  }
;

atomic:
    const AT typ                                  { `ACONST { atmtyp = $3; atmvalue = $1; } }
  | typ const                                     { `ACONST { atmtyp = $1; atmvalue = $2; } }
  | defined_var                                   { ($1 :> atomic_t) }
  /*| LPAR atomic RPAR                              { fun cm vm ym gm -> $2 cm vm ym gm } source of reduce/reduce conflict*/
;

var_expansion:
  ID OROP NUM DOTDOT NUM
  {
    let lno = !lnum in
    fun _cm vm _ym gm ->
    let prefix = $1 in
    let st = $3 in
    let ed = $5 in
    let res = List.map
                (fun i ->
                  let vname = prefix ^ vars_expansion_infix ^ string_of_int i in
                  try
                    SM.find vname vm
                  with Not_found ->
                    raise_at lno ("Failed to determine the type of " ^ vname)
                ) ((Z.to_int st)--(Z.to_int ed)) in
    let _ = List.iter (fun v -> if not (SM.mem v.vname vm) && not (SM.mem v.vname gm) then raise_at lno ("Variable " ^ string_of_var v ^ " is not defined.")) res in
    res
  }
;

defined_var:
    ID                                            { `AVAR { atmtyphint = None;    atmname = $1; } }
  | ID AT typ                                     { `AVAR { atmtyphint = Some $3; atmname = $1; } }
  | typ ID                                        { `AVAR { atmtyphint = Some $1; atmname = $2; } }
;

gvars:
  gvar                                            { fun cm vm ym gm -> VS.singleton ($1 cm vm ym gm) }
| gvar COMMA gvars                                { fun cm vm ym gm -> VS.add ($1 cm vm ym gm) ($3 cm vm ym gm) }
| gvar error                                      {
                                                    let lno = !lnum in
                                                    fun _cm _vm _ym _gm ->
                                                      raise_at lno ("A comma is used to separate ghost variables.")
                                                  }
| error                                           {
                                                    let lno = !lnum in
                                                    fun _cm _vm _ym _gm ->
                                                      raise_at lno ("Invalid ghost variable.")
                                                  }
;

gvar:
  typ ID                                          {
                                                    let lno = !lnum in
                                                    fun _cm vm _ym gm ->
                                                      let ty = $1 in
                                                      let vname = $2 in
                                                      if SM.mem vname vm then raise_at lno ("The ghost variable " ^ vname ^ " has been defined as a program variable.")
                                                      else if SM.mem vname gm then raise_at lno ("The ghost variable " ^ vname ^ " has been defined previously.")
                                                      else mkvar vname ty
                                                  }
| ID AT typ                                       {
                                                    let lno = !lnum in
                                                    fun _cm vm _ym gm ->
                                                      let ty = $3 in
                                                      let vname = $1 in
                                                      if SM.mem vname vm then raise_at lno ("The ghost variable " ^ vname ^ " has been defined as a program variable.")
                                                      else if SM.mem vname gm then raise_at lno ("The ghost variable " ^ vname ^ " has been defined previously.")
                                                      else mkvar vname ty
                                                  }
;

const:
    simple_const                                  { fun cm -> $1 cm }
  | LPAR complex_const RPAR                       { fun cm -> ($2 cm) }
/*  | ID error                                      { raise_at !lnum ("A constant is expected but '" ^ $1 ^ "' is encountered.") }*/
;

simple_const:
    NUM                                           { fun _cm -> $1 }
  | DEREFOP ID                                    { let lno = !lnum in
                                                    fun cm ->
                                                      try
                                                        SM.find $2 cm
                                                      with Not_found ->
                                                        raise_at lno ("Undefined constant: " ^ $2) }
;

complex_const:
    const                                         { fun cm -> $1 cm }
  | SUBOP complex_const %prec UMINUS              { fun cm -> Z.neg ($2 cm) }
  | complex_const ADDOP complex_const             { fun cm -> Z.add ($1 cm) ($3 cm) }
  | complex_const SUBOP complex_const             { fun cm -> Z.sub ($1 cm) ($3 cm) }
  | complex_const MULOP complex_const             { fun cm -> Z.mul ($1 cm) ($3 cm) }
  | complex_const POWOP complex_const             { fun cm -> Z.pow ($1 cm) (Z.to_int ($3 cm)) }
;

carry:
    atomic                                        { $1 }
;

typ:
    UINT                                          { if $1 > 0 then uint_t $1
                                                    else raise_at !lnum ("The big-with must be positive") }
  | SINT                                          { if $1 > 0 then int_t $1
                                                    else raise_at !lnum ("The big-with must be positive") }
  | BIT                                           { bit_t }
;
