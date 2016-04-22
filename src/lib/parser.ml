exception Error

type token = 
  | ZDD
  | XOR
  | VERTREFL
  | UNION
  | TRUE
  | TIMING
  | TILES3
  | TILES
  | SYM
  | SVG_OUT
  | STRING of (string)
  | SOLVE3
  | SOLVE
  | SHIFT
  | SET
  | SAT
  | RSBRA
  | RPAR
  | ROT90
  | ROT270
  | ROT180
  | ROT
  | RESIZE
  | PROBLEM3
  | PROBLEM
  | PRINT
  | PATTERN3
  | PATTERN
  | ONE
  | ON
  | OFF
  | MINUS
  | MAYBE
  | LSBRA
  | LPAR
  | INTER
  | INCLUDE
  | IDENT of (string)
  | ID
  | HORIZREFL
  | HAT
  | H2G2
  | FALSE
  | EXIT
  | EQUAL
  | EOF
  | DLX
  | DIMACS
  | DIM of (int * int)
  | DIFF
  | DIAG2REFL
  | DIAG1REFL
  | DEBUG
  | CROP
  | COUNT3
  | COUNT
  | CONSTANT
  | COMMA
  | BARBAR
  | ASSERT
  | ASCII_OUT
  | ASCII of (bool array array)
  | APPLY
  | AMPAMP
  | ALL3
  | ALL

and _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  mutable _menhir_token: token;
  mutable _menhir_startp: Lexing.position;
  mutable _menhir_endp: Lexing.position;
  mutable _menhir_shifted: int
}

and _menhir_state = 
  | MenhirState162
  | MenhirState156
  | MenhirState153
  | MenhirState150
  | MenhirState149
  | MenhirState148
  | MenhirState147
  | MenhirState142
  | MenhirState139
  | MenhirState137
  | MenhirState129
  | MenhirState128
  | MenhirState123
  | MenhirState122
  | MenhirState118
  | MenhirState111
  | MenhirState110
  | MenhirState104
  | MenhirState103
  | MenhirState99
  | MenhirState96
  | MenhirState94
  | MenhirState89
  | MenhirState83
  | MenhirState81
  | MenhirState72
  | MenhirState67
  | MenhirState65
  | MenhirState63
  | MenhirState62
  | MenhirState61
  | MenhirState60
  | MenhirState58
  | MenhirState56
  | MenhirState55
  | MenhirState53
  | MenhirState51
  | MenhirState50
  | MenhirState49
  | MenhirState48
  | MenhirState47
  | MenhirState46
  | MenhirState44
  | MenhirState42
  | MenhirState40
  | MenhirState38
  | MenhirState37
  | MenhirState36
  | MenhirState22
  | MenhirState20
  | MenhirState17
  | MenhirState15
  | MenhirState14
  | MenhirState13
  | MenhirState12
  | MenhirState11
  | MenhirState10
  | MenhirState9
  | MenhirState8
  | MenhirState7
  | MenhirState1
  | MenhirState0

  
  open Ast
  open Tiling
  open D4
  type option =
    | M of multiplicity
    | S of symmetries

let _eRR =
  Error

let rec _menhir_goto_separated_nonempty_list_COMMA_tile_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.tile list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (Ast.tile list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_tile_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (Ast.tile list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_tile__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_src_lib_parser_option_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (option list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (option list) =     ( x :: xs ) in
        _menhir_goto_list_src_lib_parser_option_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let o = _v in
        let (_menhir_stack, _menhir_s, e, _startpos_e_, _endpos_e_) = _menhir_stack in
        let _v : (Ast.tile) =     ( let option (s, m) = function
        | M m' -> s, m' (* FIXME: fail on ambiguity *)
	| S s' -> s', m (* idem *)
      in
      let s,m = List.fold_left option (Snone, Minf) o in e,s,m ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | APPLY ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp
            | ASCII _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | CONSTANT ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp
            | CROP ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp
            | DIFF ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp
            | IDENT _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INTER ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp
            | LPAR ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp
            | RESIZE ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp
            | SET ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp
            | SHIFT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp
            | UNION ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp
            | XOR ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65)
        | RSBRA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : (Ast.tile list) =     ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_COMMA_tile_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_src_lib_parser_option : _menhir_env -> 'ttv_tail -> _menhir_state -> (option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | MAYBE ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | ONE ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | ROT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | SYM ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | COMMA | RSBRA ->
        _menhir_reduce58 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72

and _menhir_goto_tiles : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.tiles) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _endpos ->
    match _menhir_s with
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let tl = _v in
        let _endpos_tl_ = _endpos in
        let (((_menhir_stack, _menhir_s, _startpos__1_), id, _startpos_id_, _endpos_id_), _, e, _startpos_e_, _endpos_e_) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_tl_ in
        let _v : (Ast.decl) =     ({decl_pos = (_startpos, _endpos);
      decl_node = Problem3 (id, e, tl)}) in
        _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let tl = _v in
        let _endpos_tl_ = _endpos in
        let (((_menhir_stack, _menhir_s, _startpos__1_), id, _startpos_id_, _endpos_id_), _, e, _startpos_e_, _endpos_e_) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_tl_ in
        let _v : (Ast.decl) =     ({decl_pos = (_startpos, _endpos);
      decl_node = Problem (id, e, tl)}) in
        _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_boolean_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.bool_expr) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _endpos ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let b = _v in
    let _endpos_b_ = _endpos in
    let (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos_b_ in
    let _v : (Ast.decl) =     ({decl_pos = (_startpos, _endpos);
      decl_node = Assert b}) in
    _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_separated_nonempty_list_COMMA_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (Ast.expr list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let ((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _) = _menhir_stack in
        let _v : (Ast.expr list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run105 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let id = _v in
    let _startpos_id_ = _startpos in
    let _endpos_id_ = _endpos in
    let _endpos = _endpos_id_ in
    let _v : (Ast.tiles) =                 ( Tiles_id  id ) in
    _menhir_goto_tiles _menhir_env _menhir_stack _menhir_s _v _endpos

and _menhir_reduce58 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (option list) =     ( [] ) in
    _menhir_goto_list_src_lib_parser_option_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run68 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (option) =         ( S Sall ) in
    _menhir_goto_src_lib_parser_option _menhir_env _menhir_stack _menhir_s _v

and _menhir_run69 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (option) =         ( S Spositive ) in
    _menhir_goto_src_lib_parser_option _menhir_env _menhir_stack _menhir_s _v

and _menhir_run70 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (option) =         ( M Mone ) in
    _menhir_goto_src_lib_parser_option _menhir_env _menhir_stack _menhir_s _v

and _menhir_run71 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (option) =         ( M Mmaybe ) in
    _menhir_goto_src_lib_parser_option _menhir_env _menhir_stack _menhir_s _v

and _menhir_run38 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | APPLY ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _menhir_env._menhir_startp
    | ASCII _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | CONSTANT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _menhir_env._menhir_startp
    | CROP ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _menhir_env._menhir_startp
    | DIFF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _menhir_env._menhir_startp
    | IDENT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTER ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _menhir_env._menhir_startp
    | RESIZE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _menhir_env._menhir_startp
    | SET ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _menhir_env._menhir_startp
    | SHIFT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _menhir_env._menhir_startp
    | UNION ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _menhir_env._menhir_startp
    | XOR ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38

and _menhir_run40 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | APPLY ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _menhir_env._menhir_startp
    | ASCII _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | CONSTANT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _menhir_env._menhir_startp
    | CROP ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _menhir_env._menhir_startp
    | DIFF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _menhir_env._menhir_startp
    | IDENT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTER ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _menhir_env._menhir_startp
    | RESIZE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _menhir_env._menhir_startp
    | SET ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _menhir_env._menhir_startp
    | SHIFT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _menhir_env._menhir_startp
    | UNION ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _menhir_env._menhir_startp
    | XOR ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40

and _menhir_run42 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | APPLY ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _menhir_env._menhir_startp
    | ASCII _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | CONSTANT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _menhir_env._menhir_startp
    | CROP ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _menhir_env._menhir_startp
    | DIFF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _menhir_env._menhir_startp
    | IDENT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTER ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _menhir_env._menhir_startp
    | RESIZE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _menhir_env._menhir_startp
    | SET ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _menhir_env._menhir_startp
    | SHIFT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _menhir_env._menhir_startp
    | UNION ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _menhir_env._menhir_startp
    | XOR ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42

and _menhir_run44 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | APPLY ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _menhir_env._menhir_startp
    | ASCII _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | CONSTANT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _menhir_env._menhir_startp
    | CROP ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _menhir_env._menhir_startp
    | DIFF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _menhir_env._menhir_startp
    | IDENT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTER ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _menhir_env._menhir_startp
    | RESIZE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _menhir_env._menhir_startp
    | SET ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _menhir_env._menhir_startp
    | SHIFT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _menhir_env._menhir_startp
    | UNION ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _menhir_env._menhir_startp
    | XOR ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_loption_separated_nonempty_list_COMMA_tile__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.tile list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RSBRA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_endp in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos__3_ = _endpos in
        let ((_menhir_stack, _menhir_s), _, xs0) = _menhir_stack in
        let _endpos = _endpos__3_ in
        let _v : (Ast.tile list) = let l =
          let xs = xs0 in
              ( xs )
        in
                                                        ( l ) in
        (match _menhir_s with
        | MenhirState7 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let l = _v in
            let _endpos_l_ = _endpos in
            let ((_menhir_stack, _menhir_s, _startpos__1_), id, _startpos_id_, _endpos_id_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_l_ in
            let _v : (Ast.decl) =     ({decl_pos = (_startpos, _endpos);
      decl_node = Tiles3 (id, l)}) in
            _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v
        | MenhirState81 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let l = _v in
            let _endpos_l_ = _endpos in
            let ((_menhir_stack, _menhir_s, _startpos__1_), id, _startpos_id_, _endpos_id_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_l_ in
            let _v : (Ast.decl) =     ({decl_pos = (_startpos, _endpos);
      decl_node = Tiles (id, l)}) in
            _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v
        | MenhirState111 | MenhirState104 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let l = _v in
            let _endpos_l_ = _endpos in
            let _endpos = _endpos_l_ in
            let _v : (Ast.tiles) =                 ( Tiles_list l ) in
            _menhir_goto_tiles _menhir_env _menhir_stack _menhir_s _v _endpos
        | _ ->
            _menhir_fail ())
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_output : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.output) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _endpos ->
    match _menhir_s with
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let out = _v in
        let _endpos_out_ = _endpos in
        let (((_menhir_stack, _menhir_s, _startpos__1_), _, a), id, _startpos_id_, _endpos_id_) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_out_ in
        let _v : (Ast.decl) =     ({decl_pos = (_startpos, _endpos);
      decl_node = Command3 (SolveEMC (a, out), id)}) in
        _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let out = _v in
        let _endpos_out_ = _endpos in
        let (((_menhir_stack, _menhir_s, _startpos__1_), _, a, _startpos_a_, _endpos_a_), id, _startpos_id_, _endpos_id_) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_out_ in
        let _v : (Ast.decl) =     ({decl_pos = (_startpos, _endpos);
      decl_node = Command (Solve (a, out), id)}) in
        _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let out = _v in
        let _endpos_out_ = _endpos in
        let (((_menhir_stack, _menhir_s, _startpos__1_), _, a), id, _startpos_id_, _endpos_id_) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_out_ in
        let _v : (Ast.decl) =     ({decl_pos = (_startpos, _endpos);
      decl_node = Command (SolveEMC (a, out), id)}) in
        _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_state : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.state) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _endpos ->
    match _menhir_s with
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let st = _v in
        let _endpos_st_ = _endpos in
        let (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_st_ in
        let _v : (Ast.decl) =                       ({decl_pos = (_startpos, _endpos);
      decl_node = Timing st}) in
        _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v
    | MenhirState137 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let st = _v in
        let _endpos_st_ = _endpos in
        let (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_st_ in
        let _v : (Ast.decl) =                      ({decl_pos = (_startpos, _endpos);
      decl_node = Debug st}) in
        _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_bool : _menhir_env -> 'ttv_tail -> _menhir_state -> (bool) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _endpos ->
    match _menhir_s with
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let b = _v in
        let _endpos_b_ = _endpos in
        let ((_menhir_stack, _menhir_s, _startpos__1_), d, _endpos_d_) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_b_ in
        let _v : (Ast.expr) =     ( let w,h = d in
      { expr_pos = (_startpos, _endpos);
        expr_node = Pattern (Array.make h (Array.make w b))} ) in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let b = _v in
        let _endpos_b_ = _endpos in
        let (((_menhir_stack, _menhir_s, _startpos__1_), _, e, _startpos_e_, _endpos_e_), _, d, _endpos_d_) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_b_ in
        let _v : (Ast.expr) =     ({expr_pos = (_startpos, _endpos);
       expr_node = SetOp (SetXY (b), d, e)} ) in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
    | MenhirState147 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let b = _v in
        let _endpos_b_ = _endpos in
        let _endpos = _endpos_b_ in
        let _v : (Ast.bool_expr) =            ( Boolean b ) in
        _menhir_goto_boolean_expr _menhir_env _menhir_stack _menhir_s _v _endpos
    | _ ->
        _menhir_fail ()

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    match _menhir_s with
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPAMP ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | BARBAR ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | HAT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | ALL | ALL3 | APPLY | ASCII _ | ASSERT | COMMA | CONSTANT | COUNT | COUNT3 | CROP | DEBUG | DIFF | DIM _ | DIMACS | EOF | EQUAL | EXIT | H2G2 | IDENT _ | INCLUDE | INTER | LPAR | LSBRA | MAYBE | ONE | PATTERN | PATTERN3 | PRINT | PROBLEM | PROBLEM3 | RESIZE | ROT | RPAR | RSBRA | SET | SHIFT | SOLVE | SOLVE3 | SYM | TILES | TILES3 | TIMING | UNION | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, _startpos__1_), iso), _, e, _startpos_e_, _endpos_e_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_e_ in
            let _v : (Ast.expr) =     ({expr_pos = (_startpos, _endpos);
       expr_node = Apply (iso, e)}) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37)
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, e1, _startpos_e1_, _endpos_e1_), _), _, e2, _startpos_e2_, _endpos_e2_) = _menhir_stack in
        let _startpos = _startpos_e1_ in
        let _endpos = _endpos_e2_ in
        let _v : (Ast.expr) =       ({expr_pos = (_startpos, _endpos);
         expr_node = Binary (Diff, e1, e2)}) in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, e1, _startpos_e1_, _endpos_e1_), _), _, e2, _startpos_e2_, _endpos_e2_) = _menhir_stack in
        let _startpos = _startpos_e1_ in
        let _endpos = _endpos_e2_ in
        let _v : (Ast.expr) =       ({expr_pos = (_startpos, _endpos);
         expr_node = Binary (Xor, e1, e2)}) in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, e1, _startpos_e1_, _endpos_e1_), _), _, e2, _startpos_e2_, _endpos_e2_) = _menhir_stack in
        let _startpos = _startpos_e1_ in
        let _endpos = _endpos_e2_ in
        let _v : (Ast.expr) =       ({expr_pos = (_startpos, _endpos);
         expr_node = Binary (Union, e1, e2)}) in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, e1, _startpos_e1_, _endpos_e1_), _), _, e2, _startpos_e2_, _endpos_e2_) = _menhir_stack in
        let _startpos = _startpos_e1_ in
        let _endpos = _endpos_e2_ in
        let _v : (Ast.expr) =       ({expr_pos = (_startpos, _endpos);
         expr_node = Binary (Inter, e1, e2)}) in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPAMP ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | BARBAR ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | HAT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | ALL | ALL3 | APPLY | ASCII _ | ASSERT | COMMA | CONSTANT | COUNT | COUNT3 | CROP | DEBUG | DIFF | DIM _ | DIMACS | EOF | EQUAL | EXIT | H2G2 | IDENT _ | INCLUDE | INTER | LPAR | LSBRA | MAYBE | ONE | PATTERN | PATTERN3 | PRINT | PROBLEM | PROBLEM3 | RESIZE | ROT | RPAR | RSBRA | SET | SHIFT | SOLVE | SOLVE3 | SYM | TILES | TILES3 | TIMING | UNION | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s, _startpos__1_), pos, _endpos_pos_), d, _endpos_d_), _, e, _startpos_e_, _endpos_e_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_e_ in
            let _v : (Ast.expr) =     ({expr_pos = (_startpos, _endpos);
    expr_node = SetOp (Crop(pos), d, e)}) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46)
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPAMP ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | APPLY ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_startp
        | ASCII _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | BARBAR ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | CONSTANT ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_startp
        | CROP ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_startp
        | DIFF ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_startp
        | HAT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | IDENT _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | INTER ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_startp
        | LPAR ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_startp
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | RESIZE ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_startp
        | SET ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_startp
        | SHIFT ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_startp
        | UNION ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_startp
        | XOR ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47)
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPAMP ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | BARBAR ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | HAT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | ALL | ALL3 | APPLY | ASCII _ | ASSERT | COMMA | CONSTANT | COUNT | COUNT3 | CROP | DEBUG | DIFF | DIM _ | DIMACS | EOF | EQUAL | EXIT | H2G2 | IDENT _ | INCLUDE | INTER | LPAR | LSBRA | MAYBE | ONE | PATTERN | PATTERN3 | PRINT | PROBLEM | PROBLEM3 | RESIZE | ROT | RPAR | RSBRA | SET | SHIFT | SOLVE | SOLVE3 | SYM | TILES | TILES3 | TIMING | UNION | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, _startpos__1_), _, e1, _startpos_e1_, _endpos_e1_), _, e2, _startpos_e2_, _endpos_e2_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_e2_ in
            let _v : (Ast.expr) =       ({expr_pos = (_startpos, _endpos);
         expr_node = Binary (Diff, e1, e2)}) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48)
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPAMP ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | APPLY ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_startp
        | ASCII _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | BARBAR ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | CONSTANT ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_startp
        | CROP ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_startp
        | DIFF ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_startp
        | HAT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | IDENT _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | INTER ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_startp
        | LPAR ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_startp
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | RESIZE ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_startp
        | SET ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_startp
        | SHIFT ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_startp
        | UNION ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_startp
        | XOR ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49)
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPAMP ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | BARBAR ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | HAT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | ALL | ALL3 | APPLY | ASCII _ | ASSERT | COMMA | CONSTANT | COUNT | COUNT3 | CROP | DEBUG | DIFF | DIM _ | DIMACS | EOF | EQUAL | EXIT | H2G2 | IDENT _ | INCLUDE | INTER | LPAR | LSBRA | MAYBE | ONE | PATTERN | PATTERN3 | PRINT | PROBLEM | PROBLEM3 | RESIZE | ROT | RPAR | RSBRA | SET | SHIFT | SOLVE | SOLVE3 | SYM | TILES | TILES3 | TIMING | UNION | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, _startpos__1_), _, e1, _startpos_e1_, _endpos_e1_), _, e2, _startpos_e2_, _endpos_e2_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_e2_ in
            let _v : (Ast.expr) =       ({expr_pos = (_startpos, _endpos);
         expr_node = Binary (Inter, e1, e2)}) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50)
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPAMP ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | BARBAR ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | HAT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState51 in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__3_ = _endpos in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, e, _startpos_e_, _endpos_e_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : (Ast.expr) =     ( e ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51)
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPAMP ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | BARBAR ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | DIM _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState53 in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let d = _v in
            let _endpos_d_ = _endpos in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, e, _startpos_e_, _endpos_e_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_d_ in
            let _v : (Ast.expr) =     ({expr_pos = (_startpos, _endpos);
     expr_node = SetOp (Resize, d, e)}) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | HAT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53)
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPAMP ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | BARBAR ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | DIM _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState55 in
            let _endpos = _menhir_env._menhir_endp in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v, _endpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FALSE ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_endp
            | TRUE ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_endp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56)
        | HAT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55)
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPAMP ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | BARBAR ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | DIM _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState58 in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let d = _v in
            let _endpos_d_ = _endpos in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, e, _startpos_e_, _endpos_e_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_d_ in
            let _v : (Ast.expr) =     ({expr_pos = (_startpos, _endpos);
     expr_node = SetOp (Shift, d, e)}) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | HAT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58)
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPAMP ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | APPLY ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp
        | ASCII _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | BARBAR ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | CONSTANT ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp
        | CROP ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp
        | DIFF ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp
        | HAT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | IDENT _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | INTER ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp
        | LPAR ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | RESIZE ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp
        | SET ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp
        | SHIFT ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp
        | UNION ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp
        | XOR ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60)
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPAMP ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | BARBAR ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | HAT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | ALL | ALL3 | APPLY | ASCII _ | ASSERT | COMMA | CONSTANT | COUNT | COUNT3 | CROP | DEBUG | DIFF | DIM _ | DIMACS | EOF | EQUAL | EXIT | H2G2 | IDENT _ | INCLUDE | INTER | LPAR | LSBRA | MAYBE | ONE | PATTERN | PATTERN3 | PRINT | PROBLEM | PROBLEM3 | RESIZE | ROT | RPAR | RSBRA | SET | SHIFT | SOLVE | SOLVE3 | SYM | TILES | TILES3 | TIMING | UNION | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, _startpos__1_), _, e1, _startpos_e1_, _endpos_e1_), _, e2, _startpos_e2_, _endpos_e2_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_e2_ in
            let _v : (Ast.expr) =       ({expr_pos = (_startpos, _endpos);
         expr_node = Binary (Union, e1, e2)}) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61)
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPAMP ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | APPLY ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp
        | ASCII _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | BARBAR ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | CONSTANT ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp
        | CROP ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp
        | DIFF ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp
        | HAT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | IDENT _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | INTER ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp
        | LPAR ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | RESIZE ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp
        | SET ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp
        | SHIFT ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp
        | UNION ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp
        | XOR ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62)
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPAMP ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | BARBAR ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | HAT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | ALL | ALL3 | APPLY | ASCII _ | ASSERT | COMMA | CONSTANT | COUNT | COUNT3 | CROP | DEBUG | DIFF | DIM _ | DIMACS | EOF | EQUAL | EXIT | H2G2 | IDENT _ | INCLUDE | INTER | LPAR | LSBRA | MAYBE | ONE | PATTERN | PATTERN3 | PRINT | PROBLEM | PROBLEM3 | RESIZE | ROT | RPAR | RSBRA | SET | SHIFT | SOLVE | SOLVE3 | SYM | TILES | TILES3 | TIMING | UNION | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, _startpos__1_), _, e1, _startpos_e1_, _endpos_e1_), _, e2, _startpos_e2_, _endpos_e2_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_e2_ in
            let _v : (Ast.expr) =       ({expr_pos = (_startpos, _endpos);
         expr_node = Binary (Xor, e1, e2)}) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63)
    | MenhirState8 | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPAMP ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | BARBAR ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | HAT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | MAYBE ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | ONE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | ROT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | SYM ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | COMMA | RSBRA ->
            _menhir_reduce58 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67)
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPAMP ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | BARBAR ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | HAT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | IDENT _v ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LSBRA ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104)
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPAMP ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | BARBAR ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | HAT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | IDENT _v ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LSBRA ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111)
    | MenhirState123 | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPAMP ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | BARBAR ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState122 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | APPLY ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_startp
            | ASCII _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | CONSTANT ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_startp
            | CROP ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_startp
            | DIFF ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_startp
            | IDENT _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INTER ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_startp
            | LPAR ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_startp
            | RESIZE ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_startp
            | SET ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_startp
            | SHIFT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_startp
            | UNION ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_startp
            | XOR ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_startp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123)
        | HAT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | RSBRA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
            let _v : (Ast.expr list) =     ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState122)
    | MenhirState128 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPAMP ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | BARBAR ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | HAT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | ALL | ALL3 | ASSERT | COUNT | COUNT3 | DEBUG | DIMACS | EOF | EXIT | H2G2 | INCLUDE | PATTERN | PATTERN3 | PRINT | PROBLEM | PROBLEM3 | SOLVE | SOLVE3 | TILES | TILES3 | TIMING ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, _startpos__1_), id, _startpos_id_, _endpos_id_), _, e, _startpos_e_, _endpos_e_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_e_ in
            let _v : (Ast.decl) =     ({decl_pos = (_startpos, _endpos);
      decl_node = Dpattern (id, e)}) in
            _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129)
    | MenhirState147 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPAMP ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | BARBAR ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | EQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState148 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | APPLY ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_startp
            | ASCII _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | CONSTANT ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_startp
            | CROP ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_startp
            | DIFF ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_startp
            | IDENT _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INTER ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_startp
            | LPAR ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_startp
            | RESIZE ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_startp
            | SET ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_startp
            | SHIFT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_startp
            | UNION ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_startp
            | XOR ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_startp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState149)
        | HAT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState148)
    | MenhirState149 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPAMP ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | BARBAR ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | HAT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | ALL | ALL3 | ASSERT | COUNT | COUNT3 | DEBUG | DIMACS | EOF | EXIT | H2G2 | INCLUDE | PATTERN | PATTERN3 | PRINT | PROBLEM | PROBLEM3 | SOLVE | SOLVE3 | TILES | TILES3 | TIMING ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, e1, _startpos_e1_, _endpos_e1_), _), _, e2, _startpos_e2_, _endpos_e2_) = _menhir_stack in
            let _endpos = _endpos_e2_ in
            let _v : (Ast.bool_expr) =                               ( Comparison (Equal, e1, e2)) in
            _menhir_goto_boolean_expr _menhir_env _menhir_stack _menhir_s _v _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState150)
    | _ ->
        _menhir_fail ()

and _menhir_goto_isometry : _menhir_env -> 'ttv_tail -> (D4.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | APPLY ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_startp
    | ASCII _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | CONSTANT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_startp
    | CROP ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_startp
    | DIFF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_startp
    | IDENT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTER ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_startp
    | RESIZE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_startp
    | SET ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_startp
    | SHIFT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_startp
    | UNION ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_startp
    | XOR ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36

and _menhir_goto_algo_emc : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.algo_emc) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _endpos = _menhir_env._menhir_endp in
            let _menhir_stack = (_menhir_stack, _v, _startpos, _endpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ASCII_OUT ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_endp
            | SVG_OUT ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _endpos = _menhir_env._menhir_endp in
            let _menhir_stack = (_menhir_stack, _v, _startpos, _endpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ASCII_OUT ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_endp
            | SVG_OUT ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState139 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let id = _v in
            let _startpos_id_ = _startpos in
            let _endpos_id_ = _endpos in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, a) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_id_ in
            let _v : (Ast.decl) =     ({decl_pos = (_startpos, _endpos);
      decl_node = Command3 (CountEMC a, id)}) in
            _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState142 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let id = _v in
            let _startpos_id_ = _startpos in
            let _endpos_id_ = _endpos in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, a) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_id_ in
            let _v : (Ast.decl) =     ({decl_pos = (_startpos, _endpos);
      decl_node = Command (CountEMC a, id)}) in
            _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState153 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let id = _v in
            let _startpos_id_ = _startpos in
            let _endpos_id_ = _endpos in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, a) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_id_ in
            let _v : (Ast.decl) =     ({decl_pos = (_startpos, _endpos);
      decl_node = Command3 (AllEMC a, id)}) in
            _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState156 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let id = _v in
            let _startpos_id_ = _startpos in
            let _endpos_id_ = _endpos in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, a) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_id_ in
            let _v : (Ast.decl) =     ({decl_pos = (_startpos, _endpos);
      decl_node = Command (AllEMC a, id)}) in
            _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_decl_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.queue) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, l) = _menhir_stack in
            let _v : (Ast.queue) =                       ( l ) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = _v in
            Obj.magic _1
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState162 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : (Ast.queue) =     ( x :: xs ) in
        _menhir_goto_list_decl_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | APPLY ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _menhir_env._menhir_startp
    | ASCII _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | CONSTANT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _menhir_env._menhir_startp
    | CROP ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _menhir_env._menhir_startp
    | DIFF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _menhir_env._menhir_startp
    | IDENT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTER ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _menhir_env._menhir_startp
    | RESIZE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _menhir_env._menhir_startp
    | SET ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _menhir_env._menhir_startp
    | SHIFT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _menhir_env._menhir_startp
    | UNION ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _menhir_env._menhir_startp
    | XOR ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _menhir_env._menhir_startp
    | RSBRA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState8 in
        let _v : (Ast.tile list) =     ( [] ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_tile__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8

and _menhir_run90 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | STRING _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_endp in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let s = _v in
        let _endpos_s_ = _endpos in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _endpos = _endpos_s_ in
        let _v : (Ast.output) =                       ( Svg s ) in
        _menhir_goto_output _menhir_env _menhir_stack _menhir_s _v _endpos
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run92 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos__1_ = _endpos in
    let _endpos = _endpos__1_ in
    let _v : (Ast.output) =             ( Ascii ) in
    _menhir_goto_output _menhir_env _menhir_stack _menhir_s _v _endpos

and _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RSBRA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_endp in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos__3_ = _endpos in
        let (_menhir_stack, _, xs0) = _menhir_stack in
        let _endpos = _endpos__3_ in
        let _v : (Ast.expr list) = let l =
          let xs = xs0 in
              ( xs )
        in
                                                        ( l ) in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let l = _v in
        let _endpos_l_ = _endpos in
        let ((_menhir_stack, _menhir_s, _startpos__1_), id, _startpos_id_, _endpos_id_) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_l_ in
        let _v : (Ast.decl) =     ( { decl_pos = (_startpos, _endpos);
        decl_node = Dpattern3 (id, l) } ) in
        _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos__1_ = _endpos in
    let _endpos = _endpos__1_ in
    let _v : (Ast.state) =       ( On ) in
    _menhir_goto_state _menhir_env _menhir_stack _menhir_s _v _endpos

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos__1_ = _endpos in
    let _endpos = _endpos__1_ in
    let _v : (Ast.state) =       ( Off ) in
    _menhir_goto_state _menhir_env _menhir_stack _menhir_s _v _endpos

and _menhir_goto_decl : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.decl) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ALL ->
        _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_startp
    | ALL3 ->
        _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_startp
    | ASSERT ->
        _menhir_run147 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_startp
    | COUNT ->
        _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_startp
    | COUNT3 ->
        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_startp
    | DEBUG ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_startp
    | DIMACS ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_startp
    | EXIT ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | H2G2 ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCLUDE ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_startp
    | PATTERN ->
        _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_startp
    | PATTERN3 ->
        _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_startp
    | PRINT ->
        _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_startp
    | PROBLEM ->
        _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_startp
    | PROBLEM3 ->
        _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_startp
    | SOLVE ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_startp
    | SOLVE3 ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_startp
    | TILES ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_startp
    | TILES3 ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_startp
    | TIMING ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_startp
    | EOF ->
        _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState162

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | APPLY ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_startp
    | ASCII _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | CONSTANT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_startp
    | CROP ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_startp
    | DIFF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_startp
    | IDENT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTER ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_startp
    | RESIZE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_startp
    | SET ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_startp
    | SHIFT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_startp
    | UNION ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_startp
    | XOR ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | APPLY ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _menhir_env._menhir_startp
    | ASCII _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | CONSTANT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _menhir_env._menhir_startp
    | CROP ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _menhir_env._menhir_startp
    | DIFF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _menhir_env._menhir_startp
    | IDENT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTER ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _menhir_env._menhir_startp
    | RESIZE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _menhir_env._menhir_startp
    | SET ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _menhir_env._menhir_startp
    | SHIFT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _menhir_env._menhir_startp
    | UNION ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _menhir_env._menhir_startp
    | XOR ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10

and _menhir_run23 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos__1_ = _endpos in
    let _endpos = _endpos__1_ in
    let _v : (bool) =         ( true  ) in
    _menhir_goto_bool _menhir_env _menhir_stack _menhir_s _v _endpos

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | APPLY ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_startp
    | ASCII _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | CONSTANT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_startp
    | CROP ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_startp
    | DIFF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_startp
    | IDENT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTER ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_startp
    | RESIZE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_startp
    | SET ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_startp
    | SHIFT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_startp
    | UNION ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_startp
    | XOR ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | APPLY ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _menhir_env._menhir_startp
    | ASCII _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | CONSTANT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _menhir_env._menhir_startp
    | CROP ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _menhir_env._menhir_startp
    | DIFF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _menhir_env._menhir_startp
    | IDENT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTER ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _menhir_env._menhir_startp
    | RESIZE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _menhir_env._menhir_startp
    | SET ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _menhir_env._menhir_startp
    | SHIFT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _menhir_env._menhir_startp
    | UNION ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _menhir_env._menhir_startp
    | XOR ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | APPLY ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _menhir_env._menhir_startp
    | ASCII _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | CONSTANT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _menhir_env._menhir_startp
    | CROP ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _menhir_env._menhir_startp
    | DIFF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _menhir_env._menhir_startp
    | IDENT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTER ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _menhir_env._menhir_startp
    | RESIZE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _menhir_env._menhir_startp
    | SET ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _menhir_env._menhir_startp
    | SHIFT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _menhir_env._menhir_startp
    | UNION ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _menhir_env._menhir_startp
    | XOR ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | APPLY ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _menhir_env._menhir_startp
    | ASCII _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | CONSTANT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _menhir_env._menhir_startp
    | CROP ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _menhir_env._menhir_startp
    | DIFF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _menhir_env._menhir_startp
    | IDENT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTER ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _menhir_env._menhir_startp
    | RESIZE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _menhir_env._menhir_startp
    | SET ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _menhir_env._menhir_startp
    | SHIFT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _menhir_env._menhir_startp
    | UNION ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _menhir_env._menhir_startp
    | XOR ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | APPLY ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _menhir_env._menhir_startp
    | ASCII _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | CONSTANT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _menhir_env._menhir_startp
    | CROP ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _menhir_env._menhir_startp
    | DIFF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _menhir_env._menhir_startp
    | IDENT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTER ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _menhir_env._menhir_startp
    | RESIZE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _menhir_env._menhir_startp
    | SET ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _menhir_env._menhir_startp
    | SHIFT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _menhir_env._menhir_startp
    | UNION ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _menhir_env._menhir_startp
    | XOR ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let id = _v in
    let _startpos_id_ = _startpos in
    let _endpos_id_ = _endpos in
    let _startpos = _startpos_id_ in
    let _endpos = _endpos_id_ in
    let _v : (Ast.expr) =     ({expr_pos = (_startpos, _endpos);
      expr_node = Var id}) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_run24 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos__1_ = _endpos in
    let _endpos = _endpos__1_ in
    let _v : (bool) =         ( false ) in
    _menhir_goto_bool _menhir_env _menhir_stack _menhir_s _v _endpos

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | APPLY ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_startp
    | ASCII _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | CONSTANT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_startp
    | CROP ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_startp
    | DIFF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_startp
    | IDENT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTER ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_startp
    | RESIZE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_startp
    | SET ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_startp
    | SHIFT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_startp
    | UNION ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_startp
    | XOR ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DIM _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_endp in
        let _menhir_stack = (_menhir_stack, _v, _endpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | DIM _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _menhir_stack = (_menhir_stack, _v, _endpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | APPLY ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _menhir_env._menhir_startp
            | ASCII _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | CONSTANT ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _menhir_env._menhir_startp
            | CROP ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _menhir_env._menhir_startp
            | DIFF ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _menhir_env._menhir_startp
            | IDENT _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INTER ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _menhir_env._menhir_startp
            | LPAR ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _menhir_env._menhir_startp
            | RESIZE ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _menhir_env._menhir_startp
            | SET ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _menhir_env._menhir_startp
            | SHIFT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _menhir_env._menhir_startp
            | UNION ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _menhir_env._menhir_startp
            | XOR ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _menhir_env._menhir_startp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DIM _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_endp in
        let _menhir_stack = (_menhir_stack, _v, _endpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | FALSE ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _menhir_env._menhir_endp
        | TRUE ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _menhir_env._menhir_endp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run26 : _menhir_env -> 'ttv_tail -> _menhir_state -> (bool array array) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let a = _v in
    let _startpos_a_ = _startpos in
    let _endpos_a_ = _endpos in
    let _startpos = _startpos_a_ in
    let _endpos = _endpos_a_ in
    let _v : (Ast.expr) =     ({ expr_pos = (_startpos, _endpos);
       expr_node = Pattern a}) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DIAG1REFL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _v : (D4.t) =             ( Diag1Refl ) in
        _menhir_goto_isometry _menhir_env _menhir_stack _v
    | DIAG2REFL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _v : (D4.t) =             ( Diag2Refl ) in
        _menhir_goto_isometry _menhir_env _menhir_stack _v
    | HORIZREFL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _v : (D4.t) =             ( HorizRefl ) in
        _menhir_goto_isometry _menhir_env _menhir_stack _v
    | ID ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _v : (D4.t) =      ( Id ) in
        _menhir_goto_isometry _menhir_env _menhir_stack _v
    | ROT180 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _v : (D4.t) =          ( Rot180 ) in
        _menhir_goto_isometry _menhir_env _menhir_stack _v
    | ROT270 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _v : (D4.t) =          ( Rot270 ) in
        _menhir_goto_isometry _menhir_env _menhir_stack _v
    | ROT90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _v : (D4.t) =         ( Rot90 ) in
        _menhir_goto_isometry _menhir_env _menhir_stack _v
    | VERTREFL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _v : (D4.t) =            ( VertRefl ) in
        _menhir_goto_isometry _menhir_env _menhir_stack _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run84 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.algo_emc) =                   ( Zdd ) in
    _menhir_goto_algo_emc _menhir_env _menhir_stack _menhir_s _v

and _menhir_run85 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | STRING _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_endp in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let s = _v in
        let _endpos_s_ = _endpos in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _v : (Ast.algo_emc) =                   ( Sat s ) in
        _menhir_goto_algo_emc _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run87 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.algo_emc) =                   ( Dlx ) in
    _menhir_goto_algo_emc _menhir_env _menhir_stack _menhir_s _v

and _menhir_discard : _menhir_env -> token =
  fun _menhir_env ->
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = _menhir_env._menhir_lexer lexbuf in
    _menhir_env._menhir_token <- _tok;
    _menhir_env._menhir_startp <- lexbuf.Lexing.lex_start_p;
    _menhir_env._menhir_endp <- lexbuf.Lexing.lex_curr_p;
    let shifted = Pervasives.(+) _menhir_env._menhir_shifted 1 in
    if Pervasives.(>=) shifted 0 then
      _menhir_env._menhir_shifted <- shifted;
    _tok

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState162 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState156 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState153 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState150 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState149 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState148 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState147 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState142 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState139 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState137 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState128 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _), _, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_reduce56 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.queue) =     ( [] ) in
    _menhir_goto_list_decl_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | OFF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _menhir_env._menhir_endp
    | ON ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _endpos = _menhir_env._menhir_endp in
        let _menhir_stack = (_menhir_stack, _v, _startpos, _endpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | EQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | LSBRA ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run79 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _endpos = _menhir_env._menhir_endp in
        let _menhir_stack = (_menhir_stack, _v, _startpos, _endpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | EQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | LSBRA ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run83 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DLX ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | SAT ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | ZDD ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83

and _menhir_run94 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DLX ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState94 in
        let _startpos = _menhir_env._menhir_startp in
        let _endpos = _menhir_env._menhir_endp in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _endpos = _menhir_env._menhir_endp in
            let _menhir_stack = (_menhir_stack, _v, _startpos, _endpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ASCII_OUT ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_endp
            | SVG_OUT ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | SAT ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | ZDD ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94

and _menhir_run101 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _endpos = _menhir_env._menhir_endp in
        let _menhir_stack = (_menhir_stack, _v, _startpos, _endpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | EQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | APPLY ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp
            | ASCII _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | CONSTANT ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp
            | CROP ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp
            | DIFF ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp
            | IDENT _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INTER ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp
            | LPAR ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp
            | RESIZE ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp
            | SET ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp
            | SHIFT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp
            | UNION ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp
            | XOR ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run108 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _endpos = _menhir_env._menhir_endp in
        let _menhir_stack = (_menhir_stack, _v, _startpos, _endpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | EQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | APPLY ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp
            | ASCII _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | CONSTANT ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp
            | CROP ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp
            | DIFF ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp
            | IDENT _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INTER ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp
            | LPAR ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp
            | RESIZE ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp
            | SET ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp
            | SHIFT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp
            | UNION ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp
            | XOR ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run113 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _endpos = _menhir_env._menhir_endp in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let id = _v in
        let _startpos_id_ = _startpos in
        let _endpos_id_ = _endpos in
        let (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_id_ in
        let _v : (Ast.decl) =                     ({decl_pos = (_startpos, _endpos);
      decl_node = Command (Print, id)}) in
        _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run115 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _endpos = _menhir_env._menhir_endp in
        let _menhir_stack = (_menhir_stack, _v, _startpos, _endpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | EQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | LSBRA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | APPLY ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_startp
                | ASCII _v ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
                | CONSTANT ->
                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_startp
                | CROP ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_startp
                | DIFF ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_startp
                | IDENT _v ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
                | INTER ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_startp
                | LPAR ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_startp
                | RESIZE ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_startp
                | SET ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_startp
                | SHIFT ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_startp
                | UNION ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_startp
                | XOR ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_startp
                | RSBRA ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_s = MenhirState118 in
                    let _v : (Ast.expr list) =     ( [] ) in
                    _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run126 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _endpos = _menhir_env._menhir_endp in
        let _menhir_stack = (_menhir_stack, _v, _startpos, _endpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | EQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | APPLY ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_startp
            | ASCII _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | CONSTANT ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_startp
            | CROP ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_startp
            | DIFF ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_startp
            | IDENT _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INTER ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_startp
            | LPAR ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_startp
            | RESIZE ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_startp
            | SET ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_startp
            | SHIFT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_startp
            | UNION ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_startp
            | XOR ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_startp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState128)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run130 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | STRING _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_endp in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let s = _v in
        let _endpos_s_ = _endpos in
        let (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_s_ in
        let _v : (Ast.decl) =                       ({decl_pos = (_startpos, _endpos);
      decl_node = Include s}) in
        _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run132 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _endpos__1_ = _endpos in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (Ast.decl) =        ({decl_pos = (_startpos, _endpos);
      decl_node = H2g2}) in
    _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v

and _menhir_run133 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _endpos__1_ = _endpos in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (Ast.decl) =        ({decl_pos = (_startpos, _endpos);
      decl_node = Quit}) in
    _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v

and _menhir_run134 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _endpos = _menhir_env._menhir_endp in
        let _menhir_stack = (_menhir_stack, _v, _startpos, _endpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | STRING _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let file = _v in
            let _endpos_file_ = _endpos in
            let ((_menhir_stack, _menhir_s, _startpos__1_), id, _startpos_id_, _endpos_id_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_file_ in
            let _v : (Ast.decl) =     ({decl_pos = (_startpos, _endpos); decl_node = Dimacs (id, file) }) in
            _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run137 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | OFF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_endp
    | ON ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState137

and _menhir_run139 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DLX ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | SAT ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | ZDD ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState139

and _menhir_run142 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DLX ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState142 in
        let _startpos = _menhir_env._menhir_startp in
        let _endpos = _menhir_env._menhir_endp in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let id = _v in
            let _startpos_id_ = _startpos in
            let _endpos_id_ = _endpos in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, a, _startpos_a_, _endpos_a_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_id_ in
            let _v : (Ast.decl) =     ({decl_pos = (_startpos, _endpos);
      decl_node = Command (Count a, id)}) in
            _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | SAT ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | ZDD ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState142

and _menhir_run147 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | APPLY ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_startp
    | ASCII _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | CONSTANT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_startp
    | CROP ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_startp
    | DIFF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_startp
    | FALSE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTER ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_startp
    | RESIZE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_startp
    | SET ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_startp
    | SHIFT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_endp
    | UNION ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_startp
    | XOR ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState147

and _menhir_run153 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DLX ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState153
    | SAT ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState153
    | ZDD ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState153
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState153

and _menhir_run156 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DLX ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState156
    | SAT ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState156
    | ZDD ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState156
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState156

and queue : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.queue) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_startp = lexbuf.Lexing.lex_start_p;
      _menhir_endp = lexbuf.Lexing.lex_curr_p;
      _menhir_shifted = max_int;
      } in
    Obj.magic (let _menhir_stack = () in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ALL ->
        _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_startp
    | ALL3 ->
        _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_startp
    | ASSERT ->
        _menhir_run147 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_startp
    | COUNT ->
        _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_startp
    | COUNT3 ->
        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_startp
    | DEBUG ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_startp
    | DIMACS ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_startp
    | EXIT ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | H2G2 ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCLUDE ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_startp
    | PATTERN ->
        _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_startp
    | PATTERN3 ->
        _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_startp
    | PRINT ->
        _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_startp
    | PROBLEM ->
        _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_startp
    | PROBLEM3 ->
        _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_startp
    | SOLVE ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_startp
    | SOLVE3 ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_startp
    | TILES ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_startp
    | TILES3 ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_startp
    | TIMING ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_startp
    | EOF ->
        _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)



