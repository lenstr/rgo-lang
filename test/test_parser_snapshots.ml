let parse s = Rgo.Parse_driver.parse_string s
let show_program p = Rgo.Ast.show_program p

let parse_and_print s =
  try
    let prog = parse s in
    print_string (show_program prog)
  with
  | Rgo.Parse_driver.Parse_error e ->
      Printf.printf "Parse error at line %d, col %d: %s" e.line e.col e.msg
  | Rgo.Lexer.Lexer_error e ->
      Printf.printf "Lexer error at line %d, col %d: %s" e.pos.line e.pos.col
        e.msg

(* --- snapshot: AST rendering for significant syntax forms --- *)

let%expect_test "fn declaration" =
  parse_and_print "fn add(a: i64, b: i64) -> i64 { a + b }";
  [%expect
    {|
    { Ast.items =
      [(Ast.ItemFn
          { Ast.fn_pub = false;
            fn_name =
            { Ast.node = "add";
              span =
              { Ast.start = { Ast.line = 1; col = 4 };
                stop = { Ast.line = 1; col = 7 } }
              };
            fn_generics = []; fn_self = None;
            fn_params =
            [{ Ast.p_mut = false;
               p_name =
               { Ast.node = "a";
                 span =
                 { Ast.start = { Ast.line = 1; col = 8 };
                   stop = { Ast.line = 1; col = 9 } }
                 };
               p_ty =
               (Ast.TyName
                  { Ast.node = "i64";
                    span =
                    { Ast.start = { Ast.line = 1; col = 11 };
                      stop = { Ast.line = 1; col = 14 } }
                    })
               };
              { Ast.p_mut = false;
                p_name =
                { Ast.node = "b";
                  span =
                  { Ast.start = { Ast.line = 1; col = 16 };
                    stop = { Ast.line = 1; col = 17 } }
                  };
                p_ty =
                (Ast.TyName
                   { Ast.node = "i64";
                     span =
                     { Ast.start = { Ast.line = 1; col = 19 };
                       stop = { Ast.line = 1; col = 22 } }
                     })
                }
              ];
            fn_ret =
            (Some (Ast.TyName
                     { Ast.node = "i64";
                       span =
                       { Ast.start = { Ast.line = 1; col = 27 };
                         stop = { Ast.line = 1; col = 30 } }
                       }));
            fn_body =
            { Ast.stmts = [];
              final_expr =
              (Some (Ast.ExprBinary (Ast.Add,
                       (Ast.ExprIdent
                          { Ast.node = "a";
                            span =
                            { Ast.start = { Ast.line = 1; col = 33 };
                              stop = { Ast.line = 1; col = 34 } }
                            }),
                       (Ast.ExprIdent
                          { Ast.node = "b";
                            span =
                            { Ast.start = { Ast.line = 1; col = 37 };
                              stop = { Ast.line = 1; col = 38 } }
                            })
                       )))
              }
            })
        ]
      }
    |}]

let%expect_test "pub struct" =
  parse_and_print "pub struct Point { pub x: f64, y: f64 }";
  [%expect
    {|
    { Ast.items =
      [Ast.ItemStruct {s_pub = true;
         s_name =
         { Ast.node = "Point";
           span =
           { Ast.start = { Ast.line = 1; col = 12 };
             stop = { Ast.line = 1; col = 17 } }
           };
         s_generics = [];
         s_fields =
         [{ Ast.fd_pub = true;
            fd_name =
            { Ast.node = "x";
              span =
              { Ast.start = { Ast.line = 1; col = 24 };
                stop = { Ast.line = 1; col = 25 } }
              };
            fd_ty =
            (Ast.TyName
               { Ast.node = "f64";
                 span =
                 { Ast.start = { Ast.line = 1; col = 27 };
                   stop = { Ast.line = 1; col = 30 } }
                 })
            };
           { Ast.fd_pub = false;
             fd_name =
             { Ast.node = "y";
               span =
               { Ast.start = { Ast.line = 1; col = 32 };
                 stop = { Ast.line = 1; col = 33 } }
               };
             fd_ty =
             (Ast.TyName
                { Ast.node = "f64";
                  span =
                  { Ast.start = { Ast.line = 1; col = 35 };
                    stop = { Ast.line = 1; col = 38 } }
                  })
             }
           ]}
        ]
      }
    |}]

let%expect_test "enum with variants" =
  parse_and_print
    "enum Shape { Circle(f64), Rectangle { width: f64, height: f64 }, Empty }";
  [%expect
    {|
    { Ast.items =
      [Ast.ItemEnum {e_pub = false;
         e_name =
         { Ast.node = "Shape";
           span =
           { Ast.start = { Ast.line = 1; col = 6 };
             stop = { Ast.line = 1; col = 11 } }
           };
         e_generics = [];
         e_variants =
         [{ Ast.var_name =
            { Ast.node = "Circle";
              span =
              { Ast.start = { Ast.line = 1; col = 14 };
                stop = { Ast.line = 1; col = 20 } }
              };
            var_fields =
            (Some (Ast.TupleFields
                     [(Ast.TyName
                         { Ast.node = "f64";
                           span =
                           { Ast.start = { Ast.line = 1; col = 21 };
                             stop = { Ast.line = 1; col = 24 } }
                           })
                       ]))
            };
           { Ast.var_name =
             { Ast.node = "Rectangle";
               span =
               { Ast.start = { Ast.line = 1; col = 27 };
                 stop = { Ast.line = 1; col = 36 } }
               };
             var_fields =
             (Some (Ast.StructFields
                      [{ Ast.fd_pub = false;
                         fd_name =
                         { Ast.node = "width";
                           span =
                           { Ast.start = { Ast.line = 1; col = 39 };
                             stop = { Ast.line = 1; col = 44 } }
                           };
                         fd_ty =
                         (Ast.TyName
                            { Ast.node = "f64";
                              span =
                              { Ast.start = { Ast.line = 1; col = 46 };
                                stop = { Ast.line = 1; col = 49 } }
                              })
                         };
                        { Ast.fd_pub = false;
                          fd_name =
                          { Ast.node = "height";
                            span =
                            { Ast.start = { Ast.line = 1; col = 51 };
                              stop = { Ast.line = 1; col = 57 } }
                            };
                          fd_ty =
                          (Ast.TyName
                             { Ast.node = "f64";
                               span =
                               { Ast.start = { Ast.line = 1; col = 59 };
                                 stop = { Ast.line = 1; col = 62 } }
                               })
                          }
                        ]))
             };
           { Ast.var_name =
             { Ast.node = "Empty";
               span =
               { Ast.start = { Ast.line = 1; col = 66 };
                 stop = { Ast.line = 1; col = 71 } }
               };
             var_fields = None }
           ]}
        ]
      }
    |}]

let%expect_test "match with patterns" =
  parse_and_print
    {|fn f(s: Shape) -> f64 {
    match s {
        Shape::Circle(r) => r,
        Shape::Rectangle { width, height } => width,
        Shape::Empty => 0.0,
        _ => 0.0,
    }
}|};
  [%expect
    {|
    { Ast.items =
      [(Ast.ItemFn
          { Ast.fn_pub = false;
            fn_name =
            { Ast.node = "f";
              span =
              { Ast.start = { Ast.line = 1; col = 4 };
                stop = { Ast.line = 1; col = 5 } }
              };
            fn_generics = []; fn_self = None;
            fn_params =
            [{ Ast.p_mut = false;
               p_name =
               { Ast.node = "s";
                 span =
                 { Ast.start = { Ast.line = 1; col = 6 };
                   stop = { Ast.line = 1; col = 7 } }
                 };
               p_ty =
               (Ast.TyName
                  { Ast.node = "Shape";
                    span =
                    { Ast.start = { Ast.line = 1; col = 9 };
                      stop = { Ast.line = 1; col = 14 } }
                    })
               }
              ];
            fn_ret =
            (Some (Ast.TyName
                     { Ast.node = "f64";
                       span =
                       { Ast.start = { Ast.line = 1; col = 19 };
                         stop = { Ast.line = 1; col = 22 } }
                       }));
            fn_body =
            { Ast.stmts =
              [(Ast.StmtExpr
                  (Ast.ExprMatch (
                     (Ast.ExprIdent
                        { Ast.node = "s";
                          span =
                          { Ast.start = { Ast.line = 2; col = 11 };
                            stop = { Ast.line = 2; col = 12 } }
                          }),
                     [{ Ast.arm_pat =
                        (Ast.PatTuple (
                           { Ast.node = "Shape";
                             span =
                             { Ast.start = { Ast.line = 3; col = 9 };
                               stop = { Ast.line = 3; col = 14 } }
                             },
                           { Ast.node = "Circle";
                             span =
                             { Ast.start = { Ast.line = 3; col = 16 };
                               stop = { Ast.line = 3; col = 22 } }
                             },
                           [(Ast.PatBind
                               { Ast.node = "r";
                                 span =
                                 { Ast.start = { Ast.line = 3; col = 23 };
                                   stop = { Ast.line = 3; col = 24 } }
                                 })
                             ]
                           ));
                        arm_expr =
                        (Ast.ExprIdent
                           { Ast.node = "r";
                             span =
                             { Ast.start = { Ast.line = 3; col = 29 };
                               stop = { Ast.line = 3; col = 30 } }
                             })
                        };
                       { Ast.arm_pat =
                         (Ast.PatStruct (
                            { Ast.node = "Shape";
                              span =
                              { Ast.start = { Ast.line = 4; col = 9 };
                                stop = { Ast.line = 4; col = 14 } }
                              },
                            { Ast.node = "Rectangle";
                              span =
                              { Ast.start = { Ast.line = 4; col = 16 };
                                stop = { Ast.line = 4; col = 25 } }
                              },
                            [{ Ast.fp_name =
                               { Ast.node = "width";
                                 span =
                                 { Ast.start = { Ast.line = 4; col = 28 };
                                   stop = { Ast.line = 4; col = 33 } }
                                 };
                               fp_pat = None };
                              { Ast.fp_name =
                                { Ast.node = "height";
                                  span =
                                  { Ast.start = { Ast.line = 4; col = 35 };
                                    stop = { Ast.line = 4; col = 41 } }
                                  };
                                fp_pat = None }
                              ]
                            ));
                         arm_expr =
                         (Ast.ExprIdent
                            { Ast.node = "width";
                              span =
                              { Ast.start = { Ast.line = 4; col = 47 };
                                stop = { Ast.line = 4; col = 52 } }
                              })
                         };
                       { Ast.arm_pat =
                         (Ast.PatTuple (
                            { Ast.node = "Shape";
                              span =
                              { Ast.start = { Ast.line = 5; col = 9 };
                                stop = { Ast.line = 5; col = 14 } }
                              },
                            { Ast.node = "Empty";
                              span =
                              { Ast.start = { Ast.line = 5; col = 16 };
                                stop = { Ast.line = 5; col = 21 } }
                              },
                            []));
                         arm_expr = (Ast.ExprLit (Ast.LitFloat "0.0")) };
                       { Ast.arm_pat = Ast.PatWild;
                         arm_expr = (Ast.ExprLit (Ast.LitFloat "0.0")) }
                       ]
                     )))
                ];
              final_expr = None }
            })
        ]
      }
    |}]

let%expect_test "if-else" =
  parse_and_print "fn max(a: i64, b: i64) -> i64 { if a > b { a } else { b } }";
  [%expect
    {|
    { Ast.items =
      [(Ast.ItemFn
          { Ast.fn_pub = false;
            fn_name =
            { Ast.node = "max";
              span =
              { Ast.start = { Ast.line = 1; col = 4 };
                stop = { Ast.line = 1; col = 7 } }
              };
            fn_generics = []; fn_self = None;
            fn_params =
            [{ Ast.p_mut = false;
               p_name =
               { Ast.node = "a";
                 span =
                 { Ast.start = { Ast.line = 1; col = 8 };
                   stop = { Ast.line = 1; col = 9 } }
                 };
               p_ty =
               (Ast.TyName
                  { Ast.node = "i64";
                    span =
                    { Ast.start = { Ast.line = 1; col = 11 };
                      stop = { Ast.line = 1; col = 14 } }
                    })
               };
              { Ast.p_mut = false;
                p_name =
                { Ast.node = "b";
                  span =
                  { Ast.start = { Ast.line = 1; col = 16 };
                    stop = { Ast.line = 1; col = 17 } }
                  };
                p_ty =
                (Ast.TyName
                   { Ast.node = "i64";
                     span =
                     { Ast.start = { Ast.line = 1; col = 19 };
                       stop = { Ast.line = 1; col = 22 } }
                     })
                }
              ];
            fn_ret =
            (Some (Ast.TyName
                     { Ast.node = "i64";
                       span =
                       { Ast.start = { Ast.line = 1; col = 27 };
                         stop = { Ast.line = 1; col = 30 } }
                       }));
            fn_body =
            { Ast.stmts =
              [(Ast.StmtExpr
                  (Ast.ExprIf (
                     (Ast.ExprBinary (Ast.Gt,
                        (Ast.ExprIdent
                           { Ast.node = "a";
                             span =
                             { Ast.start = { Ast.line = 1; col = 36 };
                               stop = { Ast.line = 1; col = 37 } }
                             }),
                        (Ast.ExprIdent
                           { Ast.node = "b";
                             span =
                             { Ast.start = { Ast.line = 1; col = 40 };
                               stop = { Ast.line = 1; col = 41 } }
                             })
                        )),
                     { Ast.stmts = [];
                       final_expr =
                       (Some (Ast.ExprIdent
                                { Ast.node = "a";
                                  span =
                                  { Ast.start = { Ast.line = 1; col = 44 };
                                    stop = { Ast.line = 1; col = 45 } }
                                  }))
                       },
                     (Some { Ast.stmts = [];
                             final_expr =
                             (Some (Ast.ExprIdent
                                      { Ast.node = "b";
                                        span =
                                        { Ast.start = { Ast.line = 1; col = 55 };
                                          stop = { Ast.line = 1; col = 56 } }
                                        }))
                             })
                     )))
                ];
              final_expr = None }
            })
        ]
      }
    |}]

let%expect_test "impl with self methods" =
  parse_and_print
    {|impl Counter {
    pub fn new() -> Self { Counter { value: 0 } }
    pub fn get(&self) -> i64 { self.value }
    pub fn inc(&mut self) { self.value = self.value + 1; }
}|};
  [%expect
    {|
    { Ast.items =
      [Ast.ItemImpl {i_generics = [];
         i_ty =
         (Ast.TyName
            { Ast.node = "Counter";
              span =
              { Ast.start = { Ast.line = 1; col = 6 };
                stop = { Ast.line = 1; col = 13 } }
              });
         i_items =
         [{ Ast.fn_pub = true;
            fn_name =
            { Ast.node = "new";
              span =
              { Ast.start = { Ast.line = 2; col = 12 };
                stop = { Ast.line = 2; col = 15 } }
              };
            fn_generics = []; fn_self = None; fn_params = [];
            fn_ret = (Some Ast.TySelf);
            fn_body =
            { Ast.stmts = [];
              final_expr =
              (Some (Ast.ExprStruct (
                       (Ast.TyName
                          { Ast.node = "Counter";
                            span =
                            { Ast.start = { Ast.line = 2; col = 28 };
                              stop = { Ast.line = 2; col = 35 } }
                            }),
                       [{ Ast.sf_name =
                          { Ast.node = "value";
                            span =
                            { Ast.start = { Ast.line = 2; col = 38 };
                              stop = { Ast.line = 2; col = 43 } }
                            };
                          sf_expr = (Ast.ExprLit (Ast.LitInt "0")) }
                         ]
                       )))
              }
            };
           { Ast.fn_pub = true;
             fn_name =
             { Ast.node = "get";
               span =
               { Ast.start = { Ast.line = 3; col = 12 };
                 stop = { Ast.line = 3; col = 15 } }
               };
             fn_generics = []; fn_self = (Some Ast.SelfRef); fn_params = [];
             fn_ret =
             (Some (Ast.TyName
                      { Ast.node = "i64";
                        span =
                        { Ast.start = { Ast.line = 3; col = 26 };
                          stop = { Ast.line = 3; col = 29 } }
                        }));
             fn_body =
             { Ast.stmts = [];
               final_expr =
               (Some (Ast.ExprFieldAccess (Ast.ExprSelf,
                        { Ast.node = "value";
                          span =
                          { Ast.start = { Ast.line = 3; col = 37 };
                            stop = { Ast.line = 3; col = 42 } }
                          }
                        )))
               }
             };
           { Ast.fn_pub = true;
             fn_name =
             { Ast.node = "inc";
               span =
               { Ast.start = { Ast.line = 4; col = 12 };
                 stop = { Ast.line = 4; col = 15 } }
               };
             fn_generics = []; fn_self = (Some Ast.SelfMutRef); fn_params = [];
             fn_ret = None;
             fn_body =
             { Ast.stmts =
               [(Ast.StmtExpr
                   (Ast.ExprAssign (Ast.Assign,
                      (Ast.ExprFieldAccess (Ast.ExprSelf,
                         { Ast.node = "value";
                           span =
                           { Ast.start = { Ast.line = 4; col = 34 };
                             stop = { Ast.line = 4; col = 39 } }
                           }
                         )),
                      (Ast.ExprBinary (Ast.Add,
                         (Ast.ExprFieldAccess (Ast.ExprSelf,
                            { Ast.node = "value";
                              span =
                              { Ast.start = { Ast.line = 4; col = 47 };
                                stop = { Ast.line = 4; col = 52 } }
                              }
                            )),
                         (Ast.ExprLit (Ast.LitInt "1"))))
                      )))
                 ];
               final_expr = None }
             }
           ]}
        ]
      }
    |}]

let%expect_test "trait with default method" =
  parse_and_print
    {|trait Summary {
    fn summary(&self) -> String;
    fn short(&self) -> String {
        self.summary()
    }
}|};
  [%expect
    {|
    { Ast.items =
      [Ast.ItemTrait {t_pub = false;
         t_name =
         { Ast.node = "Summary";
           span =
           { Ast.start = { Ast.line = 1; col = 7 };
             stop = { Ast.line = 1; col = 14 } }
           };
         t_generics = [];
         t_items =
         [(Ast.TraitFnSig
             { Ast.sig_name =
               { Ast.node = "summary";
                 span =
                 { Ast.start = { Ast.line = 2; col = 8 };
                   stop = { Ast.line = 2; col = 15 } }
                 };
               sig_generics = []; sig_self = (Some Ast.SelfRef); sig_params = [];
               sig_ret =
               (Some (Ast.TyName
                        { Ast.node = "String";
                          span =
                          { Ast.start = { Ast.line = 2; col = 26 };
                            stop = { Ast.line = 2; col = 32 } }
                          }))
               });
           (Ast.TraitFnDecl
              { Ast.fn_pub = false;
                fn_name =
                { Ast.node = "short";
                  span =
                  { Ast.start = { Ast.line = 3; col = 8 };
                    stop = { Ast.line = 3; col = 13 } }
                  };
                fn_generics = []; fn_self = (Some Ast.SelfRef); fn_params = [];
                fn_ret =
                (Some (Ast.TyName
                         { Ast.node = "String";
                           span =
                           { Ast.start = { Ast.line = 3; col = 24 };
                             stop = { Ast.line = 3; col = 30 } }
                           }));
                fn_body =
                { Ast.stmts = [];
                  final_expr =
                  (Some (Ast.ExprMethodCall (Ast.ExprSelf,
                           { Ast.node = "summary";
                             span =
                             { Ast.start = { Ast.line = 4; col = 14 };
                               stop = { Ast.line = 4; col = 21 } }
                             },
                           [])))
                  }
                })
           ]}
        ]
      }
    |}]

let%expect_test "trait impl for struct" =
  parse_and_print
    {|impl Display for Article {
    fn display(&self) -> String {
        self.title
    }
}|};
  [%expect
    {|
    { Ast.items =
      [Ast.ItemTraitImpl {ti_generics = [];
         ti_trait =
         { Ast.node = "Display";
           span =
           { Ast.start = { Ast.line = 1; col = 6 };
             stop = { Ast.line = 1; col = 13 } }
           };
         ti_ty =
         (Ast.TyName
            { Ast.node = "Article";
              span =
              { Ast.start = { Ast.line = 1; col = 18 };
                stop = { Ast.line = 1; col = 25 } }
              });
         ti_items =
         [{ Ast.fn_pub = false;
            fn_name =
            { Ast.node = "display";
              span =
              { Ast.start = { Ast.line = 2; col = 8 };
                stop = { Ast.line = 2; col = 15 } }
              };
            fn_generics = []; fn_self = (Some Ast.SelfRef); fn_params = [];
            fn_ret =
            (Some (Ast.TyName
                     { Ast.node = "String";
                       span =
                       { Ast.start = { Ast.line = 2; col = 26 };
                         stop = { Ast.line = 2; col = 32 } }
                       }));
            fn_body =
            { Ast.stmts = [];
              final_expr =
              (Some (Ast.ExprFieldAccess (Ast.ExprSelf,
                       { Ast.node = "title";
                         span =
                         { Ast.start = { Ast.line = 3; col = 14 };
                           stop = { Ast.line = 3; col = 19 } }
                         }
                       )))
              }
            }
           ]}
        ]
      }
    |}]

let%expect_test "generic struct and impl" =
  parse_and_print
    {|struct Box<T> { value: T }
impl<T> Box<T> {
    pub fn new(v: T) -> Self { Box { value: v } }
}|};
  [%expect
    {|
    { Ast.items =
      [Ast.ItemStruct {s_pub = false;
         s_name =
         { Ast.node = "Box";
           span =
           { Ast.start = { Ast.line = 1; col = 8 };
             stop = { Ast.line = 1; col = 11 } }
           };
         s_generics =
         [{ Ast.tp_name =
            { Ast.node = "T";
              span =
              { Ast.start = { Ast.line = 1; col = 12 };
                stop = { Ast.line = 1; col = 13 } }
              };
            tp_bound = None }
           ];
         s_fields =
         [{ Ast.fd_pub = false;
            fd_name =
            { Ast.node = "value";
              span =
              { Ast.start = { Ast.line = 1; col = 17 };
                stop = { Ast.line = 1; col = 22 } }
              };
            fd_ty =
            (Ast.TyName
               { Ast.node = "T";
                 span =
                 { Ast.start = { Ast.line = 1; col = 24 };
                   stop = { Ast.line = 1; col = 25 } }
                 })
            }
           ]};
        Ast.ItemImpl {
          i_generics =
          [{ Ast.tp_name =
             { Ast.node = "T";
               span =
               { Ast.start = { Ast.line = 2; col = 6 };
                 stop = { Ast.line = 2; col = 7 } }
               };
             tp_bound = None }
            ];
          i_ty =
          (Ast.TyGeneric (
             { Ast.node = "Box";
               span =
               { Ast.start = { Ast.line = 2; col = 9 };
                 stop = { Ast.line = 2; col = 12 } }
               },
             [(Ast.TyName
                 { Ast.node = "T";
                   span =
                   { Ast.start = { Ast.line = 2; col = 13 };
                     stop = { Ast.line = 2; col = 14 } }
                   })
               ]
             ));
          i_items =
          [{ Ast.fn_pub = true;
             fn_name =
             { Ast.node = "new";
               span =
               { Ast.start = { Ast.line = 3; col = 12 };
                 stop = { Ast.line = 3; col = 15 } }
               };
             fn_generics = []; fn_self = None;
             fn_params =
             [{ Ast.p_mut = false;
                p_name =
                { Ast.node = "v";
                  span =
                  { Ast.start = { Ast.line = 3; col = 16 };
                    stop = { Ast.line = 3; col = 17 } }
                  };
                p_ty =
                (Ast.TyName
                   { Ast.node = "T";
                     span =
                     { Ast.start = { Ast.line = 3; col = 19 };
                       stop = { Ast.line = 3; col = 20 } }
                     })
                }
               ];
             fn_ret = (Some Ast.TySelf);
             fn_body =
             { Ast.stmts = [];
               final_expr =
               (Some (Ast.ExprStruct (
                        (Ast.TyName
                           { Ast.node = "Box";
                             span =
                             { Ast.start = { Ast.line = 3; col = 32 };
                               stop = { Ast.line = 3; col = 35 } }
                             }),
                        [{ Ast.sf_name =
                           { Ast.node = "value";
                             span =
                             { Ast.start = { Ast.line = 3; col = 38 };
                               stop = { Ast.line = 3; col = 43 } }
                             };
                           sf_expr =
                           (Ast.ExprIdent
                              { Ast.node = "v";
                                span =
                                { Ast.start = { Ast.line = 3; col = 45 };
                                  stop = { Ast.line = 3; col = 46 } }
                                })
                           }
                          ]
                        )))
               }
             }
            ]}
        ]
      }
    |}]

let%expect_test "generic bounds" =
  parse_and_print "fn print_all<T: Display + Summary>(items: Vec<T>) { }";
  [%expect
    {|
    { Ast.items =
      [(Ast.ItemFn
          { Ast.fn_pub = false;
            fn_name =
            { Ast.node = "print_all";
              span =
              { Ast.start = { Ast.line = 1; col = 4 };
                stop = { Ast.line = 1; col = 13 } }
              };
            fn_generics =
            [{ Ast.tp_name =
               { Ast.node = "T";
                 span =
                 { Ast.start = { Ast.line = 1; col = 14 };
                   stop = { Ast.line = 1; col = 15 } }
                 };
               tp_bound =
               (Some [{ Ast.node = "Display";
                        span =
                        { Ast.start = { Ast.line = 1; col = 17 };
                          stop = { Ast.line = 1; col = 24 } }
                        };
                       { Ast.node = "Summary";
                         span =
                         { Ast.start = { Ast.line = 1; col = 27 };
                           stop = { Ast.line = 1; col = 34 } }
                         }
                       ])
               }
              ];
            fn_self = None;
            fn_params =
            [{ Ast.p_mut = false;
               p_name =
               { Ast.node = "items";
                 span =
                 { Ast.start = { Ast.line = 1; col = 36 };
                   stop = { Ast.line = 1; col = 41 } }
                 };
               p_ty =
               (Ast.TyGeneric (
                  { Ast.node = "Vec";
                    span =
                    { Ast.start = { Ast.line = 1; col = 43 };
                      stop = { Ast.line = 1; col = 46 } }
                    },
                  [(Ast.TyName
                      { Ast.node = "T";
                        span =
                        { Ast.start = { Ast.line = 1; col = 47 };
                          stop = { Ast.line = 1; col = 48 } }
                        })
                    ]
                  ))
               }
              ];
            fn_ret = None; fn_body = { Ast.stmts = []; final_expr = None } })
        ]
      }
    |}]

let%expect_test "question mark" =
  parse_and_print
    {|fn double(s: str) -> Result<i64, str> {
    let n = parse_int(s)?;
    Ok(n * 2)
}|};
  [%expect
    {|
    { Ast.items =
      [(Ast.ItemFn
          { Ast.fn_pub = false;
            fn_name =
            { Ast.node = "double";
              span =
              { Ast.start = { Ast.line = 1; col = 4 };
                stop = { Ast.line = 1; col = 10 } }
              };
            fn_generics = []; fn_self = None;
            fn_params =
            [{ Ast.p_mut = false;
               p_name =
               { Ast.node = "s";
                 span =
                 { Ast.start = { Ast.line = 1; col = 11 };
                   stop = { Ast.line = 1; col = 12 } }
                 };
               p_ty =
               (Ast.TyName
                  { Ast.node = "str";
                    span =
                    { Ast.start = { Ast.line = 1; col = 14 };
                      stop = { Ast.line = 1; col = 17 } }
                    })
               }
              ];
            fn_ret =
            (Some (Ast.TyGeneric (
                     { Ast.node = "Result";
                       span =
                       { Ast.start = { Ast.line = 1; col = 22 };
                         stop = { Ast.line = 1; col = 28 } }
                       },
                     [(Ast.TyName
                         { Ast.node = "i64";
                           span =
                           { Ast.start = { Ast.line = 1; col = 29 };
                             stop = { Ast.line = 1; col = 32 } }
                           });
                       (Ast.TyName
                          { Ast.node = "str";
                            span =
                            { Ast.start = { Ast.line = 1; col = 34 };
                              stop = { Ast.line = 1; col = 37 } }
                            })
                       ]
                     )));
            fn_body =
            { Ast.stmts =
              [Ast.StmtLet {is_mut = false;
                 pat =
                 (Ast.PatBind
                    { Ast.node = "n";
                      span =
                      { Ast.start = { Ast.line = 2; col = 9 };
                        stop = { Ast.line = 2; col = 10 } }
                      });
                 ty = None;
                 init =
                 (Ast.ExprQuestion
                    (Ast.ExprCall (
                       (Ast.ExprIdent
                          { Ast.node = "parse_int";
                            span =
                            { Ast.start = { Ast.line = 2; col = 13 };
                              stop = { Ast.line = 2; col = 22 } }
                            }),
                       [(Ast.ExprIdent
                           { Ast.node = "s";
                             span =
                             { Ast.start = { Ast.line = 2; col = 23 };
                               stop = { Ast.line = 2; col = 24 } }
                             })
                         ]
                       )))}
                ];
              final_expr =
              (Some (Ast.ExprCall (
                       (Ast.ExprIdent
                          { Ast.node = "Ok";
                            span =
                            { Ast.start = { Ast.line = 3; col = 5 };
                              stop = { Ast.line = 3; col = 7 } }
                            }),
                       [(Ast.ExprBinary (Ast.Mul,
                           (Ast.ExprIdent
                              { Ast.node = "n";
                                span =
                                { Ast.start = { Ast.line = 3; col = 8 };
                                  stop = { Ast.line = 3; col = 9 } }
                                }),
                           (Ast.ExprLit (Ast.LitInt "2"))))
                         ]
                       )))
              }
            })
        ]
      }
    |}]

let%expect_test "array literals and repeat" =
  parse_and_print
    {|fn main() {
    let xs = [1, 2, 3];
    let ys: Vec<i64> = [];
    let zs = [0; 100];
}|};
  [%expect
    {|
    { Ast.items =
      [(Ast.ItemFn
          { Ast.fn_pub = false;
            fn_name =
            { Ast.node = "main";
              span =
              { Ast.start = { Ast.line = 1; col = 4 };
                stop = { Ast.line = 1; col = 8 } }
              };
            fn_generics = []; fn_self = None; fn_params = []; fn_ret = None;
            fn_body =
            { Ast.stmts =
              [Ast.StmtLet {is_mut = false;
                 pat =
                 (Ast.PatBind
                    { Ast.node = "xs";
                      span =
                      { Ast.start = { Ast.line = 2; col = 9 };
                        stop = { Ast.line = 2; col = 11 } }
                      });
                 ty = None;
                 init =
                 (Ast.ExprArray
                    [(Ast.ExprLit (Ast.LitInt "1"));
                      (Ast.ExprLit (Ast.LitInt "2"));
                      (Ast.ExprLit (Ast.LitInt "3"))])};
                Ast.StmtLet {is_mut = false;
                  pat =
                  (Ast.PatBind
                     { Ast.node = "ys";
                       span =
                       { Ast.start = { Ast.line = 3; col = 9 };
                         stop = { Ast.line = 3; col = 11 } }
                       });
                  ty =
                  (Some (Ast.TyGeneric (
                           { Ast.node = "Vec";
                             span =
                             { Ast.start = { Ast.line = 3; col = 13 };
                               stop = { Ast.line = 3; col = 16 } }
                             },
                           [(Ast.TyName
                               { Ast.node = "i64";
                                 span =
                                 { Ast.start = { Ast.line = 3; col = 17 };
                                   stop = { Ast.line = 3; col = 20 } }
                                 })
                             ]
                           )));
                  init = (Ast.ExprArray [])};
                Ast.StmtLet {is_mut = false;
                  pat =
                  (Ast.PatBind
                     { Ast.node = "zs";
                       span =
                       { Ast.start = { Ast.line = 4; col = 9 };
                         stop = { Ast.line = 4; col = 11 } }
                       });
                  ty = None;
                  init =
                  (Ast.ExprRepeat ((Ast.ExprLit (Ast.LitInt "0")),
                     (Ast.ExprLit (Ast.LitInt "100"))))}
                ];
              final_expr = None }
            })
        ]
      }
    |}]

let%expect_test "loops" =
  parse_and_print
    {|fn main() {
    for x in xs { sum = sum + x; }
    while i < 10 { i = i + 1; }
    loop { break; }
}|};
  [%expect
    {|
    { Ast.items =
      [(Ast.ItemFn
          { Ast.fn_pub = false;
            fn_name =
            { Ast.node = "main";
              span =
              { Ast.start = { Ast.line = 1; col = 4 };
                stop = { Ast.line = 1; col = 8 } }
              };
            fn_generics = []; fn_self = None; fn_params = []; fn_ret = None;
            fn_body =
            { Ast.stmts =
              [(Ast.StmtExpr
                  (Ast.ExprFor (
                     { Ast.node = "x";
                       span =
                       { Ast.start = { Ast.line = 2; col = 9 };
                         stop = { Ast.line = 2; col = 10 } }
                       },
                     (Ast.ExprIdent
                        { Ast.node = "xs";
                          span =
                          { Ast.start = { Ast.line = 2; col = 14 };
                            stop = { Ast.line = 2; col = 16 } }
                          }),
                     { Ast.stmts =
                       [(Ast.StmtExpr
                           (Ast.ExprAssign (Ast.Assign,
                              (Ast.ExprIdent
                                 { Ast.node = "sum";
                                   span =
                                   { Ast.start = { Ast.line = 2; col = 19 };
                                     stop = { Ast.line = 2; col = 22 } }
                                   }),
                              (Ast.ExprBinary (Ast.Add,
                                 (Ast.ExprIdent
                                    { Ast.node = "sum";
                                      span =
                                      { Ast.start = { Ast.line = 2; col = 25 };
                                        stop = { Ast.line = 2; col = 28 } }
                                      }),
                                 (Ast.ExprIdent
                                    { Ast.node = "x";
                                      span =
                                      { Ast.start = { Ast.line = 2; col = 31 };
                                        stop = { Ast.line = 2; col = 32 } }
                                      })
                                 ))
                              )))
                         ];
                       final_expr = None }
                     )));
                (Ast.StmtExpr
                   (Ast.ExprWhile (
                      (Ast.ExprBinary (Ast.Lt,
                         (Ast.ExprIdent
                            { Ast.node = "i";
                              span =
                              { Ast.start = { Ast.line = 3; col = 11 };
                                stop = { Ast.line = 3; col = 12 } }
                              }),
                         (Ast.ExprLit (Ast.LitInt "10")))),
                      { Ast.stmts =
                        [(Ast.StmtExpr
                            (Ast.ExprAssign (Ast.Assign,
                               (Ast.ExprIdent
                                  { Ast.node = "i";
                                    span =
                                    { Ast.start = { Ast.line = 3; col = 20 };
                                      stop = { Ast.line = 3; col = 21 } }
                                    }),
                               (Ast.ExprBinary (Ast.Add,
                                  (Ast.ExprIdent
                                     { Ast.node = "i";
                                       span =
                                       { Ast.start = { Ast.line = 3; col = 24 };
                                         stop = { Ast.line = 3; col = 25 } }
                                       }),
                                  (Ast.ExprLit (Ast.LitInt "1"))))
                               )))
                          ];
                        final_expr = None }
                      )));
                (Ast.StmtExpr
                   (Ast.ExprLoop (None,
                      { Ast.stmts = [(Ast.StmtExpr Ast.ExprBreak)];
                        final_expr = None }
                      )))
                ];
              final_expr = None }
            })
        ]
      }
    |}]

let%expect_test "struct literal" =
  parse_and_print {|fn main() {
    let p = Point { x: 1.0, y: 2.0 };
}|};
  [%expect
    {|
    { Ast.items =
      [(Ast.ItemFn
          { Ast.fn_pub = false;
            fn_name =
            { Ast.node = "main";
              span =
              { Ast.start = { Ast.line = 1; col = 4 };
                stop = { Ast.line = 1; col = 8 } }
              };
            fn_generics = []; fn_self = None; fn_params = []; fn_ret = None;
            fn_body =
            { Ast.stmts =
              [Ast.StmtLet {is_mut = false;
                 pat =
                 (Ast.PatBind
                    { Ast.node = "p";
                      span =
                      { Ast.start = { Ast.line = 2; col = 9 };
                        stop = { Ast.line = 2; col = 10 } }
                      });
                 ty = None;
                 init =
                 (Ast.ExprStruct (
                    (Ast.TyName
                       { Ast.node = "Point";
                         span =
                         { Ast.start = { Ast.line = 2; col = 13 };
                           stop = { Ast.line = 2; col = 18 } }
                         }),
                    [{ Ast.sf_name =
                       { Ast.node = "x";
                         span =
                         { Ast.start = { Ast.line = 2; col = 21 };
                           stop = { Ast.line = 2; col = 22 } }
                         };
                       sf_expr = (Ast.ExprLit (Ast.LitFloat "1.0")) };
                      { Ast.sf_name =
                        { Ast.node = "y";
                          span =
                          { Ast.start = { Ast.line = 2; col = 29 };
                            stop = { Ast.line = 2; col = 30 } }
                          };
                        sf_expr = (Ast.ExprLit (Ast.LitFloat "2.0")) }
                      ]
                    ))}
                ];
              final_expr = None }
            })
        ]
      }
    |}]

let%expect_test "method call and field access" =
  parse_and_print
    {|fn main() {
    let n = v.len();
    let x = p.x;
    v.push(1);
}|};
  [%expect
    {|
    { Ast.items =
      [(Ast.ItemFn
          { Ast.fn_pub = false;
            fn_name =
            { Ast.node = "main";
              span =
              { Ast.start = { Ast.line = 1; col = 4 };
                stop = { Ast.line = 1; col = 8 } }
              };
            fn_generics = []; fn_self = None; fn_params = []; fn_ret = None;
            fn_body =
            { Ast.stmts =
              [Ast.StmtLet {is_mut = false;
                 pat =
                 (Ast.PatBind
                    { Ast.node = "n";
                      span =
                      { Ast.start = { Ast.line = 2; col = 9 };
                        stop = { Ast.line = 2; col = 10 } }
                      });
                 ty = None;
                 init =
                 (Ast.ExprMethodCall (
                    (Ast.ExprIdent
                       { Ast.node = "v";
                         span =
                         { Ast.start = { Ast.line = 2; col = 13 };
                           stop = { Ast.line = 2; col = 14 } }
                         }),
                    { Ast.node = "len";
                      span =
                      { Ast.start = { Ast.line = 2; col = 15 };
                        stop = { Ast.line = 2; col = 18 } }
                      },
                    []))};
                Ast.StmtLet {is_mut = false;
                  pat =
                  (Ast.PatBind
                     { Ast.node = "x";
                       span =
                       { Ast.start = { Ast.line = 3; col = 9 };
                         stop = { Ast.line = 3; col = 10 } }
                       });
                  ty = None;
                  init =
                  (Ast.ExprFieldAccess (
                     (Ast.ExprIdent
                        { Ast.node = "p";
                          span =
                          { Ast.start = { Ast.line = 3; col = 13 };
                            stop = { Ast.line = 3; col = 14 } }
                          }),
                     { Ast.node = "x";
                       span =
                       { Ast.start = { Ast.line = 3; col = 15 };
                         stop = { Ast.line = 3; col = 16 } }
                       }
                     ))};
                (Ast.StmtExpr
                   (Ast.ExprMethodCall (
                      (Ast.ExprIdent
                         { Ast.node = "v";
                           span =
                           { Ast.start = { Ast.line = 4; col = 5 };
                             stop = { Ast.line = 4; col = 6 } }
                           }),
                      { Ast.node = "push";
                        span =
                        { Ast.start = { Ast.line = 4; col = 7 };
                          stop = { Ast.line = 4; col = 11 } }
                        },
                      [(Ast.ExprLit (Ast.LitInt "1"))])))
                ];
              final_expr = None }
            })
        ]
      }
    |}]

let%expect_test "index expression" =
  parse_and_print "fn main() { let x = v[0]; }";
  [%expect
    {|
    { Ast.items =
      [(Ast.ItemFn
          { Ast.fn_pub = false;
            fn_name =
            { Ast.node = "main";
              span =
              { Ast.start = { Ast.line = 1; col = 4 };
                stop = { Ast.line = 1; col = 8 } }
              };
            fn_generics = []; fn_self = None; fn_params = []; fn_ret = None;
            fn_body =
            { Ast.stmts =
              [Ast.StmtLet {is_mut = false;
                 pat =
                 (Ast.PatBind
                    { Ast.node = "x";
                      span =
                      { Ast.start = { Ast.line = 1; col = 17 };
                        stop = { Ast.line = 1; col = 18 } }
                      });
                 ty = None;
                 init =
                 (Ast.ExprIndex (
                    (Ast.ExprIdent
                       { Ast.node = "v";
                         span =
                         { Ast.start = { Ast.line = 1; col = 21 };
                           stop = { Ast.line = 1; col = 22 } }
                         }),
                    (Ast.ExprLit (Ast.LitInt "0"))))}
                ];
              final_expr = None }
            })
        ]
      }
    |}]

let%expect_test "return and break" =
  parse_and_print {|fn f() -> i64 {
    if true { return 42; }
    0
}|};
  [%expect
    {|
    { Ast.items =
      [(Ast.ItemFn
          { Ast.fn_pub = false;
            fn_name =
            { Ast.node = "f";
              span =
              { Ast.start = { Ast.line = 1; col = 4 };
                stop = { Ast.line = 1; col = 5 } }
              };
            fn_generics = []; fn_self = None; fn_params = [];
            fn_ret =
            (Some (Ast.TyName
                     { Ast.node = "i64";
                       span =
                       { Ast.start = { Ast.line = 1; col = 11 };
                         stop = { Ast.line = 1; col = 14 } }
                       }));
            fn_body =
            { Ast.stmts =
              [(Ast.StmtExpr
                  (Ast.ExprIf ((Ast.ExprLit (Ast.LitBool true)),
                     { Ast.stmts =
                       [(Ast.StmtExpr
                           (Ast.ExprReturn (Some (Ast.ExprLit (Ast.LitInt "42")))))
                         ];
                       final_expr = None },
                     None)))
                ];
              final_expr = (Some (Ast.ExprLit (Ast.LitInt "0"))) }
            })
        ]
      }
    |}]

(* --- negative: syntax errors report line references --- *)

let%expect_test "error: missing closing brace" =
  parse_and_print "fn main() {\n  let x = 1;\n";
  [%expect {| Parse error at line 3, col 1: syntax error |}]

let%expect_test "let wildcard" =
  parse_and_print "fn main() { let _ = foo(); }";
  [%expect
    {|
    { Ast.items =
      [(Ast.ItemFn
          { Ast.fn_pub = false;
            fn_name =
            { Ast.node = "main";
              span =
              { Ast.start = { Ast.line = 1; col = 4 };
                stop = { Ast.line = 1; col = 8 } }
              };
            fn_generics = []; fn_self = None; fn_params = []; fn_ret = None;
            fn_body =
            { Ast.stmts =
              [Ast.StmtLet {is_mut = false; pat = Ast.PatWild; ty = None;
                 init =
                 (Ast.ExprCall (
                    (Ast.ExprIdent
                       { Ast.node = "foo";
                         span =
                         { Ast.start = { Ast.line = 1; col = 21 };
                           stop = { Ast.line = 1; col = 24 } }
                         }),
                    []))}
                ];
              final_expr = None }
            })
        ]
      }
    |}]

let%expect_test "error: missing semicolon" =
  parse_and_print "fn main() { let x = 1 }";
  [%expect {| Parse error at line 1, col 23: syntax error |}]

let%expect_test "error: bad token" =
  parse_and_print "struct Foo { @@ }";
  [%expect {| Lexer error at line 1, col 14: unexpected character: @ |}]

let%expect_test "error: missing fn body" =
  parse_and_print "fn foo()";
  [%expect {| Parse error at line 1, col 9: syntax error |}]
