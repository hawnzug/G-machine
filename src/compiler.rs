use gmachine::*;
use heap::Heap;
use ast::{Name, Program, ScDef, Expr, LetEq};
use std::collections::HashMap;

pub fn compile(prog: Program) -> GmState {
    use self::Instruction::{Eval, PushGlobal, Print};
    let init_code = vec![Print, Eval, PushGlobal(String::from("main"))];
    let (heap, globals) = build_initial_heap(prog);
    GmState::new(init_code, heap, globals)
}

fn build_initial_heap(prog: Program) -> (GmHeap, GmGlobals) {
    use self::Instruction::*;
    let mut heap = Heap::new();
    let mut globals = HashMap::new();
    let primitives =
        vec![(String::from("+"),
              2,
              vec![Unwind, Pop(2), Update(2), Add, Eval, Push(1), Eval, Push(1)]),
             (String::from("-"),
              2,
              vec![Unwind, Pop(2), Update(2), Sub, Eval, Push(1), Eval, Push(1)]),
             (String::from("*"),
              2,
              vec![Unwind, Pop(2), Update(2), Mul, Eval, Push(1), Eval, Push(1)]),
             (String::from("/"),
              2,
              vec![Unwind, Pop(2), Update(2), Div, Eval, Push(1), Eval, Push(1)]),
             (String::from("=="),
              2,
              vec![Unwind, Pop(2), Update(2), Equal, Eval, Push(1), Eval, Push(1)]),
             (String::from("!="),
              2,
              vec![Unwind, Pop(2), Update(2), Noteq, Eval, Push(1), Eval, Push(1)]),
             (String::from("<"),
              2,
              vec![Unwind, Pop(2), Update(2), Lt, Eval, Push(1), Eval, Push(1)]),
             (String::from("<="),
              2,
              vec![Unwind, Pop(2), Update(2), Le, Eval, Push(1), Eval, Push(1)]),
             (String::from(">"),
              2,
              vec![Unwind, Pop(2), Update(2), Gt, Eval, Push(1), Eval, Push(1)]),
             (String::from(">="),
              2,
              vec![Unwind, Pop(2), Update(2), Ge, Eval, Push(1), Eval, Push(1)]),
             (String::from("negate"), 1, vec![Unwind, Pop(1), Update(1), Neg, Eval, Push(0)])];
    for (name, nargs, code) in primitives {
        let addr = heap.alloc(Node::Global(nargs, code));
        globals.insert(name, addr);
    }
    for scdef in prog {
        let (name, nargs, code) = compile_sc(scdef);
        let addr = heap.alloc(Node::Global(nargs, code));
        globals.insert(name, addr);
    }
    (heap, globals)
}

type GmEnvironment = HashMap<Name, usize>;

fn compile_sc(scdef: ScDef) -> (Name, usize, GmCode) {
    let len = scdef.args.len();
    let mut env: GmEnvironment = HashMap::new();
    for (i, arg) in scdef.args.into_iter().enumerate() {
        env.insert(arg, i);
    }
    (scdef.name, len, compile_r(scdef.body, env))
}

fn compile_r(expr: Expr, env: GmEnvironment) -> GmCode {
    use self::Instruction::{Unwind, Pop, Update};
    let mut init = vec![Unwind, Pop(env.len()), Update(env.len())];
    let mut compiled = compile_e(expr, env);
    init.append(&mut compiled);
    init
}

enum Flat {
    Pack(usize, usize, Vec<Expr>),
    Other(Expr),
}

fn flatten(mut expr: Expr) -> Flat {
    enum Temp {
        Cons,
        Other,
    }
    let mut flag = Temp::Other;
    {
        let mut count: usize = 0;
        let mut e = &expr;
        while let &Expr::EAp(ref e1, _) = e {
            count += 1;
            e = e1;
        }
        match e {
            &Expr::EConstr(_, a) => {
                if count == a {
                    flag = Temp::Cons;
                }
            }
            _ => {}
        }
    }
    match flag {
        Temp::Other => Flat::Other(expr),
        Temp::Cons => {
            let mut v = Vec::new();
            while let Expr::EAp(e1, e2) = expr {
                v.push(*e2);
                expr = *e1;
            }
            if let Expr::EConstr(t, a) = expr {
                Flat::Pack(t, a, v)
            } else {
                unreachable!()
            }
        }
    }
}

#[derive(Debug, Clone)]
enum Context {
    Strict,
    Lazy,
}

fn compile_e(expr: Expr, env: GmEnvironment) -> GmCode {
    use self::Instruction::*;
    let expr = match flatten(expr) {
        Flat::Other(expr) => expr,
        Flat::Pack(t, a, v) => {
            let mut init = vec![Pack(t, a)];
            for (n, e) in v.into_iter().enumerate().rev() {
                let new_env = env.clone()
                    .into_iter()
                    .map(|(name, i)| (name, i + n))
                    .collect();
                init.append(&mut compile_c(e, new_env));
            }
            return init;
        }
    };
    match expr {
        Expr::ENum(i) => vec![Pushint(i)],
        Expr::ELet(defs, box_expr) => compile_let(defs, *box_expr, env, Context::Strict),
        Expr::ELetrec(defs, box_expr) => compile_letrec(defs, *box_expr, env, Context::Strict),
        Expr::ECase(e, alts) => {
            let mut hm = HashMap::new();
            for (t, names, body) in alts {
                let n = names.len();
                let mut code = vec![Slide(n)];
                let mut new_env: GmEnvironment = env.clone()
                    .into_iter()
                    .map(|(name, i)| (name, i + n))
                    .collect();
                for (i, name) in names.into_iter().enumerate() {
                    new_env.insert(name, i);
                }
                code.append(&mut compile_e(body, new_env));
                code.push(Split(n));
                hm.insert(t, code);
            }
            let mut init = vec![Casejump(hm)];
            init.append(&mut compile_e(*e, env));
            init
        }
        _ => {
            let expr = if let Expr::EAp(e1, e2) = expr {
                let temp = *e1;
                let e1 = match temp {
                    Expr::EVar(name) => {
                        if name == "negate" {
                            let mut init = vec![Neg];
                            let mut compiled = compile_e(*e2, env);
                            init.append(&mut compiled);
                            return init;
                        } else {
                            Box::new(Expr::EVar(name))
                        }
                    }
                    Expr::EAp(e3, e4) => {
                        let e3 = match *e3 {
                            Expr::EVar(name) => {
                                let mut init = match name.as_ref() {
                                    "+" => vec![Add],
                                    "-" => vec![Sub],
                                    "*" => vec![Mul],
                                    "/" => vec![Div],
                                    "==" => vec![Equal],
                                    "!=" => vec![Noteq],
                                    "<=" => vec![Le],
                                    ">=" => vec![Ge],
                                    "<" => vec![Lt],
                                    ">" => vec![Gt],
                                    _ => vec![],
                                };
                                if init.is_empty() {
                                    Box::new(Expr::EVar(name))
                                } else {
                                    let new_env = env.clone()
                                        .into_iter()
                                        .map(|(name, i)| (name, i + 1))
                                        .collect();
                                    let mut compiled = compile_e(*e4, new_env);
                                    init.append(&mut compiled);
                                    let mut compiled = compile_e(*e2, env);
                                    init.append(&mut compiled);
                                    return init;
                                }
                            }
                            _ => e3,
                        };
                        Box::new(Expr::EAp(e3, e4))
                    }
                    _ => Box::new(temp),
                };
                Expr::EAp(e1, e2)
            } else {
                expr
            };
            let mut code = vec![Eval];
            let mut left = compile_c(expr, env);
            code.append(&mut left);
            code
        }
    }
}

fn compile_c(expr: Expr, env: GmEnvironment) -> GmCode {
    use self::Instruction::{Push, PushGlobal, Pushint, Mkap, Pack};
    let expr = match flatten(expr) {
        Flat::Other(expr) => expr,
        Flat::Pack(t, a, v) => {
            let mut init = vec![Pack(t, a)];
            for (n, e) in v.into_iter().enumerate().rev() {
                let new_env = env.clone()
                    .into_iter()
                    .map(|(name, i)| (name, i + n))
                    .collect();
                init.append(&mut compile_c(e, new_env));
            }
            return init;
        }
    };
    match expr {
        Expr::EVar(name) => {
            if env.contains_key(&name) {
                vec![Push(env[&name])]
            } else {
                vec![PushGlobal(name)]
            }
        }
        Expr::ENum(n) => vec![Pushint(n)],
        Expr::EAp(box_e1, box_e2) => {
            let new_env = env.clone()
                .into_iter()
                .map(|(name, i)| (name, i + 1))
                .collect();
            let mut compiled1 = compile_c(*box_e1, new_env);
            let mut compiled2 = compile_c(*box_e2, env);
            let mut init = vec![Mkap];
            init.append(&mut compiled1);
            init.append(&mut compiled2);
            init
        }
        Expr::ELet(defs, box_expr) => compile_let(defs, *box_expr, env, Context::Lazy),
        Expr::ELetrec(defs, box_expr) => compile_letrec(defs, *box_expr, env, Context::Lazy),
        _ => unreachable!(),
    }
}

fn compile_let(defs: Vec<LetEq>, expr: Expr, env: GmEnvironment, context: Context) -> GmCode {
    use self::Instruction::Slide;
    let len = defs.len();
    let mut code = vec![Slide(len)];
    let mut new_env: GmEnvironment = env.clone()
        .into_iter()
        .map(|(name, i)| (name, i + len))
        .collect();
    for (i, pair) in defs.iter().enumerate() {
        new_env.insert(pair.0.clone(), len - i - 1);
    }
    let mut code_expr = match context {
        Context::Strict => compile_e(expr, new_env),
        Context::Lazy => compile_c(expr, new_env),
    };
    code.append(&mut code_expr);
    for (n, (_, e)) in defs.into_iter().enumerate().rev() {
        let new_env = env.clone()
            .into_iter()
            .map(|(name, i)| (name, i + n))
            .collect();
        code.append(&mut compile_c(e, new_env));
    }
    code
}

fn compile_letrec(defs: Vec<LetEq>, expr: Expr, env: GmEnvironment, context: Context) -> GmCode {
    use self::Instruction::{Slide, Update, Alloc};
    let len = defs.len();
    let mut code = vec![Slide(len)];
    let mut new_env: GmEnvironment = env.into_iter()
        .map(|(name, i)| (name, i + len))
        .collect();
    for (i, pair) in defs.iter().enumerate() {
        new_env.insert(pair.0.clone(), len - i - 1);
    }
    let new_env_clone = new_env.clone();
    let mut code_expr = match context {
        Context::Strict => compile_e(expr, new_env),
        Context::Lazy => compile_c(expr, new_env),
    };
    code.append(&mut code_expr);
    for (n, (_, e)) in defs.into_iter().enumerate().rev() {
        let new_env = new_env_clone.clone();
        code.push(Update(n));
        code.append(&mut compile_c(e, new_env));
    }
    code.push(Alloc(len));
    code
}
