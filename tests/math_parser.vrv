type result {
  Result(int, string)
}

fn _parse_factor(sum: int, expr: string) -> result {
  if (!count(expr)) Result(sum, expr)
  else let c = at(expr, 0) {
    if (c >= '0' && c <= '9') {
      _parse_factor(10 * sum + c - '0', substr(expr, 1))
    } else Result(sum, expr)
  }
}

fn parse_factor(e: string) -> result {
  _parse_factor(0, e)
}

fn _eval_term(pair: result) -> result {
  let Result(lhs, expr) = pair {
    if (!count(expr)) pair
    else let lookahead = at(expr, 0)
          expr = substr(expr, 1) {
      if (lookahead == '*' || lookahead == '/') {
        let Result(rhs, expr) = parse_factor(expr) {
          let res = if (lookahead == '*') lhs * rhs else lhs / rhs {
            _eval_term(Result(res, expr))
          }
        }
      } else pair
    }
  }
}

fn eval_term(expr: string) -> result {
  _eval_term(parse_factor(expr))
}

fn _eval_expr(pair: result) -> result {
  let Result(lhs, expr) = pair {
    if (!count(expr)) pair
    else let lookahead = at(expr, 0)
             expr = substr(expr, 1) {
      if (lookahead == '+' || lookahead == '-') {
        let Result(rhs, expr) = eval_term(expr) {
          let res = if (lookahead == '+') lhs + rhs else lhs - rhs {
            _eval_expr(Result(res, expr))
          }
        }
      } else pair
    }
  }
}

fn eval_expr(e: string) -> result {
  _eval_expr(eval_term(e))
}

fn eval(e: string) -> int {
  let Result(res, _) = eval_expr(e) {
    res
  }
}

print(eval("22"))
print(eval("22+22"))
print(eval("22-11"))
print(eval("10+51-17"))
print(eval("3*5+10"))
print(eval("10+3*5+10"))
