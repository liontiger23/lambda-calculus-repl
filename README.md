# Lambda calculus REPL

Read-Evaluate-Print-Loop for lambda calculus terms.

Entered terms are immediately evaluated using *normal order reduction*
(limited to 100 reduction steps).
Previously entered terms are remembered in `.lambda-history` with usual
command-line history support out-of-the-box.

## Usage

REPL can be started using `stack run` command which will build it if necessary:

```bash
$ stack run
λ> \x.x
~ λx.x
λ> (\x.x) A
~ (λx.x) A
~ A
λ> :q
```

To exit REPL enter `:q` or `:quit`.

## Syntax

```haskell
term ::= var | term term | lambda ident "." term | (term)

lambda ::= "\" | "λ"
var ::= ident
ident ::= letter (alphaNum | "'")+

letter ::= "a"-"z" | "A"-"Z"
alphaNum ::= letter | "0"-"9"
```

With usual notation conventions:

- Application is left associative  
  `a b c = (a b) c`
- Abstraction is right associative  
  `\x.\y.x = \x.(\y.x)`
- Consecutive abstractions can be combined (*not supported yet*)  
  `\x.\y.x = \x y.x`

### Comments

Haskell-style comments are supported in some capacity:

- Single line comments starting with `--`
- Multi-line or inline comments inside `{-` and `-}`

## Examples

> [!Important]
>
> Although often times parentheses and spaces can be omitted,
> some care is necessary when writing application of two variables.
> Because both spaces and parentheses may act as invisible "application"
> operator without which the two variables will "glue together".
>
> For example, `\x.xx` is constant term that returns *variable* `xx`,
> while `\x.x x` returns application of `x` to `x`.

### Reduction

```haskell
λ> (\z.(((\x. x z) u) ((\y. y z) v)))w
~ (λz.(λx.x z) u ((λy.y z) v)) w
~ (λx.x w) u ((λy.y w) v)
~ u w ((λy.y w) v)
~ u w (v w)
```

### Non-normalizing terms

```haskell
λ> (\x.x x)(\x.x x) -- Omega
~ (λx.x x) (λx.x x)
...
~ (λx.x x) (λx.x x)
~ (λx.x x) (λx.x x)
```

### Boolean

```haskell
λ> (\x.\y.x) T F -- True
~ (λx.λy.x) T F
~ (λy.T) F
~ T
λ> (\x.\y.y) T F -- False
~ (λx.λy.y) T F
~ (λy.y) F
~ F
```
