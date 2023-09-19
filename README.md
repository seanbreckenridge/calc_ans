# calc_ans

A CLI REPL calculator which remembers your last answer:

```python
> 5 + 10.5
15.5
# assumes the last answer as the left-hand side
> / 5
3.1
> ceil  # rounds up
4
> round (1 + sqrt(-4 ^ 4) * 5)
81
> (88 - ans)
7
> 4 + x
      ^
Unknown token in input: 'x'
```

If you don't provide an expression, it uses the last answer as the left-hand side.

Supports:

- `+`, `-`, `*`, `/`, `//` (floor divide) `^` (exponentiation), `%` (modulo), or their [aliases](#aliases)
- `ceil`, `floor`, `abs`, `round`, `sqrt`
- `epoch` (current time in seconds since the Unix epoch), `pi`
- `ans` (last answer)

This is optimized for my use-case on my phone with [`termux`](https://termux.dev/en/), so it has a fast boot time, and is forgiving with whitespace/parentheses, and has aliases for the basic operators, see [about](#about) for more details.

## Installation

Requires [`opam`](https://opam.ocaml.org/doc/Install.html) and `dune` (install with `opam install dune`)

```bash
git clone https://github.com/seanbreckenridge/calc_ans
cd calc_ans
make
```

Or manually:

```bash
dune build
cp _build/default/bin/main.exe ~/.local/bin/calc_ans
```

Installs `calc_ans` and `calc_ans_rlwrap` to `~/.local/bin/`.

`calc_ans_rlwrap` is a wrapper around `calc_ans` which uses [`rlwrap`](https://github.com/hanslub42/rlwrap) to provide lots of nice CLI functionality - a persistent history, tab completion, arrow keys, line-editing, history search with <CTRL+R>

I alias `calc_ans_rlwrap` to `c` in my shell:

```bash
alias c='calc_ans_rlwrap'
```

To install `opam`/`dune` to build this on `termux`, see [notes on installing opam](https://exobrain.sean.fish/devlog/ocaml_android/) and [dune issue](https://github.com/ocaml/dune/issues/8676#issuecomment-1722574714) for a patch.

## Usage

To exit the repl, can use `q`, `exit`, `quit`, or <CTRL+D>, or <CTRL+C>

`calc_ans -d` to enable debug mode:

```python
$ calc_ans -d
> 5 + 3
[DEBUG] Input: 5 + 3
[DEBUG] Prev result: None
[DEBUG] Tokens: <Num 5> <Op +> <Num 3>
[DEBUG] Postfix: 5 3 +
[DEBUG] Evaluating 5 + 3
8
> / (5 + 100)
[DEBUG] Input: / (5 + 100)
[DEBUG] Prev result: 8
[DEBUG] Tokens: <Op /> <Lparen> <Num 5> <Op +> <Num 100> <Rparen>
[DEBUG] Postfix: 5 100 + /
[DEBUG] Evaluating 5 + 100
[DEBUG] Evaluating 8 / 105
0.0761904761905
```

`calc_ans -e <expr>` to evaluate an expression:

```python
$ calc_ans -e 'round(2 ^ 20 / 5)'
209715
```

### Aliases

You can also use aliases instead of the symbols:

| Symbol | Alias  |
| ------ | ------ |
| `+`    | a, p   |
| `-`    | m      |
| `*`    | t      |
| `/`    | d      |

### Precision

This does not use any special integer/float to manage precision, so you get the default `2^63` precision for integers, and `IEEE 754` (64 bits) for floats. Don't use this for especially large numbers or calculations which require a lot of precision.

### About

This is really optimized for my use-case using [`termux`](https://termux.dev/en/) on my phone.

I had used [`ipython`](https://github.com/ipython/ipython) for a while, which is just a fancy REPL for python, but the boot time when using [`termux`](https://termux.dev/en/) on my phone was pretty slow. [eva](https://github.com/nerdypepper/eva) is pretty good, but `_` referring to the last answer always meant I had to switch keyboard layouts, and it wasnt the most foriving on calling functions without parentheses.

So, the goals for this were:

- fast boot time
- last answer is assumed as the left-hand side if an operand is missing
- aliases for the basic operators so I don't have to switch keyboard layouts
- being forgiving with whitespace/parentheses
- decent error messages

This was my first time ever writing `ocaml`, and I wanted to write something that I'd actually use. I knew it would be nice to write something related to parsing (it was indeed), and I've never handled tokenizing/parsing postfix notation manually (though I have used [antlr](https://www.antlr.org/) and [pest.rs](https://pest.rs/)).

This parses the tokens using `match` expressions, and then parses it into postfix notation using the [Shunting-yard algorithm](https://en.wikipedia.org/wiki/Shunting-yard_algorithm). Then, it evaluates the postfix notation using a stack, using your previous answer if its missing an operand.

Pretty happy with the error messages:

```
> 4 + x
      ^
Unknown token in input: 'x'
> / 5 + 1
  ^
Need two operands for operator
```

### Known issues

Parsing negative numbers vs. subtraction is a bit weird. Currently, if this finds a `-`, it does lookahead to figure out if its a negative number or subtraction. So, if you provide an expression like `4-3`, it parses it as two numbers `4` `-3`, instead of `4 - 3`. This can be fixed by using spaces between the input like `4 - 3`, or by using the subtraction alias `4m3`

```
> 4-3
Error: Malformed expression, multiple values without operator or function. Stack contents: 4 -3
> 4 - 3
1
> 4m3
1
```
