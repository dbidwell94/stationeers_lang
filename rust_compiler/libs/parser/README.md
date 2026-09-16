# Parser

## What is this?

The parser is the second step in turning Slang code into something the compiler can use. If the tokenizer is like splitting a sentence into words, the parser is like reading those words and understanding the sentence structure.

## What does it do?

This library takes a stream of tokens from the tokenizer and organizes them into a structured tree of expressions.

That tree tells later compiler stages what the code actually means, not just what text appeared in what order.

**Example:**

```
Input tokens: [TokenType::Keyword(Keyword::Let), TokenType::Identifier("x"), TokenType::Symbol(Symbol::Assign), TokenType::Number(5), TokenType::Symbol(Symbol::Plus), TokenType::Number(3), TokenType::Symbol(Symbol::Semicolon)]
Output shape (simplified AST enums):
Expression::Declaration(
	Spanned("x"),
	Box::new(Expression::Binary(
		Spanned(BinaryExpression::Add(
			Box::new(Expression::Literal(Spanned(Literal::Number(5)))),
			Box::new(Expression::Literal(Spanned(Literal::Number(3))))
		))
	))
)
```

## Why is this useful?

Without a parser, the compiler would only see a flat list of tokens and would not know how pieces relate to each other.

The parser gives structure to code so the compiler can:

- Understand operator order (for example, multiplication before addition)
- Recognize blocks, loops, and conditionals
- Understand declarations like `let`, `const`, `fn`, and `device`
- Report syntax errors in a meaningful way

## Who should care?

- **Compiler developers** building language features
- **Contributors** adding new syntax or control flow rules
- **Anyone** debugging parse errors or AST shape issues

## How does it work?

The parser walks through tokens from left to right and applies language rules to build a tree.

At a high level, it does this:

- Reads the next token (and sometimes peeks ahead)
- Chooses which parse rule applies (declaration, expression, loop, function, etc.)
- Builds tree nodes with source span information
- Collects syntax errors and tries to recover so it can keep parsing
- Produces one root block expression containing the full program

In short: the tokenizer answers "what are these pieces?" and the parser answers "how do these pieces fit together?"
