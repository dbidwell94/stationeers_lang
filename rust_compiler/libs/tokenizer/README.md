# Tokenizer

## What is this?

The tokenizer is the first step in translating code written by humans into something a computer can understand and execute. Think of it like a spell-checker that breaks down a sentence into individual words and identifies what type of word each one is.

## What does it do?

This library takes raw source code (just text) and converts it into a series of **tokens** - meaningful pieces of code that are easier for the compiler to work with.

**Example:**

```
Input:  let x = 5 + 3;
Output: [TokenType::Keyword(Keyword::Let), TokenType::Identifier("x"), TokenType::Symbol(Symbol::Assign), TokenType::Number(5), TokenType::Symbol(Symbol::Plus), TokenType::Number(3), TokenType::Symbol(Symbol::Semicolon)]
```

## Why is this useful?

Instead of the compiler having to understand every possible way someone might write code, the tokenizer standardizes it into simple, categorized pieces. This makes the rest of the compiler's job much easier.

## Who should care?

- **Compiler developers** building language tools
- **Contributors** adding new language features or keywords
- **Anyone** working on parsing or analyzing code

## How does it work?

The tokenizer reads through your code character by character and groups them into meaningful tokens based on patterns:

- **Keywords** (`let`, `if`, `while`, etc.)
- **Identifiers** (variable/function names)
- **Numbers** (integers, floats)
- **Symbols** (operators and punctuation, like `+`, `-`, `=`, `;`, `{`, `}`)
- **Strings** and comments
