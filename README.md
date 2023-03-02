# Hermes - fast & featureful Haskell NLP library

⚠️ **This library is very experimental & being actively developed with breaking changes**

Natural Language Processing tools for Haskell with emphasis on performance

Some examples are provided in `app/Main.hs` (to be moved).
High-level code to check out first is in `src/Pipeline.hs`

## Features

### Language detection

https://github.com/sigrlami/lanhunch is integrated to pipeline & used when `languages = Auto`. 
Whole language group based on writing is added when predicted. 
Considered unsafe & experimental, so disabled by default.
This library is to be replaced, because of very poor performance

### Tokenizing

Two tokenizers are being developed right now:
1. Fast tokenizer (with just splits on certain charset)
2. Typed tokenizer (many features added, specific groups, such as spaces & punct are Text)

#### Typed tokenizer

The motivation here is extending beyond the typical tokenization scope, enabling customization with still decent performance.

- "Typed tokenizer" - the one distinguising text, numbers, dates etc.
- Built-in optional parsing for punctuation, spaces, integral, floating point scalars, money (& more stuff comming soon)
- Cleanup & fixes, such as detecting hyphens & safe word merging, spaces merging
- Extracted spans, pointing to a slice in source text
- Properties system which lets us propagate tags through the whole pipeline

##### Performance estimations

~ 3 times slower than the fast one with current optimizations in `package.yaml` and default configuration
Some ablation experiments have shown that the most important is agressive inlining.

### Stemming

[NLP.Snowball](https://hackage.haskell.org/package/snowball-1.0.0.1/docs/NLP-Snowball.html) is integrated to pipeline

### Bag of Words (& derived models)

- Performant Vector-based BoW model builder
- TF-IDF
- Token-level n-gram support

## Complementary modules

- Good overview of other Haskell linguistics & NLP modules: https://wiki.haskell.org/Applications_and_libraries/Linguistics
- [Data Haskell](http://www.datahaskell.org/docs/community/current-environment.html) provides more info on general ML & stuff

### Machine learning

- Naive Bayes classifier with [Hext](https://hackage.haskell.org/package/hext)
- Classic algorithms with [HLearn](https://github.com/mikeizbicki/HLearn) 
- Deep Learning with [Grenade](https://hackage.haskell.org/package/grenade). It's planned to develop the lib in this way for now

## Observations

### Typed tokenizer

- Longer & more type-diverse patterns are much faster as less cases are checked 

## Plans

- Refactor some `Tok.hs` (`show`, `showText` problems, use text builder)
- Remove simplified config & simplify `Pipeline.hs`
- Improve language detection
- Provide a single coherent interface for common ML approaches (in context of NLP)
- Provide examples (separate `<project root>/examples`)
- Fix export lists & add basic documentations on exported entities
- Add date & time, email, phone parsing

## Project naming

[Hermes](https://en.wikipedia.org/wiki/Hermes) is a member of 12 Olympians in ancient Greek mythology, divine messenger & god of a lot of stuff, including eloquence & known for his ability to move quickly.
So, Hermes is a good collective image of what this library is moving torwards for: deeper language understanding, code expressiveness & performance.
In addition it just cool & do we want more ancient gods-libraries in Haskell! (yep, Aeson as inspiration)

