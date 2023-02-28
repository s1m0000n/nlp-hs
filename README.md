# Hermes - fast & featureful Haskell NLP library

Natural Language Processing tools for Haskell with emphasis on performance

⚠️ **This library is very experimental & being actively developed with massive breaking changes**

## Features

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

### Bag of Words (& derived models)

- Performant Vector-based BoW model builder
- TF-IDF
- Token-level n-gram support

## Observations

### Typed tokenizer

- Longer & more type-diverse patterns are much faster as less cases are checked 

## Plans

- Fix export lists & add basic documentations on exported entities
- Add date & time, email, phone parsing
- Reimplement fast stemming | lematization or just integrate some ready solution
- Naive Bayes classifier (based on BoW) 
- Adopt & nativify some ML-library, add some examples (especially to do NBSVM)

## Project naming

[Hermes](https://en.wikipedia.org/wiki/Hermes) is a member of 12 Olympians in ancient Greek mythology, divine messenger & god of a lot of stuff, including eloquence & known for his ability to move quickly.
So, Hermes is a good collective image of what this library is moving torwards for: deeper language understanding, code expressiveness & performance.
In addition it just cool & do we want more ancient gods-libraries in Haskell! (yep, Aeson as inspiration)

