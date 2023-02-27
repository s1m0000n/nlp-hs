# Yet Unnamed Haskell NLP library

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
- Built-in optional parsing for punctuation, spaces, integral, floating point scalars
- Cleanup & fixes, such as detecting hyphens & safe word merging
- Extracted spans, pointing to a slice in source text
- Properties system which lets us propagate tags through the whole pipeline

##### Performance estimations

~ 2-3 times slower than the fast one with current optimizations in `package.yaml` and default configuration
Some ablation experiments have shown that the most important is agressive inlining.

### Bag of Words (& derived models)

- Performant Vector-based BoW model builder
- TF-IDF
- Token-level n-gram support

## Plans

- Fix export lists & add basic documentations on exported entities
- Add date & time, email, phone parsing
- Reimplement fast stemming | lematization or just integrate some ready solution
- Naive Bayes classifier (based on BoW) 
- Adopt & nativify some ML-library, add some examples (especially to do NBSVM)

