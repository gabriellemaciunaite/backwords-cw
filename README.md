---

# Backwords Coursework (CS141)

An implementation of the **Backwords** word game built in Haskell for the **CS141 Functional Programming** module at the University of Warwick. This coursework achieved a mark of **84/100**.

## Overview

The Backwords application consists of a core game engine, an interactive Terminal User Interface (TUI), and an automated AI move generator. The word list is "safedict_full.txt" from [InnovativeInventor's dict4schools project](https://github.com/InnovativeInventor/dict4schools/) intersected with the [Levidrome list](https://www.levidromelist.com/levidrome-list/dictionary) and excluding any words of length 2 or 1.

Key functional implementations include:
* **Board & Tile Logic**: Handling tile validation, bag distributions, and board state updates.
* **Word Scoring**: Validating dictionary words and calculating tile scores.
* **AI Move Selection**: Algorithmic choice generation balancing word scores and optimal tile management (vowel/consonant requests).

## Setup & Running

To compile the project run:

```bash
stack build

```


To launch the TUI application and play the Backwords game run:

```bash
stack run

```


To run the test suite:

```bash
stack test

```

Additional commands include `stack run ai` and `stack run total 100`.
