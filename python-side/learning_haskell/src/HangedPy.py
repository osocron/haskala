from effects.io import IO
from typing import List, Optional
from random import randint
from dataclasses import dataclass
from functools import partial

WordList = List[str]


def all_words() -> IO[WordList]:
    return IO.effect(lambda: open("src/resources/words.txt")) \
        .map(lambda f: (f.read().split(), f)) \
        .map(lambda t: (t[0], t[1].close())) \
        .map(lambda t: t[0])


min_word_length: int = 5
max_word_length: int = 9


def game_words() -> IO[WordList]:
    return all_words() \
        .map(partial(filter, lambda x: min_word_length <= len(x) <= max_word_length))\
        .map(list)


def random_word(words: WordList) -> IO[str]:
    return IO.unit(words[randint(0, len(words) - 1)])


def select_random_word() -> IO[str]:
    return game_words() \
        .bind(lambda ws: random_word(ws))


@dataclass(frozen=True)
class Puzzle:
    word: str
    filled_in: List[Optional[str]]
    guessed: List[str]


def show_puzzle(p: Puzzle) -> str:
    return f"""{' '.join(['_' if c is None else c for c in p.filled_in])}""" + \
           f""" Guessed so far: {"".join(p.guessed)}"""


def fresh_puzzle(word: str) -> Puzzle:
    return Puzzle(word, [None] * len(word), [])


def char_in_word(p: Puzzle, c: str) -> bool:
    return c in p.word


def already_guessed(p: Puzzle, c: str) -> bool:
    return c in p.guessed


def fill_in_the_character(p: Puzzle, c: str) -> Puzzle:
    return Puzzle(p.word,
                  [c if c == t[0] else t[1] for t in zip(p.word, p.filled_in)],
                  p.guessed + [c])


def match_guess(p: Puzzle, c: str, incorrect_guesses: int) -> IO[tuple[Puzzle, int]]:
    match(char_in_word(p, c), already_guessed(p, c)):
        case(True, _):
            return IO.print_line(text=f'You guessed correctly!') \
                .map(lambda _: (fill_in_the_character(p, c), incorrect_guesses))
        case(_, True):
            return IO.print_line(text=f'You already guessed {c}') \
                .map(lambda _: (p, incorrect_guesses))
        case _:
            return IO.print_line(text=f'You guessed incorrectly!') \
                .map(lambda _: (fill_in_the_character(p, c), incorrect_guesses + 1))


def handle_guess(p: Puzzle, c: str, incorrect_guesses: int) -> IO[tuple[Puzzle, int]]:
    return IO.print_line(f'Your guess: {c}') \
        .bind(lambda _: match_guess(p, c, incorrect_guesses))


def handle_game_over(p: Puzzle, out_of_guesses: bool) -> IO[bool]:
    if out_of_guesses:
        return IO.print_line(f"You've run out of guesses, the word was: {p.word}") \
            .map(lambda _: True)
    else:
        return IO.unit(False)


def game_over(p: Puzzle, incorrect_guesses: int) -> IO[bool]:
    if incorrect_guesses == len(p.word):
        return IO.print_line(f"You've run out of guesses, the word was: {p.word}") \
            .map(lambda _: True)
    else:
        return IO.unit(False)


def game_win(p: Puzzle) -> IO[bool]:
    if all(c is not None for c in p.filled_in):
        return IO.print_line(f"You've guessed the word: {p.word} !!!") \
            .map(lambda _: True)
    else:
        return IO.unit(False)


def handle_user_input(user_input: str, p: Puzzle, incorrect_guesses: int) -> IO[tuple[Puzzle, int]]:
    match user_input:
        case s if len(s) == 1:
            return handle_guess(p, s, incorrect_guesses).bind(lambda t: run_game(t[0], t[1]))
        case _:
            return IO.print_line("Please enter a single character")\
                .zip_right(run_game(p, incorrect_guesses))


def read_prompt(p: Puzzle, incorrect_guesses: int) -> IO[tuple[Puzzle, int]]:
    return IO.effect(lambda: input('Please guess a letter:'))\
        .bind(lambda user_input: handle_user_input(user_input, p, incorrect_guesses))


def handle_should_continue(p: Puzzle, incorrect_guesses: int) -> IO[tuple[Puzzle, int]]:
    return IO.print_line(f"Current puzzle is: {show_puzzle(p)}")\
        .zip_right(read_prompt(p, incorrect_guesses))


def run_game(p: Puzzle, incorrect_guesses: int) -> IO[tuple[Puzzle, int]]:
    should_stop: IO[bool] = IO.map2(
        game_over(p, incorrect_guesses), game_win(p), lambda a, b: a or b)
    return should_stop.bind(
        lambda b: IO.unit((p, incorrect_guesses)) if b else handle_should_continue(
            p, incorrect_guesses)
    )


def run_main() -> IO[int]:
    return IO.print_line("Welcome to Hangman!")\
        .zip_right(IO.print_line("Guess a letter to play!"))\
        .zip_right(select_random_word())\
        .bind(lambda word: IO.unit(fresh_puzzle(word)))\
        .bind(lambda puzzle: run_game(puzzle, 0))\
        .map(lambda _: 0)


run_main().unsafe_run_sync()
