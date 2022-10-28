from typing import Generic, TypeVar, Callable
from dataclasses import dataclass
from abc import ABC

A = TypeVar('A')
B = TypeVar('B')
C = TypeVar('C')


class IO(Generic[A], ABC):

    @staticmethod
    def effect(f: Callable[..., A]) -> 'IO[A]':
        return Succeed(f)

    @staticmethod
    def unit(v: A) -> 'IO[A]':
        return Succeed(lambda: v)

    def map(self, f: Callable[[A], B]) -> 'IO[B]':
        return FlatMap(self, lambda a: IO.unit(f(a)))

    def bind(self, f: Callable[[A], 'IO[B]']) -> 'IO[B]':
        return FlatMap(self, f)

    def zip_right(self, that: 'IO[B]') -> 'IO[B]':
        return self.bind(lambda _: that)

    @staticmethod
    def map2(this: 'IO[A]', that: 'IO[B]', f: Callable[[A, B], C]) -> 'IO[C]':
        return this.bind(lambda a: that.map(lambda b: f(a, b)))

    @staticmethod
    def print_line(text: str) -> 'IO[None]':
        return IO.effect(lambda: print(text))

    def unsafe_run_sync(self) -> A:
        match self:
            case Succeed(eff):
                return eff()
            case FlatMap(io, cont):
                return cont(io.unsafe_run_sync()).unsafe_run_sync()


@dataclass(frozen=True)
class Succeed(Generic[A], IO[A]):
    effect: Callable[..., A]


@dataclass(frozen=True)
class FlatMap(Generic[A, B], IO[B]):
    io: IO[A]
    f: Callable[[A], IO[B]]
