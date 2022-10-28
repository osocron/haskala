from abc import ABC
from typing import Generic, List, TypeVar
from functools import reduce

A = TypeVar('A')


class Monoid(Generic[A], ABC):

    def mempty(self) -> A:
        pass

    def mappend(self, a: A, b: A) -> A:
        pass

    def mconcat(self, xs: List[A]) -> A:
        return reduce(self.mappend, xs, self.mempty())


class MonoidInt(Monoid[int]):

    def mempty(self) -> int:
        return 0

    def mappend(self, a: int, b: int) -> int:
        return a + b


class MonoidList(Monoid[List[A]]):

    def mempty(self) -> List[A]:
        return []

    def mappend(self, a: List[A], b: List[A]) -> List[A]:
        return a + b


class MonoidString(Monoid[str]):

    def mempty(self) -> str:
        return ''

    def mappend(self, a: str, b: str) -> str:
        return a + b


monoid_int = MonoidInt()
print(monoid_int.mconcat([1, 2, 3]))

monoid_list = MonoidList()
print(monoid_list.mconcat([[1, 2], [3, 4]]))
