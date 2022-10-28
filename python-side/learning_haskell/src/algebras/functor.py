from abc import ABC, abc
from typing import Generic, List, TypeVar, Callable
from returns.primitives.hkt import Kind1, kinded

A = TypeVar('A')
B = TypeVar('B')
F = TypeVar('F')


@kinded
class Functor(Generic[F, A, B], ABC):

    @abc.abstractmethod
    def fmap(self, fun: Callable[[A], B], func: Kind1[F, A]) -> Kind1[F, B]:
        raise NotImplementedError


class ListFunctor(Functor[List, A, B]):
    def fmap(self, fun: Callable[[A], B], func: List[A]) -> List[B]:
        return list(map(fun, func))


listFunc = ListFunctor()
result = listFunc.fmap(lambda x: x + 1, [1, 2, 3])
print(result)
