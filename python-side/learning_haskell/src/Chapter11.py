from dataclasses import dataclass
from typing import TypeVar, Generic
from abc import ABC

T = TypeVar('T')


class Tree(ABC, Generic[T]):
    pass


@dataclass
class Leaf(Tree[T]):
    pass


@dataclass
class Node(Tree[T]):
    left: Tree[T]
    value: T
    right: Tree[T]


# def show_tree(tree: Tree[int]) -> str:
#     match tree:
#         case Leaf():
#             return 'LEAF'
#         case Node(left, v, right):
#             return f'{show_tree(left)} {v} {show_tree(right)}'
#         case _:
#             raise Exception('Invalid tree')


tree: Tree[int] = Node(Node(Leaf(), 2, Leaf()), 1, Leaf())


def print_hello(msg: str) -> None:
    print(msg)
