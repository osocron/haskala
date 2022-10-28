import unittest
from learning_haskell.src.effects.io import IO


class TestIO(unittest.TestCase):
    def test_io_unit(self):
        io: IO[int] = IO.unit(22)
        result = io.unsafe_run_sync()
        self.assertEqual(result, 22)
