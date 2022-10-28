module Chapter11 where

-- Int ? -2147483648 => 2147483647
-- Bool -> True or False = 2
-- String -> Too many
-- Short -> -32768 => 32767

-- 1 + 1 = 2
data MBool = MTrue | MFlase

-- -2147483648 => 2147483647 -> A
-- -2147483648 => 2147483647 -> B
-- Total of inhabitants = A * B
data Geom = Point Int Int | Polygon [Int]

data HouseNumber = PositiveInt

data StreetNames = StreetNames

data AddressRepr = AddressRepr HouseNumber StreetNames

-- T = * -> * -> *

-- O(log n) vs O(n)
-- Node - (Node (Leaf 2) (10) (Leaf 13)) (23) (Leaf 55)
-- insert 22 tree ->
data MyMaybe a = MyJust a | MyNone deriving (Show)

mymaybe = MyJust 8

-- sealed trait Tree[A]
-- case class Node[A](right: Tree[A], value: A) extends Tree[A]
-- case class Leaf[A](value: A) extends Tree[A]

-- sealed trait MyMaybe[A]
-- case class MyJust[A](value: A) extends MyMaybe[A]
-- case object MyNone extends MyMaybe[Nothing]

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Eq, Ord, Show)

myTree :: Tree Integer
myTree = Node Leaf 15 Leaf

insert :: (Ord a, Eq a) => Tree a -> a -> Tree a
insert (Node left currentValue right) newValue
  | currentValue == newValue = undefined
  | currentValue > newValue = undefined
  | otherwise = undefined
insert Leaf newValue = undefined

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Leaf = Leaf
mapTree f (Node left value right) = Node (mapTree f left) (f value) (mapTree f right)

preorder :: Tree a -> [a]
preorder Leaf = []
preorder (Node left value right) = [value] ++ preorder left ++ preorder right

inorder :: Tree a -> [a]
inorder Leaf = []
inorder (Node left value right) = inorder left ++ [value] ++ inorder right

postorder :: Tree a -> [a]
postorder Leaf = []
postorder (Node left value right) = postorder left ++ postorder right ++ [value]

testTree :: Tree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
    then putStrLn "Preorder fine!"
    else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
    then putStrLn "Inorder fine!"
    else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
    then putStrLn "Postorder fine!"
    else putStrLn "Bad news bears"

foldTree ::
  (a -> b -> b) ->
  b ->
  Tree a ->
  b
foldTree _ acc Leaf = acc
foldTree f acc (Node left value right) =
  foldTree f (f value (foldTree f acc left)) right

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x =
  case f x of
    Nothing -> []
    Just (a, b) -> a : myUnfoldr f b

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\a -> Just (a, f a))

unfold :: (a -> Maybe (a, b, a)) -> a -> Tree b
unfold f x =
  case f x of
    Nothing -> Leaf
    Just (a, b, c) -> Node (unfold f a) b (unfold f c)

treeBuild :: Integer -> Tree Integer
treeBuild n = unfold (\x -> if x < n then Just (x + 1, x, x + 1) else Nothing) 0
