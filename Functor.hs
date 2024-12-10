data Pair a = Pair a a deriving (Show, Eq)
data Labelled e a = Labelled e a deriving (Show, Eq)
data OneOrTwo a = One a | Two a a deriving (Show, Eq)
data Either e a = Left e | Right a deriving (Show, Eq)
data MultiTree a = Leaf | Node a [MultiTree a] deriving (Show, Eq)
data Stream a = Cons a (Stream a)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

--Pair представляет пару значений, к каждому из которых применяется функция.

instance Functor (Labelled e) where
  fmap f (Labelled e x) = Labelled e (f x)

--Labelled хранит метку и значение; функция применяется только к значению.

instance Functor OneOrTwo where
  fmap f (One x) = One (f x)
  fmap f (Two x y) = Two (f x) (f y)

--OneOrTwo представляет либо одно, либо два значения, к которым применяется функция.

instance Functor (Either e) where
  fmap _ (Left e) = Left e
  fmap f (Right x) = Right (f x)

--Either хранит либо значение, либо ошибку; функция применяется только к значению.

instance Functor MultiTree where
  fmap _ Leaf = Leaf
  fmap f (Node x children) = Node (f x) (map (fmap f) children)

--MultiTree представляет дерево, где функция применяется к узлам и рекурсивно к дочерним деревьям.

instance Functor Stream where
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

--Stream представляет бесконечный поток данных, к каждому элементу которого применяется функция.
