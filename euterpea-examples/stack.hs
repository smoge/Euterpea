
data StackOperation a where
    Push :: a -> StackOperation a
    Pop :: StackOperation a
    Transform :: (a -> a) -> StackOperation a
    Combine :: (a -> a -> a) -> StackOperation a
    
newtype Stack a = Stack [a]

pushStack :: a -> Stack a -> Stack a
popStack :: Stack a -> Maybe (a, Stack a)
transformTop :: (a -> a) -> Stack a -> Maybe (Stack a)

musicOperations :: [StackOperation (Music Pitch)]
musicOperations = [
    Push (c 4 qn),
    Push (e 4 qn),
    Combine (:+:),
    Transform (transpose 12)
]