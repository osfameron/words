newHead :: [a] -> a
newHead [] = error "empty list"
newHead (x:_) = x

newTail :: [a] -> [a]
newTail [] = error "empty list"
newTail (_:xs) = xs
