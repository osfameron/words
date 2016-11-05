main :: IO ()
main = putStrLn (greet "World")

greeting = "Hello"

greet :: String -> String
greet who = greeting ++ ", " ++ who
