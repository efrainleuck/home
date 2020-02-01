
prod :: [Integer] -> Integer
prod lst =  head * (prod drop 1 lst)

prodEven :: [Integer] -> Integer
