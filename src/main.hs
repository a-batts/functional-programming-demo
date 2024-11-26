-- Write printAMessage here
printAMessage :: Show a => a -> IO ()
printAMessage x = print x

-- Write division here
division :: Double -> Double -> Maybe Double
division _ 0 = Nothing
division x y = Just (x / y)

-- Write factorial here
factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n - 1)

-- Write factList here
factList :: Int -> [Int]
factList n = map factorial [1..n]

-- Write merge here
merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

main = do
    -- Testing printAMessage
    printAMessage "\nHello World!\n\n"

    -- Testing division
    putStrLn "Testing division: \n"
    let z = division 1 2
    let w = division 1 0  -- Note: denominator is 0 for a valid test
    let g = division 6 2
    putStrLn $ "z: " ++ show z ++ ", w: " ++ show w ++ ", g: " ++ show g ++ "\n"

    -- Testing factorial
    putStrLn "\nTesting factorial: \n"
    let a = factorial 1
    let b = factorial 7
    putStrLn $ "a: " ++ show a ++ ", b: " ++ show b ++ "\n"

    -- Testing factList
    putStrLn "\nTesting factList: \n"
    let testList = factList 5
    putStrLn $ "testList: " ++ show testList ++ "\n"

    -- Testing merge
    putStrLn "\nTesting merge: \n"
    let merged = merge [1, 3, 6] [2, 4, 5, 6, 7]
    putStrLn $ "merge: " ++ show merged ++ "\n"