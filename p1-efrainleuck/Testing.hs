{-# LANGUAGE FlexibleInstances #-}

module Testing where
import Debug.Trace
import Data.Maybe
import DigitRecognition
import System.CPUTime
import Framework
import Control.Monad
import System.Console.ANSI
import Control.Exception
import Data.List
import Coloring
import Data.Either
import Data.Dynamic

class Testable a where
    makeLens :: a -> [(Digit, Int)]
    getInfo :: Digit -> a -> Dynamic

instance Testable ([(Digit, Int)], [(Digit, [[(Int, Int)]])]) where
    makeLens (dc, ds) = dc
    getInfo digit (dc, ds) = toDyn $ lookupVal digit ds

instance Testable ([(Digit, Int)], [(Digit, [(Int, Int)])]) where
    makeLens (dc, ds) = dc
    getInfo digit (dc, ds) = toDyn $ lookupVal digit ds

instance Testable [(Digit, [PixelImage])] where
    makeLens corpus = [(d, length i) | (d,i) <- corpus]
    getInfo digit corpus = toDyn $ lookupVal digit corpus

type TestCase = IO (Either String String)

-- Print functions take "quiet"
putGood True = putStr
putGood False = putGreen
putGoodLn True = putStrLn
putGoodLn False = putGreenLn
putGoodSc True = putStr --section header
putGoodSc False = putGreenLn 
putBad True = putStr
putBad False = putRed
putBadLn True = putStrLn
putBadLn False = putRedLn
   
-- Time an IO action
time :: IO t -> IO Double
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    return (diff :: Double)

assertTrue :: Bool -> String -> TestCase
assertTrue b errs =  do
    ret <- (try $ evaluate b :: IO (Either SomeException Bool))
    return $ case ret of
        Left ex -> Left $ "Failed with exception: " ++ (head $ lines $ show ex)
        Right False -> Left $ "Failed when checking " ++ errs ++ "."
        Right True -> Right "*"

assertApprox :: (Ord a, Fractional a) => a -> a -> (a -> String) -> String -> TestCase
assertApprox a c s errs =  do
    let approx x y = abs (x-y) < 0.1
    ret <- (try $ evaluate (a `approx` c) :: IO (Either SomeException Bool))
    return $ case ret of
        Left ex -> Left $ "Failed with exception: " ++ (head $ lines $ show ex)
        Right False -> Left $ intercalate "\n" ["Failed when checking " ++ errs ++ ".", "\t\tShould have been about: " ++ (s c), "\t\tBut got: " ++ (s a)]
        Right True -> Right "*"

assertEqual :: Eq a => a -> a -> (a -> String) -> String -> TestCase
assertEqual a c s errs =  do
    ret <- (try $ evaluate (a == c) :: IO (Either SomeException Bool))
    return $ case ret of
        Left ex -> Left $ "Failed with exception: " ++ (head $ lines $ show ex)
        Right False -> Left $ intercalate "\n" ["Failed when checking " ++ errs ++ ".", "\t\tShould have been: " ++ (s c), "\t\tBut got: " ++ (s a)]
        Right True -> Right "*"

assertError :: a -> (a -> String) -> String -> TestCase
assertError exp s errMsg = do
    ret <- (try $ evaluate exp )
    return $ case ret of
        Left ex -> let exStr = show (ex :: SomeException)
                   in if "Prelude" `isInfixOf` exStr || "Non-exhaustive patterns" `isInfixOf` exStr
                      then Left $ "Failed, returning the built-in exception: " ++ (head $ lines exStr)
                      else Right $ "\n\tProbably passed, error message: " ++ (head $ lines exStr )
        Right b -> Left $ "Failed, reason: " ++ errMsg ++ " : returned " ++ s b

printResult :: Bool -> Bool -> (Either String String)-> IO Bool
printResult q _ (Left err) = putBad q ("\n\t* "++ err) >> return False
printResult q False (Right pass) = putGood q ("\n\t"++pass) >> return True
printResult q True (Right pass) = putGood q pass >> return True


printTests :: Bool -> String -> [TestCase] -> [TestCase] -> IO Bool
printTests q label tests bonus = do
    if q then do
            results <- sequence tests
            let pass = all isRight results
            unless pass $ 
                do putStr (label++": ")
                   foldM_ (printResult q) True $ filter isLeft results
                   putStrLn ""
            opts <- sequence bonus
            let passO = all isRight opts
            unless passO $ 
                do putStrLn (label++" non-critical:")
                   foldM_ (printResult q) True $ filter isLeft opts
                   putStrLn ""
            return pass
        else do
            putStr (label++": ")
            results <- sequence tests
            foldM_ (printResult q) True results
            putStrLn ""
            unless (null bonus) $
              do putStr "\tFull-credit tests:"
                 opts <- sequence bonus
                 foldM_ (printResult q) True opts 
                 putStrLn ""
            return $ all isRight results

testAllFeatures :: Bool -> IO Bool
testAllFeatures q = printTests q "Testing allFeatures"
                [ assertEqual (length allFeatures) 784 show "that the length of allFeatures is 784"
                , assertTrue (0 `elem` allFeatures) "that allFeatures contains 0"
                , assertTrue (700 `elem` allFeatures) "that allFeatures contains 700"
                , assertTrue (allFeatures == [0..783]) "allFeatures is correct"
                ] []
    
testAllDigits :: Bool -> IO Bool
testAllDigits q = printTests q "Testing allDigits"
              [assertTrue (sort allDigits == (map read $ map (\x -> [x]) "0123456789")) "allDigits"] []

imageOutput = unlines ["                            ",
                       "                            ",
                       "                            ",
                       "                            ",
                       "                            ",
                       "                            ",
                       "                            ",
                       "      ######                ",
                       "      ################      ",
                       "      ################      ",
                       "            # ########      ",
                       "                  ###       ",
                       "                 ####       ",
                       "                 ####       ",
                       "                ####        ",
                       "                ###         ",
                       "                ###         ",
                       "               ###          ",
                       "              ####          ",
                       "              ###           ",
                       "             ####           ",
                       "            ####            ",
                       "           ####             ",
                       "           ####             ",
                       "          #####             ",
                       "          #####             ",
                       "          ###               ",
                       "                            "]

testShowPixelImage :: PixelImage -> Bool -> IO Bool
testShowPixelImage img q = printTests q "Testing showPixelImage" 
                       [  assertEqual (showPixelImage [[True, True], [True, False]]) ("##\n# \n") space "you display a small image properly"
                        , assertEqual (showPixelImage img) (imageOutput) space "you display a full-sized image properly"] []
                where space x = "\n"++x++"\n"

testLookupVal :: Bool -> IO Bool
testLookupVal q = printTests q "Testing lookupVal"
              [ assertEqual (lookupVal 'b' [('a', 8), ('b', 7), ('c', 9)]) 7 show "the given example."
              , assertEqual (lookupVal 10 (zip [1..10] ['a'..'z'])) 'j' show "that j is the 10th value in the alphabet"
              , assertEqual (lookupVal 'c' (zip ['a'..'z'] [1..10])) 3 show "that c is the 3rd value in the alphabet"]
              []

testLookupValFC q = printTests q "Testing lookupVal error handling:" 
              [ assertError (lookupVal 1 [(1, "lookup"), (1, "conflicts"), (1, "are"), (1, "fun")]) 
                show "lookupVal should throw an error when there is more than 1 result for a lookup!"
              , assertError ((lookupVal 1 []) :: String) 
                show "lookupVal should throw an error when the key is not in the list!"] []

testBuildCorpus :: [(PixelImage, Digit)] -> Bool -> IO Bool
testBuildCorpus trainingImages q = 
    let corpus = buildCorpus trainingImages
        lens = makeLens corpus
        lc v = (v,lookupVal v lens)
    in printTests q "Testing buildCorpus" 
         [ assertEqual (map lc [0..9]) (zip [0..9] [479, 563, 488, 493, 535, 434, 501, 550, 462, 495])
                      show "buildCorpus associates the right number of images with each digit"
         , assertEqual (length lens) 10 show "that buildCorpus has 10 entries"
         ]  []

testBuildCorpusFC trainingImages q =
    let smallCorpus = buildCorpus (take 10 trainingImages)
        slens = makeLens smallCorpus
        ls v = (v,lookupVal v slens)
    in printTests q "Testing buildCorpus with missing digits"  
         [ assertEqual (map ls [0,1,2,3,4,5,9]) [(0,1),(1,3),(2,1),(3,1),(4,2),(5,1),(9,1)] show "buildCorpus works on small inputs"
         , assertEqual (length slens) 7  show "buildCorpus doesn't generate empty associations"
         ] []

testProbOfDigit :: [(PixelImage, Digit)] -> Bool -> IO Bool
testProbOfDigit trainingImages q = 
    let corpus = buildCorpus trainingImages
        testDigit k = (k,probOfDigit corpus k)
    in printTests q "Testing probOfDigit" 
          [assertEqual (map testDigit [0..3])
                       [(0, 479 `outOf` 5000), (1, 563 `outOf` 5000), (2, 61 `outOf` 625), (3, 493 `outOf` 5000)]
                      show "probOfDigit for 0..3"
         , assertEqual (map testDigit [4..9])
                       [(4, 107 `outOf` 1000), (5, 217 `outOf` 2500), (6, 501 `outOf` 5000)
                       , (7, 11  `outOf`  100), (8, 231 `outOf` 2500), (9, 99  `outOf` 1000)]
                       show  "probOfDigit for 4..9"
          , assertEqual (sum [ probOfDigit corpus x | x <- [0..9] ]) 1 (show) "that the probabilites of all digits sums to 1"
          ] []

mbc xs d n = 
    let y = nub $ map snd $ xs
        i = take n $ map fst $ filter (\k -> snd k == d) xs
    in buildCorpus $ map (\x -> (x,0)) i



fd = (fromJust . fromDynamic)

testSmoothing :: Bool -> IO Bool
testSmoothing q = 
  catch ( 
    do 
    putStr "Testing smoothing: "
    let summary = fd $ getInfo 0 $ buildCorpus smoothingImgs
        features = [probOfFeature summary f | f <- [0..4]]
        complements = [probOfNoFeature summary f | f <- [0..4]]
        results = features ++ complements
        zeroProb = 0 `elem` results
        superProb = any (>1) results
        sumsToOne =  zipWith (+) features complements == [1,1,1,1,1] 
        noCollisions = ((sort features) == (sort $ nub features)) && ((sort complements) == (sort $ nub complements))
        testGaps f = let gaps = zipWith (-) f (tail f) in and $ zipWith (==) gaps (tail gaps)
        goodGaps = testGaps features && testGaps complements
    case (zeroProb, superProb, sumsToOne, goodGaps) of
        (True, _, _, _)       -> putBad q "No smoothing. (0/7)"
        (False, True, _, _)      -> putBad q "Poor smoothing. (1/7)"
        (False, False, False, _) -> putGood q "Good smoothing, but doesn't sum to 1. (4/7)"
        (False, False, True, False) -> putGood q "Good smoothing, but doesn't have even gaps. (5/7)"
        (False, False, True, True)  -> putGood q"Excellent smoothing! (7/7)"
    when (not q) $ putStrLn ""
    case (noCollisions, goodGaps) of
        (False, _)            -> if q then putStrLn " -2" else putRedLn "\tCollisions detected! -2 Points!"
        (True, False)         -> if q then putStrLn " -1" else putRedLn "\tNo collisions, but distance not preserved by smoothing! -1 Point!"
        (True, True)         -> putStrLn ""
    return True
    ) handler
  where handler :: SomeException -> IO  Bool
        handler ex = do putBadLn q ("Failed with exception: " ++ (head $ lines $ show ex))
                        return False
        smoothingImgs =
            let bottom = replicate 27 (replicate 28 True) 
                footer = replicate 24 True
                a = ([False, False, False, False] ++ footer):bottom
                b = ([False, False, False, True] ++ footer):bottom
                c = ([False, False, True , True] ++ footer):bottom
                d = ([False, True , True , True] ++ footer):bottom
            in [(img, 0) | img <- [a,b,c,d]] 

testProbOfFeature :: [(PixelImage, Digit)] -> Bool -> IO Bool
testProbOfFeature trainingImages q = 
    let zeroImages = fd $ getInfo 0 $ mbc trainingImages 0 20
    in printTests q "Testing probOfFeature" 
            [ assertApprox (probOfFeature zeroImages 10) 0 show "that probOfFeature is near 0 when no images have that feature (10)"
            , assertApprox (probOfFeature zeroImages 300) (17 `outOf` 20) show "probOfFeature on feature 300"
            ] []

testProbOfNoFeature :: [(PixelImage, Digit)] -> Bool -> IO Bool
testProbOfNoFeature trainingImages q = 
    let zeroImages = fd $ getInfo 0 $ mbc trainingImages 0 20 
    in printTests q "Testing probOfNoFeature" 
            [ assertApprox (probOfNoFeature zeroImages 10) 1 show "that probOfNoFeature is near 1 when no images have that feature (10)"
            , assertApprox (probOfNoFeature zeroImages 300) (3 `outOf` 20) show "probOfFeature on feature 300"
            ] []

testRankOfDigit :: [(PixelImage, Digit)] -> [PixelImage] -> [Digit] -> Bool -> IO Bool 
testRankOfDigit trainingImages validImages validLabels q = 
    let corpus = buildCorpus $ take 1000 trainingImages
        img1 = validImages!!5
        img2 = validImages!!10
        img3 = validImages!!20
    in printTests q "Testing rankOfDigit (preliminary)" 
            [ assertTrue (((rankOfDigit corpus 1 img1) > (rankOfDigit corpus 8 img1))) "rankOfDigit considers 1 more likely than 8 for 6th image"
            , assertTrue (((rankOfDigit corpus 0 img2) > (rankOfDigit corpus 4 img2))) "rankOfDigit considers 0 more likely than 4 for 11th image"
            , assertTrue (((rankOfDigit corpus 0 img2) > (rankOfDigit corpus 5 img2))) "rankOfDigit considers 0 more likely than 5 for 11th image"
            , assertTrue (((rankOfDigit corpus 9 img3) > (rankOfDigit corpus 1 img3))) "rankOfDigit considers 9 more likely than 1 for 21st image"
            ] []

testClassifyImage :: [(PixelImage, Digit)] -> [PixelImage] -> [Digit] -> Bool -> IO Bool 
testClassifyImage trainingImages validImages validLabels q = 
    let corpus = buildCorpus $ take 1000 trainingImages
        showRank k = "digit " ++ (show k) ++ " with rank " ++ (show $ rankOfDigit corpus k (validImages!!54))
    in printTests q "Testing classifyImage" 
            [ assertEqual (classifyImage corpus (validImages!!2)) 1 show "classifyImage guesses the 3rd image is 1"
            , assertEqual (classifyImage corpus (validImages!!10)) 0 show "classifyImage guesses the 11th image is 0"
            , assertEqual (classifyImage corpus (validImages!!20)) 9 show "classifyImage guesses the 21rd image is 9"
            ] 
            [ assertTrue (((classifyImage corpus (validImages!!8)) /= 5)) $  "classifyImage doesn't guesses the 9th image is 5 (requires smoothing)"
            , assertError (classifyImage (buildCorpus [(validImages!!0, 10)]) (validImages!!54))  -- THIS CASE NEEDS TO BE TESTED!
                           showRank "classifyImage shouldn't allow a maximum rank of 0 for 55th image (breaks without smoothing)"
            ]

testRankImage :: [(PixelImage, Digit)] -> [PixelImage] -> [Digit] -> Bool -> IO Bool 
testRankImage trainingImages validImages validLabels q =  
    let corpus = buildCorpus $ take 1000 trainingImages
    in printTests q "Testing rankImage (optional) and if rankImage is sorted (giga optional)" 
            [ assertEqual (length $ rankImage corpus (validImages!!2)) 10 show "rankImage"
            , assertTrue (let ranks = map snd $ rankImage corpus (validImages!!2) in reverse ranks == sort ranks)  "that rankImage is sorted"
            ] []



runTests :: [(PixelImage, Digit)] -> [PixelImage] -> [Digit] -> Bool -> Bool -> IO ()
runTests trainingImages validImages validLabels q force = 
     do milestone <- fmap and $ sequence $ 
                  [ testAllFeatures q
                  , testAllDigits q
                  , testLookupVal q
                  , testShowPixelImage (validImages!!0) q
                  ]
        if milestone
        then putGoodSc q  "You passed critical tests for the milestone.\n"
        else putBadLn q "Milestone not completed.\n"
        when (milestone || force) $ do
        buildCorp <- testBuildCorpus (trainingImages) q
        if buildCorp 
        then putGoodSc q "You passed critical tests for buildCorpus.\n"
        else putBadLn q "buildCorpus not completed.\n"
        when (buildCorp || force) $ do 
        project <- fmap and $ sequence $ 
                  [ testProbOfDigit (trainingImages) q
                  , testProbOfFeature (trainingImages) q
                  , testProbOfNoFeature (trainingImages) q
                  , testRankOfDigit (trainingImages) (validImages) (validLabels) q
                  , testClassifyImage (trainingImages) (validImages) (validLabels) q
                  ]
        if project 
        then putGoodSc q "You pass all critical tests for the core project.\n"
        else putBadLn q "Project not completed.\n"
        when (not q) $ putStrLn "Full credit tests:"
        fmap and $ sequence $
                  [ testLookupValFC q
                  , testBuildCorpusFC trainingImages q
                  ]
        when (project || force) $ do
        fmap and $ sequence $
            [ testSmoothing q
            ]
        --testRankImage trainingImages validImages validLabels q
        when (project && not q) $
                       do putGreenLn "\nTry running the classifier! For example:"
                          putStrLn $ intercalate "\n"
                              ["./classifier -q -v 2 -c 40" 
                              ,"\tYou should get about 85% accuracy."
                              ,"\tWithout full credit, it should take less than 15s."
                              ,"\tIf you remove the -q flag, it will probably take 30-60s."
                              ,"\tWith full credit, it will take about 5s, or about 7s without the -q flag."]
