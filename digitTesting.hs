module DigitRecognition where
import Data.List (nub, sort)
--import Data.List.Split (chunksOf)
import Data.Tuple (swap)
import Data.Ratio (numerator, denominator)
import Data.Ratio ((%))
import Data.Map (fromListWith, toList)

unique :: Eq a  => [a] -> [a]
unique []       = []
unique (x : xs) = x : unique (filter (x /=) xs)


type PixelImage = [[Bool]]
type Feature = Int
type Digit = Integer
type Image = (Digit,[PixelImage])
type Corpus = [Image]


outOf :: Int -> Int -> Rational
outOf a b =  (fromIntegral a) % (fromIntegral b)



hasFeature :: PixelImage -> Feature -> Bool
hasFeature img ftr =
    let dim = length img
        row = img !! (ftr `div` dim)
        pixel = row !! (ftr `mod` dim)
    in pixel

allFeatures :: [Feature]
allFeatures = [0..783]

allDigits :: [Digit]
allDigits = [0..9]

-- Example: showPixelImage [[True, True], [True, False]]
--          "##\n# \n"
pix = [[True,True],[True,False]]
showPixelImage :: PixelImage -> String
showPixelImage img = unlines [pixelToString y |y <- img]

pixelToString :: [Bool] -> String
pixelToString x = [if pixel then '#' else ' ' |pixel <- x]

-- Example: lookupVal 'b' [('a', 8), ('b', 7), ('c', 9)]
--          7
lookupVal :: (Eq a) => a -> [(a, b)] -> b
lookupVal key lst = head [snd comm |comm <- lst, fst comm == key]

-- Example:  
imgA = [[True, False]]
imgB = [[False, False]]
imgC = [[False, True]]

tstImgLbls = [(imgA, 9), (imgB, 2), (imgC, 9)]

-- ex. buildCorpus imgLbls 
--           [(9, [ [[True, False]], [[False, True]] ]), (2, [[[False, False]]])]
-- creates corpus of labeled images.
buildCorpus :: [(PixelImage, Digit)] -> Corpus
buildCorpus imgLbls = [(digit, pixImgLst digit imgLbls) | digit <- corpusDig imgLbls]

-- creates a list of PixelImages labeled a specific digit.
pixImgLst :: Digit -> [(PixelImage, Digit)] -> [PixelImage]
pixImgLst dgtInpt imgLbls = [pixImg |(pixImg, digit) <- imgLbls, digit == dgtInpt]

-- creates a list of the possible Digit labels; removing any duplicates.
corpusDig :: [(PixelImage, Digit)] -> [Digit]
corpusDig imgLbl = unique (sort [ snd dig | dig <- imgLbl ])

-- list of all digit labels within a corpus.
corpDigitLbls :: Corpus -> [Digit]
corpDigitLbls corpus = [fst digitLst | digitLst <- corpus]

--returns a tuple of type Image, where the fst elem of the tuple is == to the input digit.
imgTup :: Corpus -> Digit -> Image
imgTup corpus digit
    | digit `elem` (corpDigitLbls corpus) = head [ tup | tup <- corpus , fst tup == digit]
    | otherwise = error "Digit label not part of corpus."

-- creates a list of the pixel images in a specific digit label.
listOfPixImg :: Corpus -> Digit -> [PixelImage]
listOfPixImg corpus digit
    | logicTest corpus digit = snd (imgTup corpus digit)
    | otherwise = []

-- checks to see if a desired digit is a digit label contained in a corpus.
-- used in listOfPixImg
logicTest :: Corpus -> Digit -> Bool
logicTest corpus digit = digit `elem` corpDigitLbls corpus

--used as the denominator for the probOfDigit function.
numberOfAllPixelImages :: Corpus -> Int
numberOfAllPixelImages corpus = length (listOfAllPixelImages corpus)

listOfAllPixelImages :: Corpus -> [PixelImage]
listOfAllPixelImages corpus =
    (listOfPixImg corpus 1) ++ (listOfPixImg corpus 2) ++ (listOfPixImg corpus 3)
    ++ (listOfPixImg corpus 4) ++ (listOfPixImg corpus 5) ++ (listOfPixImg corpus 6)
    ++ (listOfPixImg corpus 7) ++ (listOfPixImg corpus 8) ++ (listOfPixImg corpus 9)

-- used as the numerator for the probOfDigit function.
numberOfDigPix :: Corpus -> Digit -> Int
numberOfDigPix corpus digit = length (listOfPixImg corpus digit)

probOfDigit :: Corpus -> Digit -> Rational
probOfDigit corpus digit = if digit `elem` corpDigitLbls corpus
                           then outOf (numberOfDigPix corpus digit) (numberOfAllPixelImages corpus)
                           else error "Input digit not part of digit labels in corpus."


--frequency :: (Ord a) => [a] -> [(a, Int)]
--frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])


probOfFeature :: [PixelImage] -> Feature -> Rational
probOfFeature imgs ftr = 
    outOf (fracNumer (bytListBlk (logicList imgs ftr))) (fracTotal imgs)

probOfNoFeature :: [PixelImage] -> Feature -> Rational
probOfNoFeature imgs ftr = 
    outOf (fracNumer (bytListWht (logicList imgs ftr))) (fracTotal imgs)

-- used as numerator for probOfFeature function.
-- calculates the total number of values that meet a specific condition.
fracNumer :: [Int] -> Int
fracNumer lst = sum lst

-- used as denominator for probOfFeature function.
-- calculates total number of pixel images in a list of pixel images.
fracTotal :: [PixelImage] -> Int
fracTotal imgs = length imgs

-- runs through a list of pixel images, goes through each image and searches
-- a specific index (feature), then creates a list of the data found in the
-- specific index of each image.
logicList :: [PixelImage] -> Feature -> [Bool]
logicList imgs ftr = [ hasFeature img ftr | img <- imgs]

-- calculates the number of True values; where if the value is True, then the 
-- feature is black.
bytListBlk :: [Bool] -> [Int]
bytListBlk lst = [ if y then 1 else 0 | y <- lst]

-- calculates same thing as bytListBlk but for if the values are False.
bytListWht :: [Bool] -> [Int]
bytListWht lst = [ if y then 0 else 1 | y <- lst]


rankOfDigit :: Corpus -> Digit -> PixelImage -> Rational
rankOfDigit corpus digit newImg = 
    undefined

classifyImage :: Corpus -> PixelImage -> Digit
classifyImage corpus newImg = 
    undefined

listOfImg :: Corpus -> [PixelImage]
listOfImg corpus = concat[ snd corp | corp <- corpus]

helpr :: Corpus -> Digit -> [Rational]
helpr corpus digit = [ if hasFeature pixImg ftr then probOfFeature pixImgLst ftr else probOfNoFeature pixImgLst ftr | pixImg <- listOfPixImg corpus digit, pixImgLst <- [listOfPixImg corpus digit], ftr <- allFeatures]

listOfIndex :: PixelImage -> [Feature]
listOfIndex img =   let leng = length(concat img) - 1
                in [0..leng]

listOfFtrVals :: PixelImage -> [Bool]
listOfFtrVals img = [ hasFeature img ftr | ftr <- listOfIndex img]

nestedToList :: Corpus -> Digit -> PixelImage
nestedToList corpus digit = concat(lookupVal digit corpus)

testFun corpus digit newImg = let ftr = listOfIndex newImg
                                  img = nestedToList corpus digit
                              in [ if hasFeature img ftr then probOfFeature img ftr else probOfNoFeature]