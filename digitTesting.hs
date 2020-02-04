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

newBuildCorpus :: [(PixelImage, Digit)] -> Corpus
newBuildCorpus imgLbls = undefined


-- ex. buildCorpus imgLbls 
--           [(9, [ [[True, False]], [[False, True]] ]), (2, [[[False, False]]])]
-- creates corpus of labeled images.
buildCorpus :: [(PixelImage, Digit)] -> Corpus
buildCorpus imgLbls = [(digit, pixImgLst digit imgLbls) | digit <- corpusDig imgLbls]

-- creates a list of PixelImages labeled a specific digit.
pixImgLst :: Digit -> [(PixelImage, Digit)] -> [PixelImage]
pixImgLst dgtInpt imgLbls = [pixImg |(pixImg, digit) <- imgLbls, digit == dgtInpt]

pixVal :: Digit -> [(PixelImage, Digit)] -> [PixelImage]
pixVal digit imgLbls = undefined 

-- creates a list of the possible Digit labels; removing any duplicates.
corpusDig :: [(PixelImage, Digit)] -> [Digit]
corpusDig imgLbl = unique (sort [ snd dig | dig <- imgLbl ])

--probOfDigit :: Corpus -> Digit -> Rational
--probOfDigit corpus digit = outOf (imgPixLngh )

imgPixLngh :: Corpus -> Digit -> Int
imgPixLngh corpus dig = length(snd(imgTup corpus dig))

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


frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])


probOfFeature :: [PixelImage] -> Feature -> Rational
probOfFeature imgs ftr = undefined

-- calculates denominator for probOfFeature function
imgLength :: [PixelImage] -> Int
imgLength imgs = length imgs

-- outputs number equal to amount of times a value is repeated
-- in a given list of values of the same type.    
numTimesFound :: Ord a => a -> [a] -> Integer
numTimesFound _ [] = 0
numTimesFound x list = sum $ map (\a -> 1) $ filter (== x) list






