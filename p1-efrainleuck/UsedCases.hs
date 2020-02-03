import Framework
import DigitRecognition
import Images

--Images used in various tests
img6 = validImages!!5
img11 = validImages!!10
img21 = validImages!!20
img3 = validImages!!2
img9 = validImages!!8
img55 = validImages!!54



-- Test values for showPixelImage
shownImage = validImages!!0
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
-- Test values for lookupVal
assocs1 = [('a', 8), ('b', 7), ('c', 9)]
assocs2 = zip [1..10] ['a'..'z']
assocs3 = [(1, "lookup"), (1, "conflicts"), (1, "are"), (1, "fun")] 
assocs4 = []

--Test values for buildCorpus
fullCorpus = buildCorpus trainingImages
corpusLengths = [(d, length i) | (d,i) <- fullCorpus]

--Test values for buildCorpus (full credit)
smallCorpus = buildCorpus (take 10 trainingImages)
smallCorpusLengths = [(d, length i) | (d,i) <- smallCorpus]

--Image list used for testing probOfFeature/probOfNoFeature
probFeaturesImages = take 20 $ map fst $ filter (\k -> snd k == 0) trainingImages
--Corpus used for testing rankDigit and classifyImage
rankClassifyCorpus = buildCorpus $ take 1000 trainingImages
