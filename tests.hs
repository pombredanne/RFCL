-- tests.hs
-- Trevor Pottinger
-- Sun Aug 31 12:44:58 PDT 2014

import Test.HUnit

import qualified KMeans
import qualified Simple


-- kmeans basics
test_random_shuffle = TestCase (assertEqual "Random shuffling" [99,22,11,88,55] (KMeans.take_random 5 [11, 22, 33, 44, 55, 66, 77, 88, 99]))
test_pair_func = TestCase (assertEqual "Pair function" 8 (KMeans.pairF (*) (2, 4)))

simple_similarity = Simple.logSimilarity (-1/(2^10))
-- simple
test_simple_similarity1 = TestCase (assertEqual "Similarity 1" 1.0 (simple_similarity 4 4))
test_simple_similarity2 = TestCase (assertEqual "Similarity 2" 0.9993233275026507 (simple_similarity 4 5))
test_simple_similarity3 = TestCase (assertEqual "Similarity 3" 0.992034584292225 (simple_similarity 1409206263 1409202662))
test5 = TestCase (assertEqual "Cast to Weighted" 4.0 (Simple.castToWeighted 4))
test6 = TestCase (assertEqual "Cast to Raw" 4 (Simple.castToRaw 4.0))
test7 = TestCase (assertEqual "Addition" 5.0 (2.0 `Simple.add` 3.0))
test8 = TestCase (assertEqual "Multiplication" 6.0 (2.0 `Simple.mult` 3.0))
test9 = TestCase (assertEqual "Zero Sample" 0 Simple.zero)
test10 = TestCase (assertEqual "Zero Weighted Sample" 0.0 Simple.weighted_zero)

-- kmeans helpers
test11 = TestCase (assertEqual 
                    "Apply weights to sample vector" 
                    [5.5,11.0,16.5,11.0,27.5,11.0,16.5,44.0] 
                    (KMeans.apply_sim fromIntegral (*) [11, 22, 33, 22, 55, 22, 33, 88] [0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5]))
test12 = TestCase (assertEqual 
                    "Computes similarity between samples and centers" 
                    [[1.0,0.9975762753251197,0.9969426770891702],[0.9969426770891702,0.9975762753251197,1.0]]
                    (KMeans.similarity_matrix simple_similarity [11, 22, 33] [11, 33]))
test13 = TestCase (assertEqual 
                    "Computes sample weights from similarity"
                    [[0.3,0.19999999999999998,9.999999999999999e-2],[8.333333333333333e-2,0.16666666666666666,0.25]]
                    (KMeans.weighted_similarity [[0.9, 0.6, 0.3], [0.25, 0.5, 0.75]]))
test14 = TestCase (assertEqual 
                    "Average Similarity 1"
                    0.39177404288468176
                    (KMeans.avg_similarity (Simple.logSimilarity (-1/2^2)) [11, 22, 33, 44] [78, 53, 81, 13]))
test15 = TestCase (assertEqual 
                    "Average Similarity 2"
                    0.9991255196259243
                    (KMeans.avg_similarity simple_similarity [11, 22, 33, 44] [13, 23, 32, 42]))
test16 = TestCase (assertEqual 
                    "Average Similarity 3"
                    1.0
                    (KMeans.avg_similarity simple_similarity [11, 22, 33, 44] [11, 22, 33, 44]))

times = [ 1409119864 , 1409206260 , 1409202665 , 1409206262
        , 1409206268 , 1409119867 , 1409119861 , 1409202662
        , 1409119862 , 1409119865 , 1409206263 , 1409202667
        , 1409206269
        ] :: [Simple.SimpleInt]
-- kmeans complete
test_simple_kmeans1 = TestCase (assertEqual "Simple kmeans 1" [22] (KMeans.soft_kmeans Simple.funcs [11, 22, 33] 1))
test_simple_kmeans2 = TestCase (assertEqual 
                                "Simple kmeans 2" 
                                [1409157855,1409157855,1409157855]
                                (KMeans.soft_kmeans Simple.funcs times 3))

main = runTestTT $ TestList [ TestLabel "random_shuffle" test_random_shuffle
                            , TestLabel "pair_func" test_pair_func
                            , TestLabel "simple_similarity1" test_simple_similarity1
                            , TestLabel "simple_similarity2" test_simple_similarity2
                            , TestLabel "simple_similarity3" test_simple_similarity3
                            , TestLabel "test5" test5
                            , TestLabel "test6" test6
                            , TestLabel "test7" test7
                            , TestLabel "test8" test8
                            , TestLabel "test9" test9
                            , TestLabel "test10" test10
                            , TestLabel "test11" test11
                            , TestLabel "test12" test12
                            , TestLabel "test13" test13
                            , TestLabel "test14" test14
                            , TestLabel "test15" test15
                            , TestLabel "simple_kmeans1" test_simple_kmeans1
                            , TestLabel "simple_kmeans2" test_simple_kmeans2
                            ]

