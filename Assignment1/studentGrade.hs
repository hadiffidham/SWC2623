-- Student Grade Analysis based on university_db

import Data.List (maximumBy)
import Data.Ord (comparing)

-- Define Student type
type Student = (String, String, [Double])
-- (StudentID, StudentName, Grades)

-- Student data from database
students :: [Student]
students =
    [ ("S1001", "Hadif Idham", [85, 90, 78])
    , ("S1002", "Akmal", [76, 82, 81])
    , ("S1003", "Hezry", [92, 94, 96, 91, 89, 85, 82])
    , ("S1004", "Hakimi", [88, 84, 85, 86, 90])
    , ("S1006", "Faliq", [91, 89, 93, 88, 90, 87, 86, 85])
    ]

-- Function to calculate average
average :: [Double] -> Double
average grades = sum grades / fromIntegral (length grades)

-- Calculate average for each student
studentAverages :: [(String, String, Double)]
studentAverages =
    map (\(id, name, grades) -> (id, name, average grades)) students

-- Find distinction students (> 80)
distinctionStudents :: [(String, String, Double)]
distinctionStudents =
    filter (\(_, _, avg) -> avg > 80) studentAverages

-- Find top student
topStudent :: (String, String, Double)
topStudent =
    maximumBy (comparing (\(_, _, avg) -> avg)) studentAverages

-- Main program
main :: IO ()
main = do
    putStrLn "=== Student Average Grades ==="
    mapM_ print studentAverages

    putStrLn "\n=== Distinction Students (Average > 80) ==="
    mapM_ print distinctionStudents

    putStrLn "\n=== Top Performing Student ==="
    print topStudent