module School (School, add, empty, grade, sorted) where

import Data.List (sort)
import qualified Data.Map as Map

type School = Map.Map Int [String] 

add :: Int -> String -> School -> School
add gradeNum student school = Map.insertWith (++) gradeNum [student] school

empty :: School
empty = Map.empty

grade :: Int -> School -> [String]
grade gradeNum school = Map.findWithDefault [] gradeNum school

sorted :: School -> [(Int, [String])]
sorted school = map sortGrade $ Map.toAscList school 
  where sortGrade (grade, students) = (grade, sort students)
