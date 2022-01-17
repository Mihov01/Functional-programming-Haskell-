import Data.List
main::IO()
main = do
    
    
    print $ hardestSubject [("John", "Maths", 5), ("Kennedy", "English", 2), ("Joe", "Programming", 4), ("Claudia", "Programming", 6), ("Sam", "Maths", 4), ("Jenn", "English", 2)] == "English"
    print $ hardestSubject [("John", "Maths", 5), ("Kennedy", "English", 5), ("Joe", "Programming", 4), ("Claudia", "Programming", 6), ("Sam", "Maths", 4), ("Jenn", "English", 5)] == "Maths"


type Student = String
type Subject = String
type Note = Double
type Record = (Student, Subject, Note) 


data1 =  [("John", "Maths", 5), ("Kennedy", "English", 2), ("Joe", "Programming", 4), ("Claudia", "Programming", 6), ("Sam", "Maths", 4), ("Jenn", "English", 2)]
data2 = [("John", "Maths", 5), ("Kennedy", "English", 5), ("Joe", "Programming", 4), ("Claudia", "Programming", 6), ("Sam", "Maths", 4), ("Jenn", "English", 5)]

hardestSubject :: [Record] -> Subject
hardestSubject xs = sub 
 where 
    ((sub,grade) : rest) =  sortOn (\ (__,n1) ->  n1 ) (getAllAverage xs)



getAllAverage :: [Record] -> [(Subject,Note)]
getAllAverage []=[]
getAllAverage ((na,s,n):xs) = (s,getAverage ((na,s,n) : filter (\ (na1,s1,n1)-> s1==s) xs)): getAllAverage xs

getAverage :: [Record] -> Double 
getAverage  d = (sum $ map (\ (_ , _ , n) -> n) d) / (fromIntegral  (length d))

