import System.IO  
import Data.List
import Data.Text.Internal.Search

main = do
	let someFuncs = ["printf", "strncmp", "strlen"]
	putStrLn "Enter the file you want to analyze: "
	file_name <- getLine
	handle <- openFile file_name ReadMode
	file <- hGetContents handle
	let content_data = lines file
	let lib = checkElm content_data "#include"
	let funcStuff = map (checkElm content_data) someFuncs
	print(lib)
	print(funcStuff)


isLib :: (String, Bool) -> String
isLib (line, val) = if val == True
	then line
	else "0"

checkElm :: [String] -> String -> [String]
checkElm content malFunc = values
	where 
		content_bool =  map (isSubsequenceOf malFunc) content
		_data = zip content content_bool 
		the_conent = map isLib _data
		values = filter (\x -> x /= "0") the_conent

