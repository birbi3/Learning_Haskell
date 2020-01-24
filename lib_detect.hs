import System.IO  
import Data.List

main = do
	putStrLn "Enter the file you want to analyze: "
	file_name <- getLine
	handle <- openFile file_name ReadMode
	file <- hGetContents handle
	let content_data = lines file
	let content_bool = map (isSubsequenceOf "#include") content_data
	let _data = zip content_data content_bool
	let libs = map isLib _data
	let lib = filter (\x -> x /= "0") libs
	print(lib)


isLib :: (String, Bool) -> String
isLib (line, val) = if val == True
	then line
	else "0"

