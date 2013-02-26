import GmlTokens
import Gml
import Machine
import qualified Data.Map as Map
import RenderOperator

main :: IO ()
main = do
       program <- getContents
       let cs = (parse . gmlLex) program
       stack <- execute render (Map.empty, cs) []
       putStrLn $ show stack
