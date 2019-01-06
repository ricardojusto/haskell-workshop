data Message = Message String String deriving (Eq, Show, Read)

f :: String -> String
f str = str ++ "!!"

transformMessage :: Message -> Message
transformMessage (Message chId body) = Message chId (f body)

reply :: String -> String
reply = (show . transformMessage . read)

interactLine :: (String -> String) -> IO ()
interactLine f = interact (unlines . (map f) . lines)
main = interactLine reply
