module Input where

readInputList :: String -> IO [String]
readInputList filename = lines <$> readFile filename

readInputListParsed :: (String -> a) -> String -> IO [a]
readInputListParsed parse filename = fmap parse <$> readInputList filename

withInput :: String -> (String -> IO a) -> (a -> b) -> IO b
withInput filename reader f = f <$> reader filename
