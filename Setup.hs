import Distribution.Simple

-- import System.Directory
-- import System.FilePath

main :: IO ()
main = defaultMain

-- main :: IO ()
-- main =
--     defaultMainWithHooks
--         simpleUserHooks
--             { preBuild = \_ _ -> do
--                 listHaskellModules "src" >>= \x -> print x
--                 return (Nothing, [])
--             }
--
-- listHaskellModules :: FilePath -> IO [String]
-- listHaskellModules dir = do
--     files <- listDirectory dir
--     let hsFiles = filter (\f -> takeExtension f == ".hs" && f /= "Base.hs") files
--     return $ map dropExtension hsFiles
