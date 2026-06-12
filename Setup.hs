-- Regenerates the Futhark C library and futhask bindings before building,
-- whenever a .fut source is newer than the generated files.
import Control.Monad (unless, when)
import Data.List (isInfixOf, isSuffixOf)
import Distribution.Simple
import System.Directory
import System.Exit (ExitCode (..), die)
import System.FilePath ((</>))
import System.Process (createProcess, cwd, proc, waitForProcess)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { preBuild = \args flags -> generateBindings >> preBuild simpleUserHooks args flags
  , preRepl  = \args flags -> generateBindings >> preRepl  simpleUserHooks args flags
  }

modelsDir :: FilePath
modelsDir = "src" </> "Models"

generateBindings :: IO ()
generateBindings = do
  futs <- filter (".fut" `isSuffixOf`) <$> listDirectory modelsDir
  let outputs = ["Models.c", "Models.h", "Futhark.hs"]
  current <- do
    outsExist <- mapM (doesFileExist . (modelsDir </>)) outputs
    futharkDir <- doesDirectoryExist (modelsDir </> "Futhark")
    if and outsExist && futharkDir
      then do
        inTimes  <- mapM (getModificationTime . (modelsDir </>)) futs
        outTimes <- mapM (getModificationTime . (modelsDir </>)) outputs
        -- a Models.c compiled without --library (e.g. by testing entries
        -- with a standalone "futhark multicore") defines main and cannot
        -- be linked into the library
        isLibrary <- not . isInfixOf "int main(" <$> readFile (modelsDir </> "Models.c")
        pure (isLibrary && maximum inTimes <= minimum outTimes)
      else pure False
  unless current $ do
    mapM_ requireTool ["futhark", "futhask"]
    hasLib <- doesDirectoryExist (modelsDir </> "lib")
    when (not hasLib) $ run "futhark" ["pkg", "sync"]
    run "futhark" ["multicore", "--library", "Models.fut"]
    run "futhask" ["multicore", "Models.h", ".", "Models.Futhark"]
    -- futhask treats the module name as a literal file name, so move its
    -- output to where the Models.Futhark.* module hierarchy expects it
    removePathForcibly (modelsDir </> "Futhark")
    removePathForcibly (modelsDir </> "Futhark.hs")
    renameDirectory (modelsDir </> "Models.Futhark") (modelsDir </> "Futhark")
    renameFile (modelsDir </> "Models.Futhark.hs") (modelsDir </> "Futhark.hs")

requireTool :: String -> IO ()
requireTool tool = do
  found <- findExecutable tool
  case found of
    Just _  -> pure ()
    Nothing -> die $ tool ++ " not found in PATH (enter the dev shell: nix develop)"

run :: String -> [String] -> IO ()
run cmd args = do
  putStrLn $ unwords (cmd : args)
  (_, _, _, ph) <- createProcess (proc cmd args) { cwd = Just modelsDir }
  code <- waitForProcess ph
  case code of
    ExitSuccess   -> pure ()
    ExitFailure c -> die $ cmd ++ " failed with exit code " ++ show c
