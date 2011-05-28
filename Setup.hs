import Distribution.App
import Distribution.Simple

main :: IO ()
main = defaultMainWithHooks $ addAppHooks simpleUserHooks
