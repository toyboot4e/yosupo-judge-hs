import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Control.Monad.Trans.State.Strict (State, StateT, evalState, evalStateT, execState, execStateT, runState, runStateT)
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Char8 qualified as BS
import Data.Char
import Data.Maybe
import Data.Vector.Unboxed qualified as U
import System.IO (stdout)

runIO :: StateT BS.ByteString IO a -> IO ()
runIO = (BS.getContents >>=) . evalStateT . void

int' :: (MonadState BS.ByteString m) => m Int
int' = state $ fromJust . BS.readInt . BS.dropWhile isSpace

endlBSB :: BSB.Builder
endlBSB = BSB.char7 '\n'

putBSB :: BSB.Builder -> IO ()
putBSB = BSB.hPutBuilder stdout

concatBSB :: (U.Unbox a) => (a -> BSB.Builder) -> U.Vector a -> BSB.Builder
concatBSB f = U.foldr' ((<>) . f) mempty

showLinesBSB :: U.Vector Int -> BSB.Builder
showLinesBSB = concatBSB ((<> endlBSB) . BSB.intDec)

main :: IO ()
main = runIO $ do
  t <- int'
  abs <- U.replicateM t $ liftA2 (+) int' int'
  liftIO $ putBSB $ showLinesBSB abs

