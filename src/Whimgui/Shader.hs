module Whimgui.Shader
  ( Shader (..)
  , ShaderType (..)
  , getShader
  , shaderInfoLog
  , buildShaderFrom
  ) where

import           Control.Monad.IO.Class   (MonadIO, liftIO)

import           System.Exit              (exitFailure)
import           System.IO                (hPutStrLn, stderr)

import           Control.Monad            (liftM)

import           Foreign.ForeignPtr       (withForeignPtr)
import           Foreign.Marshal          (alloca)
import           Foreign.Marshal.Array    (allocaArray)
import           Foreign.Ptr              (Ptr, castPtr, plusPtr)
import           Foreign.Storable         (peek, pokeElemOff)

import qualified Graphics.GL              as GL

import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as BS8
import qualified Data.ByteString.Internal as BS

import qualified Data.ByteString.Lazy     as BSL

data ShaderType
  = Frag
  | Vert
  deriving (Show)

newtype Shader = Shader GL.GLuint

getShader :: MonadIO m => Shader -> GL.GLenum -> m GL.GLint
getShader (Shader s) p = liftIO $ alloca $ \q -> GL.glGetShaderiv s p q >> peek q

shaderInfoLog :: MonadIO m => Shader -> m ByteString
shaderInfoLog (Shader s) = do
  l <- fromIntegral `liftM` getShader (Shader s) GL.GL_INFO_LOG_LENGTH
  if l <= 1
    then return BS.empty
    else liftIO $ alloca $ \pl ->
      BS.createUptoN l $ \ps -> do
        GL.glGetShaderInfoLog s (fromIntegral l) pl (castPtr ps)
        return $ l - 1

compileShader :: MonadIO m => Shader -> m ()
compileShader (Shader s) = GL.glCompileShader s

buildShaderFrom :: ShaderType -> ByteString -> IO Shader
buildShaderFrom stype src =
  let
    (err, glType) = case stype of
      Frag -> ("fragment", GL.GL_FRAGMENT_SHADER)
      Vert -> ("vertex", GL.GL_VERTEX_SHADER)

    chunks = BSL.toChunks (BSL.fromStrict src)

    go :: Shader -> Int -> [BS.ByteString] -> Ptr (Ptr GL.GLchar) -> Ptr GL.GLint -> IO ()
    go s i (BS.PS fp o l:cs) ps pl = do
      pokeElemOff pl i (fromIntegral l)
      withForeignPtr fp $ \p -> do
        pokeElemOff ps i (castPtr p `plusPtr` o)
        go s (i+1) cs ps pl

    go (Shader s) i [] ps pl =
      GL.glShaderSource s (fromIntegral i) ps pl

  in do
    s <- Shader <$> GL.glCreateShader glType

    allocaArray (length chunks) $ \ps ->
      allocaArray (length chunks) $ \pl ->
        go s 0 chunks ps pl

    compileShader s

    sOK <- (GL.GL_TRUE ==) `liftM` getShader s GL.GL_COMPILE_STATUS

    if sOK then pure s else do
      slog <- shaderInfoLog s
      hPutStrLn stderr ("Error in " <> err <> " shader\n")
      BS8.hPutStrLn stderr slog
      exitFailure
