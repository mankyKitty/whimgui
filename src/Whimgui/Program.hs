module Whimgui.Program
  ( Program (..)
  , buildProgram
  , attachShader
  , programParameter1
  , linkStatus
  , validateStatus
  , programInfoLog
  ) where

import           Control.Monad.IO.Class   (MonadIO, liftIO)

import           Control.Monad            (liftM, liftM2, unless)
import           System.Exit              (exitFailure)
import           System.IO                (hPutStrLn, stderr)

import           Foreign.Marshal          (alloca)
import           Foreign.Ptr              (castPtr)
import           Foreign.Storable         (peek)

import           Data.Coerce              (coerce)

import           Data.StateVar            (StateVar (..), get)

import qualified Graphics.GL              as GL

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as BS8
import qualified Data.ByteString.Internal as BS

import           Whimgui.Shader           (Shader (..))

newtype Program = Program GL.GLuint

attachShader :: MonadIO m => Program -> Shader -> m ()
attachShader (Program p) (Shader s) = GL.glAttachShader p s

buildProgram :: MonadIO m => Shader -> Shader -> m Program
buildProgram vs fs = do
  program <- GL.glCreateProgram

  attachShader (Program program) vs
  attachShader (Program program) fs

  GL.glLinkProgram program

  linkOK <- linkStatus program
  GL.glValidateProgram program

  status <- validateStatus program

  liftIO . unless (linkOK && status) $ do
    hPutStrLn stderr "GL.linkProgram error"
    plog <- programInfoLog program
    BS8.hPutStrLn stderr plog
    exitFailure

  GL.glUseProgram program
  pure $ Program program

programParameter1 :: GL.GLuint -> GL.GLenum -> StateVar GL.GLint
programParameter1 p parm = StateVar g s where
  g = alloca $ liftM2 (>>) (GL.glGetProgramiv (coerce p) parm) peek
  s = GL.glProgramParameteri (coerce p) parm

linkStatus :: MonadIO m => GL.GLuint -> m Bool
linkStatus p = (GL.GL_FALSE /=) `liftM` get (programParameter1 p GL.GL_LINK_STATUS)

validateStatus :: MonadIO m => GL.GLuint -> m Bool
validateStatus p = (GL.GL_FALSE /=) `liftM` get (programParameter1 p GL.GL_VALIDATE_STATUS)

programInfoLog :: MonadIO m => GL.GLuint -> m BS.ByteString
programInfoLog p = liftIO $ do
  l <- fromIntegral <$> get (programParameter1 p GL.GL_INFO_LOG_LENGTH)
  if l <= 1
    then return BS.empty
    else liftIO $ alloca $ \pl ->
      BS.createUptoN l $ \ps -> do
        GL.glGetProgramInfoLog p (fromIntegral l) pl (castPtr ps)
        return $ l-1
