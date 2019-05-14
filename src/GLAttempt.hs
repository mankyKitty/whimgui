{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module GLAttempt (main) where

import           Control.Monad         (unless)

import           Foreign.C.Types       (CInt)

import qualified Graphics.GL           as GL
import qualified SDL

import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS

import           Whimgui.Program
import           Whimgui.Shader

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

-- uniformLocation :: MonadIO m => GL.GLuint -> String -> m GL.GLint
-- uniformLocation p s = liftIO $ withCString s (GL.glGetUniformLocation p . castPtr)

initResources :: ByteString -> ByteString -> IO Program
initResources fsSource vsSource = do
  -- compile vertex shader
  vs <- buildShaderFrom Vert vsSource
  -- Do it again for the fragment shader
  fs <- buildShaderFrom Frag fsSource

  buildProgram vs fs

escOrQuit :: SDL.Event -> Bool
escOrQuit (SDL.Event _ evt) = isAQuit evt
  where
    isAQuit = \case
      SDL.QuitEvent                                                                      -> True
      SDL.KeyboardEvent (SDL.KeyboardEventData _ _ _ (SDL.Keysym _ SDL.KeycodeEscape _)) -> True
      _                                                                                  -> False

draw :: Program -> IO ()
draw (Program prog) = do
    GL.glClear GL.GL_COLOR_BUFFER_BIT
    GL.glViewport 0 0 (fromIntegral screenWidth) (fromIntegral screenHeight)
    GL.glUseProgram prog
    GL.glDrawArrays GL.GL_TRIANGLES 0 3

main :: IO ()
main = do
  fragSrc <- BS.readFile "shaders/foo.frag"
  vertSrc <- BS.readFile "shaders/one_triangle.vert"

  SDL.initialize [SDL.InitVideo]

  window <- SDL.createWindow "SDL / OpenGL Example"
    SDL.defaultWindow { SDL.windowInitialSize = SDL.V2 screenWidth screenHeight
                      , SDL.windowOpenGL = Just SDL.defaultOpenGL
                      }

  SDL.showWindow window

  _ <- SDL.glCreateContext window
  prog <- initResources fragSrc vertSrc

  let loop = do
        events <- SDL.pollEvents

        GL.glClear GL.GL_COLOR

        draw prog

        SDL.glSwapWindow window

        unless (any escOrQuit events) loop

  loop

  SDL.destroyWindow window
  SDL.quit
