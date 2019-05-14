{-# LANGUAGE LambdaCase #-}
--port of https://github.com/bergey/haskell-OpenGL-examples

{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module OpenGLExample (main) where

import           Control.Monad
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as BS
import qualified Data.Vector.Storable      as V
import           Foreign.C.Types
import           SDL.Vect
import           System.Exit               (exitFailure)
import           System.IO

import qualified Graphics.Rendering.OpenGL as GL

import           SDL                       (($=))
import qualified SDL

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

escOrQuit :: SDL.Event -> Bool
escOrQuit (SDL.Event _ evt) = isAQuit evt
  where
    isAQuit = \case
      SDL.QuitEvent                                                                      -> True
      SDL.KeyboardEvent (SDL.KeyboardEventData _ _ _ (SDL.Keysym _ SDL.KeycodeEscape _)) -> True
      _                                                                                  -> False

main :: IO ()
main = do
  fragSrc <- BS.readFile "shaders/foo.frag"
  -- fragSrc <- BS.readFile "shaders/first_raymarch.frag"
  vertSrc <- BS.readFile "shaders/basic.vert"

  SDL.initialize [SDL.InitVideo]
  -- SDL.HintRenderScaleQuality $= SDL.ScaleLinear
  -- do renderQuality <- SDL.get SDL.HintRenderScaleQuality
  --    when (renderQuality /= SDL.ScaleLinear) $
  --      putStrLn "Warning: Linear texture filtering not enabled!"

  window <-
    SDL.createWindow
      "SDL / OpenGL Example"
      SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight
                        , SDL.windowOpenGL = Just SDL.defaultOpenGL
                        }
  SDL.showWindow window

  _ <- SDL.glCreateContext window
  (prog, attrib, unif) <- initResources fragSrc vertSrc

  let loop = do
        events <- SDL.pollEvents

        GL.clear [GL.ColorBuffer]
        draw prog attrib unif
        SDL.glSwapWindow window

        unless (any escOrQuit events) loop

  loop

  SDL.destroyWindow window
  SDL.quit

data ShaderType
  = Frag
  | Vert
  deriving (Show, Eq)

buildShaderFrom :: ShaderType -> ByteString -> IO GL.Shader
buildShaderFrom stype src =
  let
    (err, glType) = case stype of
      Frag -> ("fragment", GL.FragmentShader)
      Vert -> ("vertex", GL.VertexShader)
  in do
    s <- GL.createShader glType
    GL.shaderSourceBS s $= src
    GL.compileShader s
    sOK <- GL.get $ GL.compileStatus s
    if sOK then pure s else do
      slog <- GL.shaderInfoLog s
      hPutStrLn stderr ("Error in " <> err <> "shader\n")
      hPutStrLn stderr slog
      exitFailure

initResources :: ByteString -> ByteString -> IO (GL.Program, GL.AttribLocation, GL.UniformLocation)
initResources fsSource vsSource = do
    -- compile vertex shader
  vs <- buildShaderFrom Vert vsSource
    -- Do it again for the fragment shader
  fs <- buildShaderFrom Frag fsSource

  program <- GL.createProgram
  GL.attachShader program vs
  GL.attachShader program fs

  uResolution <- GL.uniformLocation program "uResolution"
  GL.attribLocation program "coord2d" $= GL.AttribLocation 0

  GL.linkProgram program
  linkOK <- GL.get $ GL.linkStatus program
  GL.validateProgram program
  status <- GL.get $ GL.validateStatus program
  unless (linkOK && status) $ do
    hPutStrLn stderr "GL.linkProgram error"
    plog <- GL.get $ GL.programInfoLog program
    putStrLn plog
    exitFailure

  GL.currentProgram $= Just program

  return (program, GL.AttribLocation 0, uResolution)

draw :: GL.Program -> GL.AttribLocation -> GL.UniformLocation -> IO ()
draw program attrib uResolution = do
    GL.clear [GL.ColorBuffer]

    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral screenWidth) (fromIntegral screenHeight))

    GL.currentProgram $= Just program

    GL.uniform uResolution $=
      (GL.Vector2 (fromIntegral screenWidth) (fromIntegral screenHeight) :: GL.Vector2 GL.GLint)

    GL.vertexAttribArray attrib $= GL.Enabled
    V.unsafeWith quad $ \ptr ->
      GL.vertexAttribPointer attrib $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 ptr)

    GL.drawArrays GL.Quads 0 4
    GL.vertexAttribArray attrib $= GL.Disabled

quad :: V.Vector Float
quad = V.fromList
  [ -1.0,  1.0
  ,  1.0,  1.0
  , -1.0, -1.0
  ,  1.0, -1.0
  ]

-- tri :: V.Vector Float
-- tri = V.fromList
--   [ 0.0, 2.0
--   , -2.0, -2.0
--   , 2.0, -2.0
--   ]
