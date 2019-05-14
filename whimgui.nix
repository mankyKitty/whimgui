{ mkDerivation, base, bytestring, containers, gl, hedgehog, lens
, linear, sdl2, StateVar, stdenv, text, vector
}:
mkDerivation {
  pname = "whimgui";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers gl lens linear sdl2 StateVar text vector
  ];
  executableHaskellDepends = [
    base bytestring containers gl lens linear sdl2 StateVar text vector
  ];
  testHaskellDepends = [
    base bytestring containers gl hedgehog lens linear sdl2 StateVar
    text vector
  ];
  homepage = "https://github.com/mankyKitty/whimgui#readme";
  license = stdenv.lib.licenses.bsd3;
}
