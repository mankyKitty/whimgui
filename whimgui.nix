{ mkDerivation, base, bytestring, containers, hedgehog, lens
, linear, OpenGL, sdl2, stdenv, text, vector
}:
mkDerivation {
  pname = "whimgui";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers lens linear OpenGL sdl2 text vector
  ];
  executableHaskellDepends = [
    base bytestring containers lens linear OpenGL sdl2 text vector
  ];
  testHaskellDepends = [
    base bytestring containers hedgehog lens linear OpenGL sdl2 text
    vector
  ];
  homepage = "https://github.com/mankyKitty/whimgui#readme";
  license = stdenv.lib.licenses.bsd3;
}
