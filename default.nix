{ mkDerivation, base, directory, filepath, stdenv, unix }:
mkDerivation {
  pname = "mydu";
  version = "1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base directory filepath unix ];
  description = "A basic recursive du";
  license = stdenv.lib.licenses.publicDomain;
}
