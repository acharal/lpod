{ lib, stdenv, fetchFromGitHub, cmake, toPythonModule, python27 }:

toPythonModule (stdenv.mkDerivation rec {
  pname = "clingo";
  version = "5.4.0";
  src = fetchFromGitHub {
    owner = "potassco";
    repo = "clingo";
    rev = "v${version}";
    sha256 = "sha256-9eCEjWMKz+GQmK2BOyLbxIKS68WJNWi4IKDj8fij2D0=";
  };

  nativeBuildInputs = [ cmake python27 ];

  cmakeFlags = [ "-DCLINGO_BUILD_WITH_PYTHON=ON" "-DPYCLINGO_INSTALL_DIR=lib/python2.7/site-packages"];

  meta = {
    description = "ASP system to ground and solve logic programs";
    license = lib.licenses.mit;
    maintainers = [lib.maintainers.raskin];
    platforms = lib.platforms.unix;
    homepage = "https://potassco.org/";
    downloadPage = "https://github.com/potassco/clingo/releases/";
  };
})
