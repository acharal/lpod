{
  lib,
  asprin,
  clingo,
  python,
  ply,
  fetchFromGitHub,
  buildPythonApplication
}:

buildPythonApplication rec {

  src = fetchFromGitHub { 
    owner = "zhunyoung";
    repo = "lpod2asprin";
    rev = "master";
    hash = "sha256-tQW0O7BxyD2Wt+mitEKPni93Li/X1IMMfJV2sVu+fKI=";
  };

  pname = "lpod2asprin";
  version = "1.0";

  format = "other";

  propagatedBuildInputs = [
    asprin clingo ply
  ];

  # does not contain any tests
  doCheck = false;

  installPhase = ''
    mkdir -p "$out"/{bin,share/lpod2asprin}
    cp -R *.py $out/share/lpod2asprin

    makeWrapper ${python.interpreter} $out/bin/lpod2asprin \
      --set PYTHONPATH "$PYTHONPATH:$out/share/lpod2asprin" \
      --add-flags "-O $out/share/lpod2asprin/lpod2asprin.py"

    $out/bin/lpod2asprin || exit 0
  '';

}
