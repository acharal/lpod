{
  lib,
  clingo,
  fetchFromGitHub,
  buildPythonPackage
}:

buildPythonPackage rec {

  src = fetchFromGitHub { 
    owner = "potassco";
    repo = "asprin";
    rev = "v3.1.1";
    hash = "sha256-9VAwXvuGhaYw/ZgC8RK7/JylrDc3UWW6zXBrVaVAs84=";
  };

  pname = "asprin";
  version = "3.1.1";
  format = "setuptools";

  propagatedBuildInputs = [
    clingo
  ];

  # does not contain any tests
  doCheck = false;


  meta = with lib; {
    license = licenses.mit;
    description = "Qualitative and quantitative optimization in answer set programming";
    homepage = "https://github.com/potassco/asprin";
  };
}
