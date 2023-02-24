{ python3
, fetchFromGitHub
}:

let
  svgelements = python3.pkgs.buildPythonPackage rec {
    pname = "svgelements";
    version = "1.9.0";
    src = python3.pkgs.fetchPypi {
      inherit pname version;
      hash = "sha256-Ic4y8YjXlo2gf1hdiBQcDNhjgEmmiBPWnH65QfoJ+tY=";
    };
    doCheck = false;
  };

in python3.pkgs.buildPythonApplication {
  pname = "bCNC";
  version = "0.9.15.318";

  src = fetchFromGitHub {
    owner = "vlachoudis";
    repo = "bCNC";
    rev = "6c205affb30b550778448e10ebeaaceee654be78";
    sha256 = "vGJveJKNLZ4jbXuBc9EQePdZdAfiiWhAWFbi96hDCkE=";
  };

  patches = [
    # FIXME: It seems that we need a wrapper called python-opencv
    ./0001-Disable-opencv.patch
  ];
  doCheck = false;

  propagatedBuildInputs = with python3.pkgs; [
    numpy
    pillow
    pyautogui
    pyserial
    scipy
    svgelements
    tkinter
  ];
}
