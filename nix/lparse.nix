{ stdenv, perl, gnumake, bison, flex, fetchzip }:


stdenv.mkDerivation rec { 
  name = "lparse";
  version = "1.1.2";


  src = fetchzip { 
    url = "http://www.tcs.hut.fi/Software/smodels/src/${name}-${version}.tar.gz";
    hash = "sha256-V/ymKlZlj3GDVLUyiBgKjEA9rzSQFNFUEgPdvFco4MI="; 
  };

  patches = [
    ./lparse/configure.patch 
    ./lparse/fix-version.patch
  ];

  configurePhase = ''
    ./configure CC=$CXX --prefix=$out/bin
  '';

  nativeBuildInputs = [ 
    gnumake
    perl
    bison
    flex
  ];

  preInstall = ''
  mkdir -p $out/bin
  '';

}
