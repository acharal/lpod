{ stdenv, gnumake, fetchzip }:


stdenv.mkDerivation rec {
   name = "psmodels";
   version = "2.26a";

   src = fetchzip { 
    url = "http://www.tcs.hut.fi/Software/smodels/src/${name}-${version}.tar.gz";
    hash = "sha256-RTVPETT34TqteycePT4WTJ1QkHBHcg/m8VNXHPFVGOA=";
   };

   nativeBuildInputs = [
     gnumake
   ];
 
   patches = [ ./psmodels/fixes.patch ];

   installPhase = ''
      install -D -m 755 psmodels $out/bin/psmodels
   '';

}

