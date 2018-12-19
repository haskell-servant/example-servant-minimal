######
#
# Author: Brandon Barker
#
######

with import <nixpkgs> { };
let
  hiepkgs = import (builtins.fetchTarball {
    # Note: please configure cachix, or this will take a while:
    # https://hie-nix.cachix.org/
    name = "example-servant-minimal";
    url = https://github.com/domenkozar/hie-nix/tarball/a7ef4c4ceef1dbf46aabff68a4b9bd86d41d0886;
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "1hx449l001jc6ijn9zxx30zr1xr2nnrv7qmhpsqwj8wp6s4zyxw8";
  }) {};
in stdenv.mkDerivation {
  name = "cow-streamer-stack";
  buildInputs = [
    #
    # Editor stuff
    #
    hiepkgs.hies
    vscode
    
    cabal-install
    gmp
    haskellPackages.Stack
  ];
  shellHook = ''
    export LD_LIBRARY_PATH=${gmp}/lib:$LD_LIBRARY_PATH
  '';  
}

