{ nixpkgs ? import <nixpkgs> {}
}:
let
  inherit (nixpkgs) pkgs;

  revealjs = pkgs.fetchFromGitHub {
    owner = "hakimel";
    repo = "reveal.js";
    rev = "43eada79901830702bd40dce857831aef8e76759";
    sha256 = "5be5c1b831e0f4a4f955f76340c2d08c8a1a57c5be5dd68592fd5be511e76bda";
  };

in
  pkgs.stdenv.mkDerivation {
    name = "property-based-testing-talk";
    src = ./.;

    unpackPhase = ''
      mkdir -p $name/reveal.js
      cd $name
      cp -r ${revealjs}/* ./reveal.js/
      cp -r $src/img .
    '';

    buildPhase = ''
      cat $src/slides/title.md \
          $src/slides/what-why.md \
          $src/slides/hedgehog.md \
          $src/slides/hpython.md \
          > slides.md
      pandoc -t revealjs --variable=transition:none --highlight-style=zenburn -s slides.md -o index.html
      rm slides.md
    '';

    installPhase = ''
      mkdir $out
      cp -r ./* $out/
    '';

    phases = ["unpackPhase" "buildPhase" "installPhase"];

    buildInputs = [pkgs.pandoc];
  }
