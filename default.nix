{ nixpkgs ? import <nixpkgs> {}
}:
let
  inherit (nixpkgs) pkgs;

  revealjs = pkgs.fetchFromGitHub {
    owner = "hakimel";
    repo = "reveal.js";
    rev = "a2e69a4b42f9e968406f62073d1c4bf0ea2d3361";
    sha256 = "0aclgbdb52zxhpzi9zvwxsx4qvvq2wy74bxm8l0lcj0csxqzrjm0";
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
          $src/slides/recursion-and-stuff.md \
          $src/slides/plated.md \
          $src/slides/in-the-wild.md \
          $src/slides/further-reading.md \
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
