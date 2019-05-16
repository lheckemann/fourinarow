with import <nixpkgs> {};
mkShell {
  buildInputs = [(ghc.withPackages (ps: with ps; [
    monad-loops
  ]))];
}
