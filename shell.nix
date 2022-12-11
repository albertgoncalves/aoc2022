with import <nixpkgs> {};
mkShell {
    buildInputs = [
        ghc
        hlint
        ormolu
    ];
    shellHook = ''
    '';
}
