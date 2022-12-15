with import <nixpkgs> {};
mkShell {
    buildInputs = [
        ghc
        haskellPackages.pointfree
        hlint
        ormolu
    ];
    shellHook = ''
        . .shellhook
    '';
}
