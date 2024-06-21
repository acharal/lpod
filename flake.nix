{
  description = "LPOD 2 ASP";

  inputs = {
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    nixpkgs.url = "nixpkgs/23.05";
    devshell.url = "github:numtide/devshell";

    vscode-ext.url = "github:nix-community/nix-vscode-extensions";
    vscode-ext.inputs.nixpkgs.follows = "nixpkgs";

    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, devshell, vscode-ext, ... }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config = { 
            allowUnfree = true;
            permittedInsecurePackages = [
              "python-2.7.18.6"
              "openssl-1.1.1u"
              "python-2.7.18.6-env"
            ];
          };
      
          overlays = [
            devshell.overlays.default
            vscode-ext.overlays.default
            self.overlays.${system}.default
          ];
        };

        overlay = self: super: {
          python2 = super.python2.override {
            packageOverrides = pself: psuper: { 
              asprin = pself.pythonPackages.callPackage ./nix/asprin.nix {};
              clingo = psuper.pythonPackages.callPackage ./nix/clingo.nix {};
            };
          };
        };
      in
      {

        overlays.default = overlay;

        devShell = with pkgs; pkgs.devshell.mkShell {
          packages = [
            gnumake
            editorconfig-checker
            nixpkgs-fmt
            swiProlog
            (python2.withPackages(ps: [ ps.asprin ps.clingo ] ))
          ];
        };
      });
}
