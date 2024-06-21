{
  description = "LPOD 2 ASP";

  inputs = {
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    nixpkgs.url = "nixpkgs/master";
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
          config.allowUnfree = true;
          overlays = [
            devshell.overlays.default
            vscode-ext.overlays.default
          ];
        };
      in
      {

        devShell = with pkgs; pkgs.devshell.mkShell {
          packages = [
            gnumake
            editorconfig-checker
            nixpkgs-fmt
            (pkgs.vscode-with-extensions.override {
              vscodeExtensions = with pkgs.vscode-extensions; [
                pkgs.vscode-marketplace.arthurwang.vsc-prolog
              ];
            })
            swiProlog
            clingo
          ];
        };
      });
}
