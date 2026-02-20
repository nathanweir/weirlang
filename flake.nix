{
  description = "Weir programming language development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      fenix,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        toolchain = fenix.packages.${system}.stable.withComponents [
          "cargo"
          "clippy"
          "rust-src"
          "rustc"
          "rustfmt"
          "rust-analyzer"
        ];
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            toolchain
            pkgs.just
            pkgs.cargo-insta
            pkgs.nodejs
            pkgs.pnpm
            pkgs.tree-sitter
            pkgs.libffi
            pkgs.pkg-config
            pkgs.glfw
            pkgs.libGL
            pkgs.libx11
            pkgs.libxrandr
            pkgs.libxinerama
            pkgs.libxcursor
            pkgs.libxi
            pkgs.libxext
            pkgs.libxkbcommon
          ];

          NIX_LD_LIBRARY_PATH = pkgs.lib.optionalString pkgs.stdenv.isLinux (
            pkgs.lib.makeLibraryPath [
              pkgs.stdenv.cc.cc
              pkgs.libffi
              pkgs.glfw
              pkgs.libGL
              pkgs.libx11
              pkgs.libxrandr
              pkgs.libxinerama
              pkgs.libxcursor
              pkgs.libxi
              pkgs.libxext
              pkgs.libxkbcommon
            ]
          );

          NIX_LD = pkgs.lib.optionalString pkgs.stdenv.isLinux (
            pkgs.lib.fileContents "${pkgs.stdenv.cc}/nix-support/dynamic-linker"
          );
        };
      }
    );
}
