{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nixpkgs_24_11.url = "github:nixos/nixpkgs/24.11";
    flake-utils.url = "github:numtide/flake-utils";
    ps-overlay.url = "github:thomashoneyman/purescript-overlay";
    mkSpagoDerivation = {
      url = "github:jeslie0/mkSpagoDerivation";
      inputs = {
        registry.url = "github:purescript/registry/fe3f499706755bb8a501bf989ed0f592295bb4e3";
        registry-index.url = "github:purescript/registry-index/a349ca528812c89915ccc98cfbd97c9731aa5d0b";
      };
    };
  };

  outputs = { self, nixpkgs, nixpkgs_24_11, flake-utils, ps-overlay, mkSpagoDerivation }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ mkSpagoDerivation.overlays.default
                       ps-overlay.overlays.default
                     ];
        };

        myPackage =
            pkgs.mkSpagoDerivation {
              spagoYaml = ./spago.yaml;
              spagoLock = ./spago.lock;
              src = ./.;
              version = "0.1.0";
              nativeBuildInputs = [ pkgs.esbuild pkgs.purs-backend-es pkgs.purs-bin.purs-0_15_15 pkgs.spago-unstable ]; # nixpkgs_24_11.nodePackages.parcel
              # changed `pkgs.purs-unstable` to `pkgs.purs-bin.purs-0_15_15`
              buildPhase = "spago build";
              installPhase = "mkdir $out; cp -r ./web $out";
              buildNodeModulesArgs = {
                  npmRoot = ./.;
                nodejs = pkgs.nodejs;
              };
            };

        myApp = {
            type = "app";
            program = "sh ./serve.sh";
        };

      in
        {
          packages.default = myPackage;

          apps.default = myApp;
        }

    );
}
