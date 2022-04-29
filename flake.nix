{
  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    ligo = {
      url = "git+https://gitlab.com/serokell/ligo/ligo";
      flake = false;
    };
  };

  outputs = { nixpkgs, haskell-nix, self, ... }:
    let
      ligo-binary = {
        "x86_64-linux" = { url = "https://gitlab.com/ligolang/ligo/-/jobs/2385579945/artifacts/raw/ligo"; hash = "sha256-UFGPJTPxyyKvlcY8Lh3poY6anOtyErCUTfZi2BdkBM4="; };
      };

      platforms = [ "x86_64-linux" ];
      filteredPlatforms = with builtins; listToAttrs (map (f: { name = f; value = nixpkgs.legacyPackages.${f}; }) platforms);
      onPkgs = f: with builtins; mapAttrs f filteredPlatforms;
      sources = builtins.path { path = ./.; filter = (path: type: (builtins.match "(.*\\.nix|.*\\.lock)" path) == null); };
    in
    {
      nixosModules.default = { config, pkgs, lib, ... }:
        let system = pkgs.system; in
        {

          options = with pkgs.lib; {

            services.ligo-webide-frontend = {
              enable = mkEnableOption "ligo-webide service";

              serverName = mkOption {
                type = types.str;
                default = "localhost";
                description = ''
                  Name of the nginx virtualhost to use.
                '';
              };

              listenHost = mkOption {
                type = types.str;
                default = "localhost";
                description = ''
                  Listen address for the virtualhost to use.
                '';
              };

            };

            services.ligo-webide = {
              enable = mkEnableOption "ligo-webide service";
            };
          };

          config = with pkgs.lib; let
            cfg_frontend = config.services.ligo-webide-frontend;
            cfg = config.services.ligo-webide;
            req = self.packages.${system};
          in
          lib.mkIf cfg.enable {
            systemd.services.ligo-webide = {
              after = [ "network.target" ];
              wantedBy = [ "multi-user.target" ];

              script =
                ''
                  ${req.backend}/bin/ligo-webide-backend --ligo-path ${req.ligo-bin}/bin/ligo
                '';

            };


            services.nginx = {
              enable = true;
              recommendedProxySettings = true;
              virtualHosts.ligo-webide = {
                serverName = cfg_frontend.serverName;
                locations."/" = {
                  root = req.webide;
                  tryFiles = "$uri $uri/ /index.html";
                };
                locations."~ ^/api(?<route>/.*)" = {
                  proxyPass = "http://127.0.0.1:8080$route";
                };
              };
            };

            networking.firewall.allowedTCPPorts = [ 80 443 ];

          };
        };

      packages = onPkgs (system: pkgs:
        with pkgs;
        let
          a_haskell-nix = haskell-nix.legacyPackages.${system}.haskell-nix;
        in
        rec {

          ligo-bin = pkgs.runCommand "ligo-bin" { } ''
            install -Dm777 ${pkgs.fetchurl ligo-binary.${system}} $out/bin/ligo
          '';

          backend =
            let
              name = "ligo-webide-backend";
              proj = a_haskell-nix.stackProject {
                src = a_haskell-nix.cleanSourceHaskell {
                  src = ./ligo-webide-backend;
                  inherit name;
                };
              };
            in
            proj.${name}.components.exes.ligo-webide-backend;


          # super naïve implementation, needs re-doing
          webide = pkgs.runCommand "ligo-webide"
            {
              outputHash = "sha256-jUnVT9UJnNlzWXn/WosDjmrJ/9YTyxOI8tiiY+3tjXU=";
              outputHashMode = "recursive";
              outputHashAlgo = "sha256";
              buildInputs = [
                yarn
                nodejs
                # for downloading external deps
                git
                cacert
                # for building keytar
                python3
                pkg-config
                libsecret
                gcc
              ];
            } ''
            TARGET=$TMP/build

            export HOME=$TMP

            cp -r ${sources} $TARGET
            chmod -R +w $TARGET
            cd $TARGET

            pushd ligo-webide-frontend/base-components
              yarn install --ignore-scripts --ignore-engines --ignore-platform
              patchShebangs node_modules
              yarn build
            popd

            pushd ligo-webide-frontend/ligo-components
              yarn install --ignore-scripts --ignore-engines --ignore-platform
              patchShebangs node_modules
              yarn build
            popd

            pushd ligo-webide-frontend/ligo-ide
              yarn install --ignore-scripts --ignore-engines --ignore-platform
              patchShebangs node_modules
              pushd node_modules/keytar
                patchShebangs node_modules
                npm run build
              popd
              yarn build:react
            popd

            cp -r $TARGET/ligo-webide-frontend/ligo-ide/build $out
          '';

        }

      );

    };

}
