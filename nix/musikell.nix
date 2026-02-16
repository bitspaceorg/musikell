{ inputs, ... }:
{
    perSystem =
        {
            pkgs,
            self',
            musikell,
            shell-config,
            projectRoot,
            ...
        }:
        let
            # Include all files (untracked, etc.) — plain path can exclude them in git repos
            src = builtins.path {
                path = projectRoot;
                name = "musikell-src";
                filter = path: type: true;
            };
            musikell-package = shell-config.ghcpkgs.callCabal2nix musikell.name src { };
            musikell-with-tests = musikell-package.overrideAttrs (old: {
                configureFlags = (old.configureFlags or [ ]) ++ [ "--enable-tests" ];
                doCheck = true;
            });
        in
        {
            packages.default = musikell-with-tests;
            packages.contract = pkgs.runCommand "musikell-contract" { } ''
                mkdir -p $out/include
                cp ${../src/Musikell/FFI/Contract.h} $out/include/Contract.h
            '';
            packages.dev = pkgs.runCommand "dev" { } ''
                ls ${self'.checks.dev-pre-commit} > /dev/null
                cp -rL ${musikell-with-tests}/* $out/
            '';
            packages.stg = pkgs.runCommand "stg" { } ''
                ls ${self'.checks.dev-pre-commit} > /dev/null
                cp -rL ${self'.checks.stg-coverage}/* $out/
            '';
            apps.default = {
                type = "app";
                program = "${musikell-with-tests}/bin/${musikell.name}";
            };
        };
}
