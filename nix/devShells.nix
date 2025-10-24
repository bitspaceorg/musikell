{ ... }:
{
    perSystem =
        {
            pkgs,
            self',
            config,
            ...
        }:
        {
            devShells.dev = pkgs.mkShell {
                packages = [ pkgs.cabal-install ];
                inputsFrom = [
                    self'.packages.default
                    self'.devShells.treefmt
                    self'.devShells.precommit
                ];
            };
            devShells.stg = pkgs.mkShell {
                inputsFrom = [ self'.packages.default ];
                packages = [
                    pkgs.jq
                    pkgs.gnused
                    pkgs.curl
                    pkgs.gitlab-release-cli
                ];
            };
        };
}
