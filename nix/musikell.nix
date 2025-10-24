{ inputs, ... }:
{
    perSystem =
        {
            pkgs,
            musikell,
            shell-config,
            ...
        }:
        let
            musikell-package = (shell-config.ghcpkgs.callCabal2nix musikell.name inputs.self { }).overrideAttrs (old: {
                buildInputs = old.buildInputs or [ ] ++ [
                    shell-config.ghcpkgs.happy
                    shell-config.ghcpkgs.alex
                    pkgs.pkg-config

                ];
            });
        in
        {
            packages.default = musikell-package;
            apps.default = {
                type = "app";
                program = "${musikell-package}/bin/${musikell.name}";
            };
        };
}
