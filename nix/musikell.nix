{ inputs, ... }:
{
    perSystem =
        { musikell, shell-config, ... }:
        let
            musikell-package = shell-config.ghcpkgs.callCabal2nix musikell.name inputs.self { };
        in
        {
            packages.default = musikell-package;
            apps.default = {
                type = "app";
                program = "${musikell-package}/bin/${musikell.name}";
            };
        };
}
