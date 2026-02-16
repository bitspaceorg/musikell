# Pre-commit hooks: run on git commit or via checks.dev-pre-commit.
# Uses treefmt-nix wrapper when available (same config as `nix develop -c treefmt`).
# Install: nix develop .#dev (shellHook auto-installs hooks).
#
# haskell-header: lives in utils/execution/haskell-header-hook.sh
#   - Auto-prepends File, Author (git user.email), Docs, Module to staged .hs files
#   - Fails if docs file missing
#   - License: omitted until finalised (see script comment)

{ inputs, ... }:
{
    imports = [ inputs.precommit.flakeModule ];

    perSystem =
        {
            config,
            pkgs,
            lib,
            ...
        }:
        let
            python = pkgs.python3.withPackages (ps: [ ps.python-frontmatter ]);
            hook-script = pkgs.writeShellScript "docs-build-hook" ''
                set -euo pipefail
                cd "$(${pkgs.git}/bin/git rev-parse --show-toplevel)"
                ${python}/bin/python utils/docs/validate.py
                ${python}/bin/python utils/docs/build_index.py
                ${pkgs.git}/bin/git add docs/data.json
            '';
        in
        {
            pre-commit = {
                check.enable = false;

                settings = {
                    hooks = {
                        treefmt.enable = true;
                        nil.enable = lib.mkDefault true;
                        docs-build = {
                            enable = true;
                            name = "docs-build";
                            entry = "${hook-script}";
                            language = "system";
                            pass_filenames = false;
                            always_run = true;
                        };
                        haskell-header = {
                            enable = true;
                            name = "haskell-header";
                            description = "Auto-append File/Author/Docs/Module header to Haskell files";
                            entry = "bash utils/execution/haskell-header-hook.sh";
                            files = "\\.l?hs$";
                            pass_filenames = true;
                        };
                    };
                };
            };

            checks.dev-pre-commit = config.pre-commit.settings.run;

            devShells.precommit = pkgs.mkShell { shellHook = config.pre-commit.settings.shellHook; };
        };
}
