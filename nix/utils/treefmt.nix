# Formatting configuration: treefmt orchestrates all formatters.
# Run: nix develop .#dev -c treefmt
# Pre-commit runs the same config via config.treefmt.build.wrapper.

{ inputs, ... }:
{
    imports = [ inputs.treefmt.flakeModule ];
    perSystem =
        { config, pkgs, ... }:
        let
            fourmoluOpts = [
                "--indentation"
                "4"
                "--column-limit"
                "none"
                "--function-arrows"
                "trailing"
                "--comma-style"
                "trailing"
                "--import-export-style"
                "diff-friendly"
                "--import-grouping"
                "by-scope-then-qualified"
                "--indent-wheres"
                "true"
                "--record-brace-space"
                "true"
                "--newlines-between-decls"
                "1"
                "--haddock-style"
                "single-line"
                "--haddock-style-module"
                "single-line"
                "--haddock-location-signature"
                "trailing"
                "--let-style"
                "auto"
                "--in-style"
                "left-align"
                "--if-style"
                "hanging"
                "--single-constraint-parens"
                "always"
                "--single-deriving-parens"
                "always"
                "--sort-constraints"
                "true"
                "--sort-derived-classes"
                "true"
                "--sort-deriving-clauses"
                "true"
                "--trailing-section-operators"
                "false"
                "--unicode"
                "never"
                "--respectful"
                "true"
            ];
        in
        {
            treefmt.config = {
                projectRootFile = "flake.nix";
                flakeCheck = false;
                settings.global.excludes = [ "docs/data.json" ];
                package = pkgs.treefmt;

                programs = {
                    # Haskell
                    fourmolu.enable = true;
                    hlint.enable = true;
                    cabal-fmt.enable = true;

                    # Nix
                    nixfmt = {
                        enable = true;
                        strict = true;
                        width = 180;
                        indent = 4;
                    };

                    # Markdown, MDX, YAML (config in Nix only)
                    prettier = {
                        enable = true;
                        includes = [
                            "**/*.md"
                            "**/*.mdx"
                            "**/*.yml"
                            "**/*.yaml"
                        ];
                        excludes = [
                            "dist-newstyle"
                            "dist"
                            "result"
                            "result-*"
                            "flake.lock"
                        ];
                    };

                    # Shell
                    shfmt = {
                        enable = true;
                        indent_size = 4;
                        simplify = true;
                    };
                };

                settings.formatter = {
                    fourmolu.options = fourmoluOpts;
                    prettier.options = [
                        "--print-width"
                        "100"
                        "--tab-width"
                        "4"
                        "--trailing-comma"
                        "es5"
                        "--end-of-line"
                        "lf"
                    ];
                };
            };

            devShells.treefmt = pkgs.mkShell { buildInputs = [ config.treefmt.build.wrapper ] ++ (builtins.attrValues config.treefmt.build.programs); };
        };
}
