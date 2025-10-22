{ inputs, ... }:
{
    imports = [
        inputs.treefmt.flakeModule
        inputs.flake-root.flakeModule
    ];
    perSystem = {config, pkgs, ...}: {
        treefmt.config = {
            inherit (config.flake-root) projectRootFile;
            flakeCheck = false;
            package = pkgs.treefmt;
            programs = {
                fourmolu.enable = true;
                hlint.enable = true;
                cabal-fmt.enable = true;
            };
            settings.formatter = {
                fourmolu = {
                    options = [
                        "--indentation" "4"
                        "--column-limit" "none"
                        "--function-arrows" "trailing"
                        "--comma-style" "trailing"
                        # "--record-style" "knr"
                        "--import-export-style" "diff-friendly"
                        "--import-grouping" "by-scope-then-qualified"
                        "--indent-wheres" "true"
                        "--record-brace-space" "true"
                        "--newlines-between-decls" "1"
                        "--haddock-style" "single-line"
                        "--haddock-style-module" "single-line"
                        "--haddock-location-signature" "trailing"
                        "--let-style" "auto"
                        "--in-style" "left-align"
                        "--if-style" "hanging"
                        "--single-constraint-parens" "always"
                        "--single-deriving-parens" "always"
                        "--sort-constraints" "true"
                        "--sort-derived-classes" "true"
                        "--sort-deriving-clauses" "true"
                        "--trailing-section-operators" "false"
                        "--unicode" "never"
                        "--respectful" "true"
                    ];
                };
            };
        };
        devShells.treefmt = pkgs.mkShell {
            buildInputs = [ config.treefmt.build.wrapper ]
                       ++ ( builtins.attrValues config.treefmt.build.programs );
        };
    };
}
