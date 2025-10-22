{ inputs, ... }:
{
	imports = [ inputs.precommit.flakeModule ];
	perSystem = { config, pkgs, lib, ... }: {
		pre-commit = {
			check.enable = false;
			settings = {
				hooks = {
					treefmt.enable = true;
          			nil.enable = lib.mkDefault true;
				};
			};
		};
		checks.dev-pre-commit = config.pre-commit.settings.run;
		devShells.precommit = pkgs.mkShell {
			shellHook = config.pre-commit.settings.shellHook;
		};
	};
}
