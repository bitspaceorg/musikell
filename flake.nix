{
    inputs = {
		nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    	parts.url = "github:hercules-ci/flake-parts";
    	treefmt.url = "github:numtide/treefmt-nix";
    	flake-root.url = "github:srid/flake-root";
    	precommit.url = "github:cachix/pre-commit-hooks.nix";
	};

    outputs = inputs:
		inputs.parts.lib.mkFlake { inherit inputs; } {
			imports = [ 
				./nix/musikell.nix
        		./nix/devShells.nix
        		./nix/checks.nix
        		./nix/utils/treefmt.nix
        		./nix/utils/precommit.nix
			];
        	systems = inputs.nixpkgs.lib.systems.flakeExposed;
			perSystem = { pkgs, ...}:
			let ghc = "ghc94";
			in {
				_module.args = {
					musikell = {
    					name = "musikell";
    					version = "0.0.1";
    					description = "Music + Haskell + engine = Musikell";
					};
					shell-config = {
						inherit ghc;
						ghcpkgs = pkgs.haskell.packages.${ghc};
					};
				};
			};
        };
}
