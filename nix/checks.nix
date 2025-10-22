{ ... }:
{
	perSystem = { self', ... }: {
		checks.dev-unit = self'.packages.default.overrideAttrs (oldAttrs: {
			name = "unit-test";
		});
		checks.stg-full-coverage = self'.packages.default.overrideAttrs (oldAttrs: {
			name = "full-coverage";
			doCoverage = true;
		});
	};
}
