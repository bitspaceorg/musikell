{ ... }:
{
    perSystem =
        {
            self',
            pkgs,
            shell-config,
            ...
        }:
        {
            checks.dev-unit = self'.packages.default.overrideAttrs (oldAttrs: {
                name = "unit-test";
                configureFlags = [ "--enable-tests" ];
            });
            checks.stg-coverage = self'.packages.default.overrideAttrs (oldAttrs: {
                name = "full-coverage";
                configureFlags = [
                    "--enable-coverage"
                    "--enable-tests"
                ];
                # add doc to seperate haddock
                outputs = [ "out" ];
                installPhase = ''
                                      				  runHook preInstall
                                      				  mkdir -p $out/coverage
                    				  					  cp -r dist/hpc $out/coverage/
                                      				  runHook postInstall
                                      				'';
            });
        };
}
