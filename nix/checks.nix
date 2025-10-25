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
            });
            checks.stg-coverage = self'.packages.default.overrideAttrs (oldAttrs: {
                name = "full-coverage";
                checkPhase = ''
                    					echo "-------------------------------------------------------------"
                      						runhaskell ./Setup.hs configure --builddir=$out/coverage --enable-library-coverage  --enable-coverage --enable-tests
                      						runhaskell ./Setup.hs build --builddir=$out/coverage
                      						runhaskell ./Setup.hs test --builddir=$out/coverage
                    					echo "-------------------------------------------------------------"
                    				'';
            });
        };
}
