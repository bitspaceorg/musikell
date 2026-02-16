{ ... }:
{
    perSystem =
        { self', pkgs, ... }:
        {
            checks.dev-unit = self'.packages.default.overrideAttrs (oldAttrs: {
                name = "unit-test";
                configureFlags = [ "--enable-tests" ];
                doCheck = true;
            });
            checks.stg-coverage = self'.packages.default.overrideAttrs (oldAttrs: {
                name = "full-coverage";
                configureFlags = [
                    "--enable-coverage"
                    "--enable-tests"
                ];
                doCheck = true;
                outputs = [ "out" ];
                installPhase = ''
                    runHook preInstall
                    mkdir -p $out/coverage
                    if [ -d dist/hpc ]; then
                        cp -r dist/hpc $out/coverage/
                    elif hpc_dir=$(find dist-newstyle -type d -name hpc 2>/dev/null | head -1) && [ -n "$hpc_dir" ]; then
                        cp -r "$hpc_dir" $out/coverage/
                    fi
                    runHook postInstall
                '';
            });
        };
}
