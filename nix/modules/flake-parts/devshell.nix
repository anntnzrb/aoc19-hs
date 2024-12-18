{
  perSystem =
    { config, pkgs, ... }:
    {
      devShells.default = pkgs.mkShell {
        name = "aoc19";
        meta.description = "Haskell development environment";
        inputsFrom = with config; [
          haskellProjects.default.outputs.devShell
          pre-commit.devShell
        ];
        packages = with pkgs; [
          just
          ghciwatch
        ];
      };
    };
}
