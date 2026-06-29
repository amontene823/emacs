{
  description = "Develop Python on Nix with uv";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = { nixpkgs, ... }:
    let
      inherit (nixpkgs) lib;
      forAllSystems = lib.genAttrs lib.systems.flakeExposed;
    in {
      devShells = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};

          qtRuntimeLibraries = with pkgs; [
            libxkbcommon
            xorg.libxcb
            xorg.xcbutil
            xorg.xcbutilcursor
            xorg.xcbutilimage
            xorg.xcbutilkeysyms
            xorg.xcbutilrenderutil
            xorg.xcbutilwm
            xorg.libX11
            xorg.libSM
            xorg.libICE
            fontconfig
            freetype
            dbus
            glib
            wayland
          ];

          wheelRuntimeLibraries = with pkgs; [
            zlib
            zstd
            stdenv.cc.cc.lib
          ];

          glRuntimeLibraries = with pkgs; [
            mesa
            libglvnd
          ];

          runtimeLibraries =
            qtRuntimeLibraries
            ++ wheelRuntimeLibraries
            ++ glRuntimeLibraries;
        in {
          default = pkgs.mkShell {
            packages = [
              pkgs.python313
              pkgs.uv
            ] ++ runtimeLibraries;

            env = (lib.optionalAttrs pkgs.stdenv.isLinux {
              LD_LIBRARY_PATH = lib.makeLibraryPath runtimeLibraries;
            }) // {
              UV_PYTHON = "${pkgs.python313}/bin/python3.13";
              UV_PYTHON_DOWNLOADS = "never";
            };

            shellHook = ''
              unset PYTHONPATH

              if [ -d .venv ]; then
                if ! .venv/bin/python -c 'import sys' >/dev/null 2>&1; then
                  rm -rf .venv
                elif [ "$(readlink -f .venv/bin/python)" != "$(readlink -f "$UV_PYTHON")" ]; then
                  rm -rf .venv
                fi
              fi

              [ -d .venv ] || uv venv --python "$UV_PYTHON"
              uv sync
              . .venv/bin/activate
            '';
          };
        });
    };
}
