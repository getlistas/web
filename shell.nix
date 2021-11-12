let
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/21.05.tar.gz";
  }) {};

  # To update to a newer version of easy-purescript-nix, run:
  # nix-prefetch-git https://github.com/justinwoo/easy-purescript-nix
  #
  # Then, copy the resulting rev and sha256 here.
  # Last update: 2020-12-03
  pursPkgs = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "7802db65618c2ead3a55121355816b4c41d276d9";
    sha256 = "0n99hxxcp9yc8yvx7bx4ac6askinfark7dnps3hzz5v9skrvq15q";
  }) { inherit pkgs; };

  twpurs = import (builtins.fetchGit {
    name = "twpurs";
    url = "https://github.com/gillchristian/tailwind-purs";
    rev = "ddd12b50623f47cd24e3d9525a9f7433100ef2a6";
  });

in pkgs.stdenv.mkDerivation {
  name = "listas.io";
  LOCALE_ARCHIVE="${pkgs.glibcLocales}/lib/locale/locale-archive";
  # TODO add npm & yarn
  buildInputs = with pursPkgs; with twpurs; [
    pursPkgs.purs
    pursPkgs.spago
    pursPkgs.zephyr
    pursPkgs.purs-tidy
    pkgs.nodejs-14_x
    twpurs
  ];
}
