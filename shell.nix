let
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/20.03.tar.gz";
  }) {};

  # To update to a newer version of easy-purescript-nix, run:
  # nix-prefetch-git https://github.com/justinwoo/easy-purescript-nix
  #
  # Then, copy the resulting rev and sha256 here.
  # Last update: 2020-12-03
  pursPkgs = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "47bdc016c7d56e987ca1aca690b1d6c9816a8584";
    sha256 = "051fzxd03y0c63sll2bhn0h66dywy9lw6ylyh5vq8fymvix20q94";
  }) { inherit pkgs; };

  twpurs = import (builtins.fetchGit {
    name = "twpurs";
    url = "https://github.com/gillchristian/tailwind-purs";
    rev = "87474189c951320959797b7a77488343de203771";
  });

in pkgs.stdenv.mkDerivation {
  name = "listas.io";
  LOCALE_ARCHIVE="${pkgs.glibcLocales}/lib/locale/locale-archive";
  # TODO add npm & yarn
  buildInputs = with pursPkgs; with twpurs; [
    pursPkgs.purs
    pursPkgs.spago
    pursPkgs.zephyr
    pkgs.nodejs-12_x
    twpurs
  ];
}
