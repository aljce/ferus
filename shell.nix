# A fully reproducible rust build environment via nix
# run `nix-shell` to enter the build enviroment
# run `nix-shell --argstr backtrace true` to enable backtracing
with rec {
  fetch-github =
    { owner  # The owner of the repo
    , repo   # The repo to fetch
    , rev    # The git commit hash you want
    , sha256 # The SHA256 hash of the unpacked archive (for reproducibility)
    }: builtins.fetchTarball {
      url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
      inherit sha256;
    };
  nixpkgs-src = fetch-github {
    owner  = "NixOS";
    repo   = "nixpkgs";
    rev    = "56d94c8c69f8cac518027d191e2f8de678b56088"; # stable 19.03
    sha256 = "1c812ssgmnmh97sarmp8jcykk0g57m8rsbfjg9ql9996ig6crsmi";
  };
  mozilla-overlay-src = fetch-github {
    owner  = "mozilla";
    repo   = "nixpkgs-mozilla";
    rev    = "b52a8b7de89b1fac49302cbaffd4caed4551515f"; # 2019-09-03
    sha256 = "1np4fmcrg6kwlmairyacvhprqixrk7x9h89k813safnlgbgqwrqb";
  };
  mozilla-overlay = import mozilla-overlay-src;
  nixpkgs = import nixpkgs-src { overlays = [ mozilla-overlay ]; };
};
{ backtrace-env ? "false" }:
with rec {
 parseBool = str: let bool = builtins.fromJSON str; in
    if builtins.typeOf bool == "bool" then bool else throw "parseBool: not a boolean";
 backtrace = builtins.fromJSON backtrace-env;
};
nixpkgs.stdenv.mkDerivation {
  name = "rust-env";
  buildInputs = [
    nixpkgs.latest.rustChannels.nightly.rust
  ];
  RUST_BACKTRACE = if backtrace then 1 else 0;
  RUST_MIN_STACK = 100 * 1024 * 1024;
}
