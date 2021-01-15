{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs, ... }:
    let
      pkgs = import nixpkgs {
        system = "x86_64-linux";
        overlays = [ self.overlay ];
      };
    in with pkgs; {
      overlay = self: super:
        let
          hpkgs = super.haskellPackages;
          tg-lambdabot = hpkgs.callCabal2nix "tg-lambdabot" ./. { };
        in with super;
        with haskell.lib; {
          inherit tg-lambdabot;
          tg-lambdabot-dev =
            addBuildTools tg-lambdabot [ haskell-language-server cabal-install ];
        };
      defaultPackage.x86_64-linux = tg-lambdabot;
      devShell.x86_64-linux = tg-lambdabot-dev.envFunc { };
    };
}
