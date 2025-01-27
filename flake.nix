{
  inputs = {
    garnix-lib.url = "github:garnix-io/garnix-lib?ref=22f244c723924550983237c2b105a7f56006ab4e";
  };

  outputs = inputs: inputs.garnix-lib.lib.mkModules {
    modules = [
    ];

    config = { pkgs, ... }: {

      garnix.deployBranch = "master";
    };
  };
}
