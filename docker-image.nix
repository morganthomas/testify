{ pkgs, testify, testify-js, jre, selenium-server-standalone-jar }:
let
  startScript = pkgs.writeScriptBin "start-sequence" ''
    mkdir /tmp
    ${jre}/bin/java -jar ${selenium-server-standalone-jar} &
    ln -s ${testify-js.outPath} ./static
    echo "=== starting server ==="
    ${testify.outPath}/bin/server
    echo "=== finished ==="
  '';
in
pkgs.dockerTools.buildLayeredImage {
  name     = "testify";
  tag      = "latest";
  created  = "now";
  contents = [ pkgs.bash
               pkgs.openssh
               pkgs.cacert
               pkgs.coreutils
               pkgs.procps
               pkgs.curl
               pkgs.nettools
               pkgs.phantomjs2
               pkgs.which
             ];
  config = {
    Cmd = [ "${pkgs.bash}/bin/bash" "${startScript}/bin/start-sequence" ];
  };
}
