{ pkgs, testify, jre, selenium-server-standalone-jar }:
let
  startScript = pkgs.writeScriptBin "start-sequence" ''
    mkdir /tmp
    ${jre}/bin/java -jar ${selenium-server-standalone-jar} &
    echo "starting client"
    ${testify}/bin/client &
    echo "started client; starting server"
    ${testify}/bin/server
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
