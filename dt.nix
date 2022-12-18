{ mkDerivation, aeson, base, containers, jose, lens, lib, mtl
, servant, servant-auth-server, servant-server, text
, typelits-witnesses, wai, warp
}:
mkDerivation {
  pname = "dt";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base containers jose lens mtl servant servant-auth-server
    servant-server text typelits-witnesses wai warp
  ];
  executableHaskellDepends = [
    base jose servant servant-auth-server servant-server text wai warp
  ];
  license = lib.licenses.asl20;
  mainProgram = "dt-test";
}
