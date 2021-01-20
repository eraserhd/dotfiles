{ lib, stdenv, fetchFromGitHub
, autoreconfHook
, pkg-config

, kmod       # insmod
, procps     # ps
, psmisc     # fuser
, util-linux # ipcs

, glib
, gtk2
, libmodbus
, libtirpc
, libudev
, libusb

}:

stdenv.mkDerivation rec {
  pname = "linuxcnc";
  version = "2.8.1";

  src = fetchFromGitHub {
    owner = "LinuxCNC";
    repo = "linuxcnc";
    rev = "v${version}";
    sha256 = "3ErJ3ndsUxpWW3e2JW08+MARwa65sIBB9vEo3Vj3ugs=";
  };

  nativeBuildInputs = [
    autoreconfHook
    pkg-config
  ];

  buildInputs = [
    glib
    gtk2
    libmodbus
    libtirpc
    libudev
    libusb
  ];

  propagatedBuildInputs = [
    kmod
    procps
    psmisc
    util-linux
  ];

  configureFlags = [
    #FIXME:
    "--with-realtime=uspace"
  ];

  sourceRoot = "source/src";
  preConfigure = ''
    ./autogen.sh
  '';

  meta = with lib; {
    description = "LinuxCNC controls CNC machines";
    homepage = "https://linuxcnc.org";
    license = licenses.gpl2;
    maintainers = maintainers.eraserhd;
    platforms = platforms.all;
  };
}
