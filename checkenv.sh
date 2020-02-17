environment=good
OPAMV=2.*
OCAMLV=4.08.*
SWITCH=4.08.*
OUNITV=2.*
OGRAPHICS=4.*
OYOJSONV=1.7.*


OPAM_LOCATION="$(command -v opam)"
if [[ $OPAM_LOCATION == "" ]]; then
  echo "OPAM is NOT available. This is bad."
  environment=bad
else
  echo "OPAM is available. Good."
fi

SWITCH="$(opam switch show 2>&1)"
if [[ $SWITCH =~ $OCAMLV ]]; then
  echo "OPAM switch $OCAMLV is active. Good."
else
  echo "OPAM switch $OCAMLV is NOT active. This is bad."
  echo "The active switch is: $SWITCH"
  environment=bad
fi

OUNIT_VERSION="$(opam info ounit -f version 2>&1)"
if [[ $OUNIT_VERSION =~ $OUNITV ]]; then
  echo "OUnit version $OUNITV is active. Good."
else
  echo "OUnit version $OUNITV is NOT active. This is bad."
  echo "The active version of OUnit is: $OUNIT_VERSION"
  environment=bad
fi

OGRAPHICS_CURRENT="$(opam info graphics -f version 2>&1)"
if [[ $OGRAPHICS_CURRENT =~ $OGRAPHICS ]]; then
  echo "graphics version $OGRAPHICS is active. Good."
else
  echo "graphics version $OGRAPHICS is NOT active. This is bad."
  echo "The active version of graphics is: $OGRAPHICS_CURRENT"
  environment=bad
fi

OYOJSON_CURRENT="$(opam info yojson -f version 2>&1)"
if [[ $OYOJSON_CURRENT =~ $OYOJSONV ]]; then
  echo "yojson version $OYOJSONV is active. Good."
else
  echo "yojson version $OYOJSONV is NOT active. This is bad."
  echo "The active version of yojson is: $OYOJSON_CURRENT"
  environment=bad
fi

if [[ $environment == good ]]; then
  cat <<EOF
===========================================================
All dependencies are installed. Congratulations!
===========================================================
EOF
else
  cat <<EOF
===========================================================
WARNING

Some of packages or enviroment settings are wrong.
===========================================================
EOF
fi
