#!/bin/bash
echo "Run lam model"

ext="tpl"
exe="Lam"
out="../../output/base"
DAT="../../input/base/stock_LAN.dat"
#DAT="../input/stock_LAN1985.dat"
don=${PWD##*/}

echo $don


if [ -f "$exe" ]; then
    echo "$exe exists"
    ./$exe -nox -iprint 50 -ind $DAT
else
    echo "$exe does not exist"
    admb -f $exe
    chmod a+x $exe
    ./$exe -nox -iprint 50 -ind $DAT
fi

mkdir -p $out
mv -v {*.cor,*.std,*.par,*.rep} $out
#mv -v $exe.cor $out/$don.cor
#mv -v $exe.std $out/$don.std
#mv -v $exe.par $out/$don.par
#mv -v $exe.rep $out/$don.rep
#mv -v $exe.bar $out/$don.bar
#cp -v proyecciones.txt $out/$don.prj
#cp -v proyecciones.txt $out/Rec2.prj
#cp -v proyecciones.txt $out/Rec3.prj
#mv -v proyecciones.lam $out/$don.prj


if [ -d $1 ]; then
  echo "clean all variables"
  rm -f *.eva *.log *.rpt *.htp *.cor *.par *.r* *.b* *.p* *.obj *.csv
  rm -f *.rep *.bar *.psv *.std $exe.cpp admodel.* variance *.o *.ecm *.mc2 *.mcm
else
  echo "no clean"
fi
