rem S=Solid archive  M5=Best compression   R=Recurse subdirs
del essmodel.rar
start /w c:\winrar\winrar a -s -r -m5 -x*.dcu -x*.hlp -x*.gid -x*.exe -x*.res -x*.dll essmodel

