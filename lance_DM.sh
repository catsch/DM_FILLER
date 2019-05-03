R DM_list_6901032 --vanilla < WRITE_DM.R
for i in `ls -1 ../../DATA/WORK/6901032/profiles/B*`
do
j=`echo $i | sed s/BR/BD/ | sed s/WORK/DM/`
cp $i $j
done
