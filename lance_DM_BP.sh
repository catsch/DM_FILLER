R DM_list_3901065 --vanilla < WRITE_DM_BP.R
for i in `ls -1 ../../DATA/WORK/3901065/profiles/B*`
do
j=`echo $i | sed s/BR/BD/ | sed s/WORK/DM/`
cp $i $j
done
