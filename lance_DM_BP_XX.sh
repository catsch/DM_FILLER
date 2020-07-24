R DM_list_6902667 --vanilla < WRITE_DM_BP_XX.R
for i in `ls -1 ../../DATA/WORK/6902667/profiles/B*`
do
j=`echo $i | sed s/BR/BD/ | sed s/WORK/DM/`
cp $i $j
done
