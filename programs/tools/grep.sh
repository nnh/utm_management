#!/bin/sh
DATA=`cat macAddr.txt`

while read line
do
  temp=`echo ${line//:/}`
  temp2=`echo ${temp:0:6}-${temp:6:12}`
  echo $temp2
  grep $temp2 ../macAddressList/*.txt > $temp2.txt
done << FILE
$DATA
FILE
