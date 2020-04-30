# An example for Kafka Zookeeper restore by using cos tool download backups from IBM Cloud Storage

#!/bin/bash


#set -x
#shopt -s extglob

ZK_SRC_DATA=/disk1/kafka/zookeeper/version-2
ZK_RESTORE_DIR=/disk1/zk_restore
#zk_mode=$(echo stat | nc localhost 2181 | grep Mode)
#mode=$(echo "${zk_mode#*:}" | xargs)
BASE_DIR=`pwd`
COS_DIR=/opt/backup/tools
COS=$COS_DIR/cos.py
BUCKET_NAME=dev-sugariq-backups

echo "Usage: ./zookeeper_restore.sh -t [zk_mode.backup_timestamp] (-t leader.202002191605 | -t follower.202002191554)"

while getopts ":t:" backupts; do
   case $backupts in
   t)
     BACKUP_FILE=Kafka_ZK_Data_${HOSTNAME%%.*}_${OPTARG}
     echo "the specified backup file to download from cloud is ${BACKUP_FILE}.tgz"
     ;;
   \?)
     echo "Usage: ./zookeeper_restore.sh -t [zk_mode.backup_timestamp] (-t leader.202002191605 | -t follower.202002191554)"
     exit 1
     ;; 
   esac
done

#Create restore folder
mkdir -p $ZK_RESTORE_DIR

# Transfer to COS
cd $ZK_RESTORE_DIR
$COS -c $COS_DIR/cred.json -b $BUCKET_NAME download ${BACKUP_FILE}.tgz

# Decompress the tar to restore dir
tar xvfz  ${BACKUP_FILE}.tgz 

# Remove the current log dir and rename the restore directory to the log dir
cd disk1/zk_backups/${BACKUP_FILE}
rm -rf ${ZK_SRC_DATA}
mv version-2 /disk1/kafka/zookeeper/.

# Change the ownership
#chown -R kafka:kafka ${ZK_SRC_DATA}
chmod -R 755 ${ZK_SRC_DATA}


# Cleanup
rm -rf ${ZK_RESTORE_DIR}
cd $BASE_DIR || exit
