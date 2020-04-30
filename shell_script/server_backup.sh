# An example for zookeeper server backup

#!/bin/bash
#


ZK_SRC_DATA=/disk1/ZookeeperData/version-2
ZK_BACKUP_DIR=/disk1/zk_backups
zk_mode=$(echo stat | nc localhost 2181 | grep Mode)
mode=$(echo "${zk_mode#*:}" | xargs)
LATEST_BACKUP=Analytics_ZK_Data_${HOSTNAME%%.*}_${mode}.$(date +%Y%m%d%H%M)
BASE_DIR=`pwd`
COS_DIR=/opt/backup/tools
COS=$COS_DIR/cos.py
BUCKET_NAME=dev-sugariq-backups

mkdir -p $ZK_BACKUP_DIR
mkdir $ZK_BACKUP_DIR/$LATEST_BACKUP

# Copy to intermediary location
cp -r $ZK_SRC_DATA $ZK_BACKUP_DIR/$LATEST_BACKUP
tar -cvzf $ZK_BACKUP_DIR/$LATEST_BACKUP.tgz $ZK_BACKUP_DIR/$LATEST_BACKUP
chmod -R 755 $ZK_BACKUP_DIR

# Transfer to COS
$COS -c $COS_DIR/cred.json -b $BUCKET_NAME upload $ZK_BACKUP_DIR/$LATEST_BACKUP.tgz

# Cleanup
cd $ZK_BACKUP_DIR || exit
ls -1tr $ZK_BACKUP_DIR | head -n -4 | xargs -d '\n' rm -rf --
rm -rf ${ZK_BACKUP_DIR:?}/*/
cd $BASE_DIR || exit
