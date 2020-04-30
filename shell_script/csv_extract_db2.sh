# An example to create csv output by pulling data from Db2 in a custom defined time window

#!/bin/bash
#/usr/bin/env bash
set -o errexit
set -o errtrace
set -o nounset

td=$(date +%m.%d.%y)
#echo "${td}"
usage() { echo "Usage: [-s <weekly|all>] or ([-f mm/dd/YYYY] and [-t mm/dd/YYYY])" 1>&2; exit 1; }
s=""
sd="x"
ed="y"

while getopts ":s:f:t:" o; do
    case "${o}" in
        s)
            s=${OPTARG}
            #echo "s is assigned as ${s}"
            if [ ${s} != "weekly" ] && [ ${s} != "all" ] ; then
                usage
                exit 1
            fi
            case ${s} in
                "weekly")
                  wst="created_at >= (CURRENT_TIMESTAMP - 7 DAYS)"
                  ;;
                "all")
                  wst="person_id !=0 "
                  ;;
                *)
                  ;;
            esac
            ;;
        f)
           sd=${OPTARG}
           echo "starting date is ${sd}"
           if ! date -d "${sd}" "+%m/%d/%Y" > /dev/null 2>&1; then
            echo "f input date is wrong"
            usage
            exit 1
           fi
           #if ! date -f "%m/%d/%Y" -j "{sd}" >/dev/null 2>&1; then
            #echo "f date is wrong"
            #usage
            #exit 1
           #fi
           ;;
        t)
          ed=${OPTARG}
          echo "ending date is ${ed}"
          if ! date -d "${ed}" "+%m/%d/%Y" > /dev/null 2>&1; then
             echo "t input date is wrong"
             usage
             exit 1
          fi
          ;;
        *)
            usage
            exit 1
            ;;
    esac
done
shift $((OPTIND-1))


if [ "${s}" == "" ]; then
  if [ "${sd}" == "x" ] || [ "${ed}" == "y" ]; 
    then
      usage
      exit 1
    else
      s="_from_${sd///}_to_${ed///}_rundate"
      echo "s= ${s}"
      wst="date(created_at)>='${sd}' and date(created_at)<='${ed}' "
 fi
fi

echo "wst= ${wst}"

db2 "connect to MDTDB user db2dat01 using xxxxxx"

#export the hypo alert and feedback data
db2 "export to test_alert.csv of del modified by nochardel select id, person_id, json_data, is_liked, created_at, created_at_tz, updated_at from analytics.streams where ${wst} and json_data like '%hypo.1%' "
sed -i '1i id,person_id,json_head,alert_start,alert_end,is_liked,created_at,created_at_tz,updated_at' test_alert.csv
mv test_alert.csv analytics.streams.alert.${s}.${td}

db2 "export to test_feedback.csv of del modified by nochardel select id, person_id, json_data, is_liked, created_at, created_at_tz, updated_at from analytics.streams where ${wst} and json_data like '%notice.1%' "
sed -i '1i id,person_id,json_data,is_liked,created_at,created_at_tz,updated_at' test_feedback.csv
mv test_feedback.csv analytics.streams.feedback.${s}.${td}

#export the mdt sg data
db2 "export to test_sg.csv of del modified by nochardel select id, person_id, sg, sg_timestamp, sg_timestamp_tz, time_change, created_at, updated_at from mdt.sensor_glucose_data where ${wst} "
sed -i '1i id,person_id,sg,sg_timestamp,sg_timestamp_tz,time_change,created_at,updated_at' test_sg.csv
mv test_sg.csv mdt.sensor_glucose_data.${s}.${td}

#export the hypo feature data
db2 "export to test_fv.csv of del modified by nochardel select person_id, partition, sg_timestamp, format_version, created_at, updated_at, features from mdt.hypo_features where ${wst} "
sed -i '1i person_id,partition_number,sg_timestamp,format_version,created_at,updated_at,hypo,recentHypo1hr,recentHypo2hr,recentHypo6hr,recentHypo1Day,recentHypo3Day,recentHypo7Day,recentHypo14Day,recentHypo30Day,recentHypo1to3Day,recentHypo3to7Day,recentHypo7to14Day,recentHypo14to30Day,dayofweek,hourofday,sglatest,sgmean10min,sgmean15min,sgmean20min,sgmean30min,sgmean2hr,sgmean4hr,sgmin30ago,sg2hrago,sg4hrago,sglatestminusthirty,sgthirtyminustwo,sgtwominusfour,sgstdev30min,sgstdev2hr,sgstdev4hr,thirtyminslope,twohourslope,fourhourslope' test_fv.csv
mv test_fv.csv mdt.hypo_features.${s}.${td}

#export the mydata data with one day and one week records
#db2 "export to test_mydata.csv of del modified by nochardel COLDEL| select id, person_id, start_at, start_at_tz, end_at, end_at_tz, period_type, mydata_json_data, created_at, updated_at, updated_at_tz from analytics.mydata_entries where period_type in ('ONE_DAY', 'ONE_WEEK') and ${wst} "
#sed -i '1i id|person_id|start_at|start_at_tz|end_at|end_at_tz|period_type|mydata_json_data|created_at|updated_at|updated_at_tz' test_mydata.csv
#mv test_mydata.csv analytics.mydata.entries.${s}.${td}

