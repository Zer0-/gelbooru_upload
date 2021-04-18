#
# Pre-flight Checklist:
#   - catalog off (theme_settings in mysql)
#   - regen pages hack (container website inc/display.php)
#   - container service scripts (dumpall, cleanup) are running
#   - host scripts ok (cleanup.sh, load_tables.sh)
#   - calling cleanup 8082
#   - calling dumpall 8081
#   - SLEEP is accurate
#
set -e

SLEEP=420
#SLEEP=1
LOGDIR=./logs
BIN=./dist-newstyle/build/x86_64-linux/ghc-8.6.5/bunkerchan-upload-0.1.0.0/x/bunkerchan-upload/build/bunkerchan-upload/bunkerchan-upload

cpus=2

function dolongthing() {
    sleep 10
    echo "long thing done"
}

function mkNewDataDir() {
    FILE=$LOGDIR/N.txt
    if test -f "$FILE"; then
        echo "$FILE exists."
        oldN=$(cat "$FILE")
        N=$(expr $oldN + 1)
    else
        echo "$FILE does not exist."
        N=1
    fi

    echo $N
    echo $N > "$FILE"

    dataDir=../data$N
    picsDir=../data_2

    mkdir "$dataDir"

    upl_b="$BIN +RTS -N$cpus -RTS $dataDir $picsDir b"
    upl_leftypol="$BIN +RTS -N$cpus -RTS $dataDir $picsDir leftypol"
    upl_anime="$BIN +RTS -N$cpus -RTS $dataDir $picsDir anime"
    upl_dead="$BIN +RTS -N$cpus -RTS $dataDir $picsDir dead"
    upl_edu="$BIN +RTS -N$cpus -RTS $dataDir $picsDir edu"
    upl_games="$BIN +RTS -N$cpus -RTS $dataDir $picsDir games"
    upl_get="$BIN +RTS -N$cpus -RTS $dataDir $picsDir GET"
    upl_gulag="$BIN +RTS -N$cpus -RTS $dataDir $picsDir gulag"
    upl_hobby="$BIN +RTS -N$cpus -RTS $dataDir $picsDir hobby"
    upl_ref="$BIN +RTS -N$cpus -RTS $dataDir $picsDir ref"
    upl_tech="$BIN +RTS -N$cpus -RTS $dataDir $picsDir tech"
}


function main() {
    mkNewDataDir
    rm -f $dataDir/*
    echo "" > $LOGDIR/log.txt

    # tells our container to run cleanup scripts
    curl -v localhost:8082; sleep .1; curl -v localhost:8082

    for i in "$upl_b" "$upl_leftypol" "$upl_hobby" "$upl_tech" "$upl_gulag" "$upl_ref" "$upl_get" "$upl_games" "$upl_edu" "$upl_dead" "$upl_anime"
    do
        echo $i
        ($i 2> /dev/stdout >> $LOGDIR/log.txt) &
        #(dolongthing >> $LOGDIR/log.txt) &
        sleep_time=$SLEEP
        echo "sleeping $sleep_time seconds"
        sleep $sleep_time
    done

    echo "$(jobs -p)"
    wait $(jobs -p)

    # tells our container to synchronize data with remote server
    # which in turn tells the imageboard server to run its cleanup scripts
    curl -v localhost:8081; sleep .1; curl -v localhost:8081
    echo "main run function finished"
}


while true
do
    { time main > /dev/stdout 2>&1; } > /dev/stdout 2>&1 | tee -a /tmp/asdf.txt
    sleep 7200
done


