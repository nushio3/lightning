while :
do
    make
    inotifywait -e modify --exclude *~ -r src/
done
