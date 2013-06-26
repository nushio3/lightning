while :
do
    inotifywait -e modify --exclude *~ -r src/
    make
done
