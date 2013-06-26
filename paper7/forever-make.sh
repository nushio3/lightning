while :
do
    inotifywait -r --exclude *~ src/
    make
done
