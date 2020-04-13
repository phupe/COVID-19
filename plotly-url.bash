#! /bin/bash

for file in $(ls *.html)
do
    echo $file
    sed -i -e "s|lib/plotly-main-1.52.2/plotly-latest.min.js|https://cdn.plot.ly/plotly-1.52.2.min.js|g" $file
done

