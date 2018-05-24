#!/usr/bin/env bash

# SVG is way better for larger images, but png is easier to view
echo Rendering svg..
cat dist/graphviz.dot | dot -Tsvg > dist/graph.svg
echo Done!
echo Rendering Png..
cat dist/graphviz.dot | dot -Tpng > dist/graph.png
echo Done!
