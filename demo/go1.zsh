#!/bin/zsh -v

curl localhost:3000/add-edge/0/1
curl localhost:3000/add-edge/1/2
curl localhost:3000/add-edge/2/0
curl localhost:3000/add-edge/0/3
curl localhost:3000/add-edge/3/4
curl localhost:3000/add-edge/4/5
curl localhost:3000/add-edge/5/3

