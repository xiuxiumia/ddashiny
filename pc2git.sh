#!/bin/bash

git status
git add .
git commit -m "Change"
git pull origin main
git push -u origin main