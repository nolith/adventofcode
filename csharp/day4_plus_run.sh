#!/bin/bash

export DEBUG=1
cat inputs/day4.txt| dotnet run 4 | grep northpole
