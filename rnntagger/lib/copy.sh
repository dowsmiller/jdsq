#!/bin/sh

DIR=~/src/PyRNN/data/middle-high-german/Experiments/new-data-100-1-400-2-400-50-10-3-10
cp $DIR/parfile.hyper PyRNN/middle-high-german.hyper
cp $DIR/parfile.io PyRNN/middle-high-german.io
cp $DIR/parfile.rnn PyRNN/middle-high-german.rnn

DIR=~/src/PyRNN/data/early-new-high-german/Experiments/new-data2-100-1-400-2-400-100-0-3-10
cp $DIR/parfile.hyper PyRNN/early-new-high-german.hyper
cp $DIR/parfile.io PyRNN/early-new-high-german.io
cp $DIR/parfile.rnn PyRNN/early-new-high-german.rnn

DIR=~/src/PyNMT/data/middle-high-german/Experiments/new-data-100-2-400-1-400-11
cp $DIR/parfile.hyper PyNMT/middle-high-german.hyper
cp $DIR/parfile.io PyNMT/middle-high-german.io
cp $DIR/parfile.nmt PyNMT/middle-high-german.nmt

DIR=~/src/PyNMT/data/middle-high-german-norm/Experiments/new-data-100-2-400-1-400-10
cp $DIR/parfile.hyper PyNMT/middle-high-german-norm.hyper
cp $DIR/parfile.io PyNMT/middle-high-german-norm.io
cp $DIR/parfile.nmt PyNMT/middle-high-german-norm.nmt

DIR=~/src/PyNMT/data/early-new-high-german/Experiments/default-100-2-400-1-400-11
cp $DIR/parfile.hyper PyNMT/early-new-high-german.hyper
cp $DIR/parfile.io PyNMT/early-new-high-german.io
cp $DIR/parfile.nmt PyNMT/early-new-high-german.nmt





