#!/bin/bash
cd app
tar czvf ../`date +"ses-deploy-%Y%m%d-%H%M.tgz"` .
cd ..
