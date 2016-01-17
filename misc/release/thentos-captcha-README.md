# Thentos Captcha

Web service that serves captcha images and sounds and verifies captcha solutions.

## Install

    apt-get install postgresql-client espeak libgmp10

## Configure

The relavent sections in `thentos-captcha.config` are: `frontend`, `backend`, `log`, and `database.

## Run

    ./thentos-captcha --config path/to/thentos-captcha.config
