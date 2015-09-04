#!/bin/bash
exec erl -sname ms_http_dev \
         -pa ebin deps/*/ebin test \
         -config config/dev \
         -boot start_sasl \
         -s ms_http_app \
         -setcookie secret
