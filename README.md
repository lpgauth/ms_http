# ms_http

[![Build Status](https://travis-ci.org/lpgauth/ms_http.svg)](https://travis-ci.org/lpgauth/ms_http)

Erlang microservice example (http)

### Requirements

* Erlang 17.0 +

## API

### kv
```
curl -i -X PUT -H 'Content-Type: text/plain' -d 'bar' 'http://127.0.0.1:8080/api/v1/kv/foo'
curl -i -X GET 'http://127.0.0.1:8080/api/v1/kv/foo'
```

### logger
```
curl -i -X PUT -H 'Content-Type: text/plain' -d 'bar' 'http://127.0.0.1:8080/api/v1/logger/2015-09-06-05'
```

## Tests

```makefile
make dialyzer
make elvis
make eunit
make xref
```

## License

```license
The MIT License (MIT)

Copyright (c) 2015 Louis-Philippe Gauthier

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```
