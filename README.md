`base32-bytestring` is efficient [base32][rfc] codec for bytestrings.
The API is similar to [base64-bytestring][base64-pkg] package.

### Performance

| function        |      MB/sec     |
|:---------------:|:---------------:|
|encoding         | 400             |
|decoding         | 400             |
|lenient decoding | N/A             |

### Build Status [![Build Status][travis-img]][travis-log]

### Maintainer <pxqr.sta@gmail.com>

You can report any issues at [Issue tracker][issues].

[base64-pkg]: http://hackage.haskell.org/package/base64-bytestring-1.0.0.1
[rfc]:        http://tools.ietf.org/html/rfc4648
[travis-img]: https://travis-ci.org/cobit/base32-bytestring.png
[travis-log]: https://travis-ci.org/cobit/base32-bytestring
[issues]:     https://github.com/cobit/base32-bytestring/issues