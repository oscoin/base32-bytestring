`base32-z-bytestring` is an efficient [z-base32][rfc] codec for
bytestrings. The API is similar to the [base16-bytestring][base16-pkg] and
[base64-bytestring][base64-pkg] packages.

### Performance

| function        |      MB/sec     |
|:---------------:|:---------------:|
|encoding         | 400             |
|decoding         | 400             |
|lenient decoding | 250             |

### Maintainer Oscoin Engineering Team <http://oscoin.io>

You can report any issues at [Issue tracker][issues].

### Credits

The original package was created by Sam Truzjan <pxqr.sta@gmail.com>, and the
human-oriented encoding support was added by the Oscoin Engineering Team.

[base16-pkg]: http://hackage.haskell.org/package/base16-bytestring
[base64-pkg]: http://hackage.haskell.org/package/base64-bytestring-1.0.0.1
[rfc]:        https://philzimmermann.com/docs/human-oriented-base-32-encoding.txt
[travis-img]: https://travis-ci.org/pxqr/base32-bytestring.png
[travis-log]: https://travis-ci.org/pxqr/base32-bytestring
[issues]:     https://github.com/pxqr/base32-bytestring/issues
