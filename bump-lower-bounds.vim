"for future reference, here is a vim macro which, when the cursor is on a line like
"
"  - haskell-src-exts >= 1.0
"
"will update the lower bound to match the version used by the current lts.
0dwdwEld$A '^istack --stack-yaml=stack-oldest-supported-lts.yaml ls dependencies --test | grep '^V:!bashWi>= 0i      - j
