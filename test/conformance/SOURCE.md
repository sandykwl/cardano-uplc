# Conformance cases

These files come from `plutus-conformance/test-cases/uplc` in
https://github.com/IntersectMBO/plutus, at commit `02f74fb600737321c55600b0d2ead58ab45ff1f8`
(committed 2026-09-23, copied 2026-10-10).
Each case keeps its path below `test-cases/uplc`, and only its `.flat` input
and `.flat.expected` result are here. `test/Conformance.hs` runs them.

450 cases are here: those whose `.uplc` uses no builtin outside
`addInteger`, `andByteString`, `appendByteString`, `byteStringToInteger`, `complementByteString`, `consByteString`, `countSetBits`, `divideInteger`, `equalsByteString`, `equalsInteger`, `expModInteger`, `findFirstSetBit`, `ifThenElse`, `indexByteString`, `integerToByteString`, `lengthOfByteString`, `lessThanByteString`, `lessThanEqualsByteString`, `lessThanEqualsInteger`, `lessThanInteger`, `modInteger`, `multiplyInteger`, `orByteString`, `quotientInteger`, `readBit`, `remainderInteger`, `replicateByte`, `rotateByteString`, `shiftByteString`, `sliceByteString`, `subtractInteger`, `writeBits`, `xorByteString`, and no `constr` or `case`, and that have a `.flat`. The text-parser
cases have none; 102 otherwise qualifying cases were left out for that.

## Licence

The files are under the Apache License, Version 2.0, in `LICENSE` beside this
one. The upstream `NOTICE` reads:

```
Copyright 2023 Input Output Global, Inc.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
```

## Refreshing

```
scripts/conformance/refresh.py <plutus checkout> <builtin name>...
```

Name every builtin the machine implements. The script empties this
directory and fills it again, so cases that no longer qualify disappear.
