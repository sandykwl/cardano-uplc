{- Known-answer tests for the six hash builtins, through the machine.

The digests come from the standards' own examples and from independent
implementations: Python's hashlib, and for Keccak-256 an implementation
checked against the Keccak team's published vectors. Inputs of 65,537 bytes
and more are refused in variants D and E and hashed in A to C.
-}
module Builtins.Hash (hashTests) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Char (digitToInt)
import Data.Word (Word8)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Cardano.UPLC.Builtin (DefaultFun (..))
import Cardano.UPLC.Constant (Constant (..))
import Cardano.UPLC.Machine (EvalError (..), Params (..), SemanticsVariant (..), defaultParams, evaluateWith)
import Cardano.UPLC.Term (Term (..))

hashTests :: TestTree
hashTests =
  testGroup "Hash builtins" (zipWith builtinGroup builtins [0 ..])
  where
    builtins =
      [ ("sha2_256", Sha2_256)
      , ("sha3_256", Sha3_256)
      , ("blake2b_224", Blake2b_224)
      , ("blake2b_256", Blake2b_256)
      , ("keccak_256", Keccak_256)
      , ("ripemd_160", Ripemd_160)
      ]

    builtinGroup (name, f) column =
      testGroup name [row f input (digests !! column) | (input, digests) <- vectors]

data Input = Lit String | Hex String | Rep Char Int

bytesOf :: Input -> BS.ByteString
bytesOf = \case
  Lit s -> BC.pack s
  Hex s -> BS.pack (pairs s)
  Rep c n -> BC.replicate n c
  where
    pairs (a : b : rest) = fromIntegral (digitToInt a * 16 + digitToInt b) : pairs rest
    pairs _ = [] :: [Word8]

describe :: Input -> String
describe = \case
  Lit s | length s > 20 -> show (length s) <> " bytes of text"
  Lit s -> show s
  Hex s -> "0x" <> s
  Rep c n -> show n <> " x " <> show c

fromHex :: String -> BS.ByteString
fromHex = bytesOf . Hex

row :: DefaultFun -> Input -> String -> TestTree
row f input digest = testCase (describe input) $ do
  mapM_ (\v -> run v @?= Right want) (if tooLong then [VariantA, VariantB, VariantC] else variants)
  mapM_ (\v -> run v @?= Left (BuiltinFailure term)) [v | tooLong, v <- [VariantD, VariantE]]
  where
    bytes = bytesOf input
    tooLong = BS.length bytes > 65536
    want = Constant () (CByteString (fromHex digest))
    term = Apply () (Builtin () f) (Constant () (CByteString bytes))
    run v = evaluateWith defaultParams {semanticsVariant = v} term
    variants = [minBound .. maxBound]

vectors :: [(Input, [String])]
vectors =
  [ (Lit "",
      [ "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
      , "a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a"
      , "836cc68931c2e4e3e838602eca1902591d216837bafddfe6f0c8cb07"
      , "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8"
      , "c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"
      , "9c1185a5c5e9fc54612808977ee8f548b2258d31"
      ])
  , (Lit "abc",
      [ "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
      , "3a985da74fe225b2045c172d6bd390bd855f086e3e9d525b46bfe24511431532"
      , "9bd237b02a29e43bdd6738afa5b53ff0eee178d6210b618e4511aec8"
      , "bddd813c634239723171ef3fee98579b94964e3bb1cb3e427262c8c068d52319"
      , "4e03657aea45a94fc7d47ba826c8d667c0d1e6e33a64a036ec44f58fa12d6c45"
      , "8eb208f7e05d987a9b044a8e98c6b087f15a0bfc"
      ])
  , (Lit "a",
      [ "ca978112ca1bbdcafac231b39a23dc4da786eff8147c4e72b9807785afee48bb"
      , "80084bf2fba02475726feb2cab2d8215eab14bc6bdd8bfb2c8151257032ecd8b"
      , "c05d5ea0257c7a4604122b8e99a0093f89d0797ef06a7f0af65a3560"
      , "8928aae63c84d87ea098564d1e03ad813f107add474e56aedd286349c0c03ea4"
      , "3ac225168df54212a25c1c01fd35bebfea408fdac2e31ddd6f80a4bbf9a5f1cb"
      , "0bdc9d2d256b3ee9daae347be6f4dc835a467ffe"
      ])
  , (Lit "message digest",
      [ "f7846f55cf23e14eebeab5b4e1550cad5b509e3348fbc4efa3a1413d393cb650"
      , "edcdb2069366e75243860c18c3a11465eca34bce6143d30c8665cefcfd32bffd"
      , "f305a410b733771b7c5c8ad1041e356ff1da48c51792dfe319ba286b"
      , "31a65b562925c6ffefdafa0ad830f4e33eff148856c2b4754de273814adf8b85"
      , "856ab8a3ad0f6168a4d0ba8d77487243f3655db6fc5b0e1669bc05b1287e0147"
      , "5d0689ef49d2fae572b881b123a85ffa21595f36"
      ])
  , (Lit "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq",
      [ "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"
      , "41c0dba2a9d6240849100376a8235e2c82e1b9998a999e21db32dd97496d3376"
      , "2634ceb48faf94cd6a424287aab968ce3fef39ee5d841760aa5b3164"
      , "5f7a93da9c5621583f22e49e8e91a40cbba37536622235a380f434b9f68e49c4"
      , "45d3b367a6904e6e8d502ee04999a7c27647f91fa845d456525fd352ae3d7371"
      , "12a053384a9c0c88e405a06c27dcf49ada62eb2b"
      ])
  , (Lit "hello world",
      [ "b94d27b9934d3e08a52e52d7da7dabfac484efe37a5380ee9088f7ace2efcde9"
      , "644bcc7e564373040999aac89e7622f3ca71fba1d972fd94a31c3bfbf24e3938"
      , "42d1854b7d69e3b57c64fcc7b4f64171b47dff43fba6ac0499ff437f"
      , "256c83b297114d201b30179f3f0ef0cace9783622da5974326b436178aeef610"
      , "47173285a8d7341e5e972fc677286384f802f8ef42a5ec5f03bbfa254cb01fad"
      , "98c615784ccb5fe5936fbc0cbe9dfdb408d92f0f"
      ])
  , (Lit "The quick brown fox jumps over the lazy dog",
      [ "d7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592"
      , "69070dda01975c8c120c3aada1b282394e7f032fa9cf32f4cb2259a0897dfc04"
      , "477c3985751dd4d1b8c93827ea5310b33bb02a26463a050dffd3e857"
      , "01718cec35cd3d796dd00020e0bfecb473ad23457d063b75eff29c0ffa2e58a9"
      , "4d741b6f1eb29cb2a9b9911c82f56fa8d73b04959d3d9d222895df6c0b28aa15"
      , "37f332f68db77bd9d7edd4969571ad671cf9dd3b"
      ])
  , (Hex "00",
      [ "6e340b9cffb37a989ca544e6bb780a2c78901d3fb33738768511a30617afa01d"
      , "5d53469f20fef4f8eab52b88044ede69c77a6a68a60728609fc4a65ff531e7d0"
      , "0d94e174732ef9aae73f395ab44507bfa983d65023c11a951f0c32e4"
      , "03170a2e7597b7b7e3d84c05391d139a62b157e78786d8c082f29dcf4c111314"
      , "bc36789e7a1e281436464229828f817d6612f7b477d66591ff96a9e064bcc98a"
      , "c81b94933420221a7ac004a90242d8b1d3e5070d"
      ])
  , (Hex "ff",
      [ "a8100ae6aa1940d0b663bb31cd466142ebbdbd5187131b92d93818987832eb89"
      , "444b89ecce395aec5dc98f19defd3a23bca0822fc72226f58ca46a17eeeca442"
      , "17f65d97f383cf41985ff2f90ce52351ace2ab239d9eadf4269a2882"
      , "ac4e4076a883937282f1377ef5cacb8edd00e3c79629e43532464eb3be277367"
      , "8b1a944cf13a9a1c08facb2c9e98623ef3254d2ddb48113885c3e8e97fec8db9"
      , "2c0c45d3ecab80fe060e5f1d7057cd2f8de5e557"
      ])
  , (Rep 'a' 55,
      [ "9f4390f8d30c2dd92ec9f095b65e2b9ae9b0a925a5258e241c9f1e910f734318"
      , "78c2a04624b9328ae0e40cb8cdd29980f6ff55abf2dca68e3412d09eed4b9d03"
      , "3b5b6098086cdb71d54cfdf6accb1f724294fd1f84f2c2c368692a2e"
      , "94ac361af706e2cb330ec00a01b685140fba6c8d9b6079640906593e7753114e"
      , "bb17c0a497f956eb60406de77632af5a598833dac1d41a5f171943dc6aaa519a"
      , "0d8a8c9063a48576a7c97e9f95253a6e53ff6765"
      ])
  , (Rep 'a' 56,
      [ "b35439a4ac6f0948b6d6f9e3c6af0f5f590ce20f1bde7090ef7970686ec6738a"
      , "f6fe8de5c8f5014786f07e9f7b08130f920dd55e587d47021686b26cf2323deb"
      , "aabf6271a7c52fd0120aa019ea25377d7d1f4cff464f031bc1deefef"
      , "749373b29d21d245933e0e994a0070b5db951a9169c3d84f84bc3ca707fce3c8"
      , "86e098d28db0370fc43ce9ced16fa20fd031f0f85f2c200dfc615a46661c4647"
      , "e72334b46c83cc70bef979e15453706c95b888be"
      ])
  , (Rep 'a' 64,
      [ "ffe054fe7ae0cb6dc65c3af9b61d5209f439851db43d0ba5997337df154668eb"
      , "043d104b5480439c7acff8831ee195183928d9b7f8fcb0c655a086a87923ffee"
      , "774d28b31bbe7d48fb48e0d489c17e8537548d344c44250e4bef2a6a"
      , "b1e70fb5c59f0727f1c099e3ddccadc0857c8d21a4b5cd9b637ef964e94c2b29"
      , "1036d73cc8350b0635393d79759b10488165e792073f84d4462e22edec243b92"
      , "9dfb7d374ad924f3f88de96291c33e9abed53e32"
      ])
  , (Rep 'a' 127,
      [ "c57e9278af78fa3cab38667bef4ce29d783787a2f731d4e12200270f0c32320a"
      , "7e377c5822478a844727ef215210bf8d4ae994ccc2088421bd984bc752fc5737"
      , "8c69dc34c82eb6333eaadc3c2f32a50378bef2cedb0da2242524bdab"
      , "59e2f1aba240f20aa591016f5ef429990bc9c2131dcd0d30f0ffd75ed18f317d"
      , "2985b12ded0bf77d3a81e1b504266c2ef82a7abc05aab74dfd3fcd9bf9d25ff2"
      , "64f2d68b85f394e2e4f49009c4bd50224c2698ed"
      ])
  , (Rep 'a' 128,
      [ "6836cf13bac400e9105071cd6af47084dfacad4e5e302c94bfed24e013afb73e"
      , "6ea6efe1dbf991d7f6d442dfe3d6036003071fa55fd438149fb845cb13f15906"
      , "d5df9e9a3d386e984c5464df2c67c4c2b2e74ff4f60fa19f3f37d479"
      , "ae2aa48507885c4c950fb809b2076f959cde9f8ea6da260d9a3587df33dac450"
      , "81555b8e18b3c117311c16373b1aa78c0a84aad7b8f7f4c753d0021fd9a6700e"
      , "8dfdfb32b2ed5cb41a73478b4fd60cc5b4648b15"
      ])
  , (Rep 'a' 129,
      [ "c12cb024a2e5551cca0e08fce8f1c5e314555cc3fef6329ee994a3db752166ae"
      , "84ee1a04bcce09fb1f4507fbdfb81655d71a95e7b546f6859859ccf86d175ae8"
      , "39d2c78556ad2b0fdcfdfdce9c017310a84e5609cfe3ace0804827b7"
      , "2f64744a6de0d2c0b56e64cf6e29a5aaa255010d415d51c75ccc82f73dccd865"
      , "64aa8a28a772ee76c84d6ebde75064fc5282e22a8cf617af1d7e5e08ef9a3248"
      , "62bb9091f499f294f15aa5b951df4d9744d50cf2"
      ])
  , (Rep 'a' 135,
      [ "dfa58dfd72f3c7080d0249a7758fd3636872f63fa24b18473ed36f031e248347"
      , "8094bb53c44cfb1e67b7c30447f9a1c33696d2463ecc1d9c92538913392843c9"
      , "64c5eae369759049d44a22e515de34b9607a83238b9511cb85e271a3"
      , "b5612841c46eda21e2b861ab2ae1731540ff19c1529768db77e9238c10d0717b"
      , "34367dc248bbd832f4e3e69dfaac2f92638bd0bbd18f2912ba4ef454919cf446"
      , "45ce81f2cf08a01135ba1895452d5431d6b013f9"
      ])
  , (Rep 'a' 136,
      [ "6f0e44b9ce4ea61d52a3479c10f60ef916937f799f11964b7f1c7771063905c4"
      , "3fc5559f14db8e453a0a3091edbd2bc25e11528d81c66fa570a4efdcc2695ee1"
      , "d17873cda702cec5f4f43e9ed552310ca980f984e2e0df0a9d5cb881"
      , "998802defa23a4df8f053d4fa9fd2a97a085a4ffb15d5494be644a8f05b4eb4e"
      , "a6c4d403279fe3e0af03729caada8374b5ca54d8065329a3ebcaeb4b60aa386e"
      , "4dbcc675e256fcd05507c62d91a7186d7cd021d3"
      ])
  , (Rep 'a' 137,
      [ "b6dc2da678c065ebdce374ebe1842728277203ee1a9a29832f058cf013d5ad85"
      , "f8d6846cedd2ccfadf15c5879ef95af724d799eed7391fb1c91f95344e738614"
      , "4ab6c8e2bfc39a17cfa9f7eaf97f6c6455c306aa74b0bb1e6d524a77"
      , "b9184415945cf6935cba82c6e4e22971e5cd659bdafddaf085c3de0123b5cfe3"
      , "d869f639c7046b4929fc92a4d988a8b22c55fbadb802c0c66ebcd484f1915f39"
      , "a80a52ac17965045d1f497683228eb07d2fe5fff"
      ])
  , (Rep 'a' 1000,
      [ "41edece42d63e8d9bf515a9ba6932e1c20cbc9f5a5d134645adb5db1b9737ea3"
      , "8f3934e6f7a15698fe0f396b95d8c4440929a8fa6eae140171c068b4549fbf81"
      , "c145e8be163c70538fa23ad8f73aed376b7f505f7abe7bdbd16227fa"
      , "e00b0ddbf1e2cdaf5c898e1a5e8826ea3a2c339bcf2a478da2e5fca9ff126672"
      , "b6a4ac1f51884d71f30fa397a5e155de3099e11fc0edef5d08b646e621e19de9"
      , "aa69deee9a8922e92f8105e007f76110f381e9cf"
      ])
  , (Rep 'a' 65536,
      [ "bf718b6f653bebc184e1479f1935b8da974d701b893afcf49e701f3e2f9f9c5a"
      , "e304a7a94bc86fe7bc81c4c80906794f9f5dde280b8a76027cbfd2e49aa3c395"
      , "9a9379cd52bafb6800dd925e0f08e4743b2f88a56d7dee1243dbb4a5"
      , "333cab8fead7b9b6a07398c7f1237c67853f96329c8c30e183bdce4889f50bf8"
      , "0047a916daa1f92130d870b542e22d3108444f5a7e4429f05762fb647e6ed9ed"
      , "4b75d2612e780c5f3dba6964b8dd3a1e42b0a20d"
      ])
  , (Rep 'a' 65537,
      [ "008ffc88d3c96a9f307524eb361e47c5222a887fc45fa0c1fb8d429c5c23b430"
      , "79732d11ed564d91711771450909b1d7034f6ec8bda85cac992d97c86d2fc370"
      , "1c61c83bede1d90d5cd4055eba87a98a991d3badd5ef906aae9c597c"
      , "9a08ac2ce73629060fea1f323ca6f6d33f6a61fb11f2c11681b9ca710c5252af"
      , "de9754ea8bd851fb25153843eec74fc49f175247db360563fb1cdc264b05fcf1"
      , "6d1e382028b1f0e5a6c07770d792fb8ecf2af269"
      ])
  , (Rep 'a' 131072,
      [ "b44ffb72fcc259676bd80495fef1b44b808ca8f1ffe1b1706a4d7911b0e31f11"
      , "951ff0621bf942d545b7cf42c14a506cadd249b11b982de0f3d11c5e11c1b3a7"
      , "b9e6b2a49227cf9df90b368b33c8683c9389f57148f6f2d1b0bc87f3"
      , "1d0809bc4f6343e9dc9535f0e00e92f27c3f52992172eefff9aaa4f27200d279"
      , "51e4de49973769670d5d69aaca5fe037d263747dd351fcc20a63384f620fa3ff"
      , "7a7f6a9259c713c01599286d2eb6d66f07b9e74a"
      ])
  ]
