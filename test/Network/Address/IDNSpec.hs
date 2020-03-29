{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Address.IDNSpec (spec) where

import           Data.ByteString (ByteString)
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import           Test.Hspec (Spec, context, parallel, shouldBe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (Arbitrary(..), elements)

import           Network.Address.IDN (toASCII, toUnicode)

spec :: Spec
spec = parallel $ do

  context "toASCII" $ parallel $ do

    prop "Should encode Unicode to ASCII" $ \UnicodeASCII{..} ->
      toASCII (encodeUtf8 _uaUnicode) `shouldBe` _uaASCII

    prop "Should encode Unicode as ASCII domain" $
      \(UnicodeASCII unicodeSubDomain asciiSubDomain)
       (UnicodeASCII unicodeDomain asciiDomain)
       (UnicodeASCII unicodeTopLevelDomain asciiTopLevelDomain) ->
        let inputDomain = unicodeSubDomain <> "." <> unicodeDomain <> "." <> unicodeTopLevelDomain
            expectedDomain = asciiSubDomain <> "." <> asciiDomain <> "." <> asciiTopLevelDomain
        in toASCII (encodeUtf8 inputDomain) `shouldBe` expectedDomain

  context "toUnicode" $ parallel $ do

    prop "Should encode ASCII to Unicode" $ \UnicodeASCII{..} ->
      toUnicode _uaASCII `shouldBe` encodeUtf8 _uaUnicode

    prop "Should encode ASCII as Unicode domain" $
      \(UnicodeASCII unicodeSubDomain asciiSubDomain)
       (UnicodeASCII unicodeDomain asciiDomain)
       (UnicodeASCII unicodeTopLevelDomain asciiTopLevelDomain) ->
        let inputDomain = asciiSubDomain <> "." <> asciiDomain <> "." <> asciiTopLevelDomain
            expectedDomain =
              unicodeSubDomain <> "." <> unicodeDomain <> "." <> unicodeTopLevelDomain
        in toUnicode inputDomain `shouldBe` encodeUtf8 expectedDomain

data UnicodeASCII = UnicodeASCII
  { _uaUnicode :: Text
  , _uaASCII   :: ByteString }
  deriving (Eq, Show)

instance Arbitrary UnicodeASCII where
  arbitrary = uncurry UnicodeASCII <$> elements
    [ ("कॉम", "xn--11b4c3d")
    , ("セール", "xn--1ck2e1b")
    , ("佛山", "xn--1qqw23a")
    , ("ಭಾರತ", "xn--2scrj9c")
    , ("慈善", "xn--30rr7y")
    , ("集团", "xn--3bst00m")
    , ("在线", "xn--3ds443g")
    , ("한국", "xn--3e0b707e")
    , ("ଭାରତ", "xn--3hcrj9c")
    , ("大众汽车", "xn--3oq18vl8pn36a")
    , ("点看", "xn--3pxu8k")
    , ("คอม", "xn--42c2d9a")
    , ("ভাৰত", "xn--45br5cyl")
    , ("ভারত", "xn--45brj9c")
    , ("八卦", "xn--45q11c")
    , ("موقع", "xn--4gbrim")
    , ("বাংলা", "xn--54b7fta0cc")
    , ("公益", "xn--55qw42g")
    , ("公司", "xn--55qx5d")
    , ("香格里拉", "xn--5su34j936bgsg")
    , ("网站", "xn--5tzm5g")
    , ("移动", "xn--6frz82g")
    , ("我爱你", "xn--6qq986b3xl")
    , ("москва", "xn--80adxhks")
    , ("қаз", "xn--80ao21a")
    , ("католик", "xn--80aqecdr1a")
    , ("онлайн", "xn--80asehdb")
    , ("сайт", "xn--80aswg")
    , ("联通", "xn--8y0a063a")
    , ("срб", "xn--90a3ac")
    , ("бг", "xn--90ae")
    , ("бел", "xn--90ais")
    , ("קום", "xn--9dbq2a")
    , ("时尚", "xn--9et52u")
    , ("微博", "xn--9krt00a")
    , ("淡马锡", "xn--b4w605ferd")
    , ("ファッション", "xn--bck1b9a5dre4c")
    , ("орг", "xn--c1avg")
    , ("नेट", "xn--c2br7g")
    , ("ストア", "xn--cck2b3b")
    , ("삼성", "xn--cg4bki")
    , ("சிங்கப்பூர்", "xn--clchc0ea0b2g2a9gcd")
    , ("商标", "xn--czr694b")
    , ("商店", "xn--czrs0t")
    , ("商城", "xn--czru2d")
    , ("дети", "xn--d1acj3b")
    , ("мкд", "xn--d1alf")
    , ("ею", "xn--e1a4c")
    , ("ポイント", "xn--eckvdtc9d")
    , ("新闻", "xn--efvy88h")
    , ("工行", "xn--estv75g")
    , ("家電", "xn--fct429k")
    , ("كوم", "xn--fhbei")
    , ("中文网", "xn--fiq228c5hs")
    , ("中信", "xn--fiq64b")
    , ("中国", "xn--fiqs8s")
    , ("中國", "xn--fiqz9s")
    , ("娱乐", "xn--fjq720a")
    , ("谷歌", "xn--flw351e")
    , ("భారత్", "xn--fpcrj9c3d")
    , ("ලංකා", "xn--fzc2c9e2c")
    , ("電訊盈科", "xn--fzys8d69uvgm")
    , ("购物", "xn--g2xx48c")
    , ("クラウド", "xn--gckr3f0f")
    , ("ભારત", "xn--gecrj9c")
    , ("通販", "xn--gk3at1e")
    , ("भारतम्", "xn--h2breg3eve")
    , ("भारत", "xn--h2brj9c")
    , ("भारोत", "xn--h2brj9c8c")
    , ("网店", "xn--hxt814e")
    , ("संगठन", "xn--i1b6b1a6a2e")
    , ("餐厅", "xn--imr513n")
    , ("网络", "xn--io0a7i")
    , ("ком", "xn--j1aef")
    , ("укр", "xn--j1amh")
    , ("香港", "xn--j6w193g")
    , ("诺基亚", "xn--jlq61u9w7b")
    , ("食品", "xn--jvr189m")
    , ("飞利浦", "xn--kcrx77d1x4a")
    , ("台湾", "xn--kprw13d")
    , ("台灣", "xn--kpry57d")
    , ("手表", "xn--kpu716f")
    , ("手机", "xn--kput3i")
    , ("мон", "xn--l1acc")
    , ("الجزائر", "xn--lgbbat1ad8j")
    , ("عمان", "xn--mgb9awbf")
    , ("ارامكو", "xn--mgba3a3ejt")
    , ("ایران", "xn--mgba3a4f16a")
    , ("العليان", "xn--mgba7c0bbn0a")
    , ("اتصالات", "xn--mgbaakc7dvf")
    , ("امارات", "xn--mgbaam7a8h")
    , ("بازار", "xn--mgbab2bd")
    , ("موريتانيا", "xn--mgbah1a3hjkrd")
    , ("پاکستان", "xn--mgbai9azgqp6j")
    , ("الاردن", "xn--mgbayh7gpa")
    , ("موبايلي", "xn--mgbb9fbpob")
    , ("بارت", "xn--mgbbh1a")
    , ("بھارت", "xn--mgbbh1a71e")
    , ("المغرب", "xn--mgbc0a9azcg")
    , ("ابوظبي", "xn--mgbca7dzdo")
    , ("السعودية", "xn--mgberp4a5d4ar")
    , ("ڀارت", "xn--mgbgu82a")
    , ("كاثوليك", "xn--mgbi4ecexp")
    , ("سودان", "xn--mgbpl2fh")
    , ("همراه", "xn--mgbt3dhd")
    , ("عراق", "xn--mgbtx2b")
    , ("مليسيا", "xn--mgbx4cd0ab")
    , ("澳門", "xn--mix891f")
    , ("닷컴", "xn--mk1bu44c")
    , ("政府", "xn--mxtq1m")
    , ("شبكة", "xn--ngbc5azd")
    , ("بيتك", "xn--ngbe9e0a")
    , ("عرب", "xn--ngbrx")
    , ("გე", "xn--node")
    , ("机构", "xn--nqv7f")
    , ("组织机构", "xn--nqv7fs00ema")
    , ("健康", "xn--nyqy26a")
    , ("ไทย", "xn--o3cw4h")
    , ("سورية", "xn--ogbpf8fl")
    , ("招聘", "xn--otu796d")
    , ("рус", "xn--p1acf")
    , ("рф", "xn--p1ai")
    , ("珠宝", "xn--pbt977c")
    , ("تونس", "xn--pgbs0dh")
    , ("大拿", "xn--pssy2u")
    , ("みんな", "xn--q9jyb4c")
    , ("グーグル", "xn--qcka1pmc")
    , ("ελ", "xn--qxam")
    , ("世界", "xn--rhqv96g")
    , ("書籍", "xn--rovu88b")
    , ("ഭാരതം", "xn--rvc1e0am3e")
    , ("ਭਾਰਤ", "xn--s9brj9c")
    , ("网址", "xn--ses554g")
    , ("닷넷", "xn--t60b56a")
    , ("コム", "xn--tckwe")
    , ("天主教", "xn--tiq49xqyj")
    , ("游戏", "xn--unup4y")
    , ("vermögensberater", "xn--vermgensberater-ctb")
    , ("vermögensberatung", "xn--vermgensberatung-pwb")
    , ("企业", "xn--vhquv")
    , ("信息", "xn--vuq861b")
    , ("嘉里大酒店", "xn--w4r85el8fhu5dnra")
    , ("嘉里", "xn--w4rs40l")
    , ("مصر", "xn--wgbh1c")
    , ("قطر", "xn--wgbl6a")
    , ("广东", "xn--xhq521b")
    , ("இலங்கை", "xn--xkc2al3hye2a")
    , ("இந்தியா", "xn--xkc2dl3a5ee0h")
    , ("հայ", "xn--y9a3aq")
    , ("新加坡", "xn--yfro4i67o")
    , ("فلسطين", "xn--ygbi2ammx")
    , ("政务", "xn--zfr164b")
    ]
