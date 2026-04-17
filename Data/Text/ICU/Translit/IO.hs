{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}

module Data.Text.ICU.Translit.IO
 where

import Foreign
import Data.Text (Text)
import Data.Text.Foreign
import Data.Text.ICU.Translit.ICUHelper
#if MIN_VERSION_text(2,0,0)
import Foreign.C.String (CString)
#endif

data UTransliterator

foreign import ccall "trans.h __hs_translit_open_trans" openTrans
    :: Ptr UChar -> Int -> Ptr UErrorCode -> IO (Ptr UTransliterator)
foreign import ccall "trans.h &__hs_translit_close_trans" closeTrans
    :: FunPtr (Ptr UTransliterator -> IO ())
foreign import ccall "trans.h __hs_translit_do_trans" doTrans
    :: Ptr UTransliterator -> Ptr UChar -> Int32 -> Int32
    -> Ptr UErrorCode -> IO Int32



data Transliterator = Transliterator {
      transPtr :: ForeignPtr UTransliterator,
      transSpec :: Text
    }


instance Show Transliterator where
    show tr = "Transliterator " ++ show (transSpec tr)



transliterator :: Text -> IO Transliterator
transliterator spec =
    compatUseAsPtr spec $ \ptr len -> do
           q <- handleError $ openTrans ptr (fromIntegral len)
           ref <- newForeignPtr closeTrans q
           touchForeignPtr ref
           return $ Transliterator ref spec


transliterate :: Transliterator -> Text -> IO Text
transliterate tr txt = do
  (fptr, len) <- compatAsForeignPtr txt
  withForeignPtr fptr $ \ptr ->
      withForeignPtr (transPtr tr) $ \tr_ptr -> do
             handleFilledOverflowError ptr (fromIntegral len)
                 (\dptr dlen ->
                        doTrans tr_ptr dptr (fromIntegral len) (fromIntegral dlen))
                 (\dptr dlen ->
                        compatFromPtr dptr (fromIntegral dlen))



compatUseAsPtr :: Text -> (Ptr UChar -> I16 -> IO a) -> IO a
compatAsForeignPtr :: Text -> IO (ForeignPtr UChar, I16)
compatFromPtr :: Ptr UChar -> I16 -> IO Text

#if !MIN_VERSION_text(2,0,0)

compatUseAsPtr = useAsPtr
compatAsForeignPtr = asForeignPtr
compatFromPtr = fromPtr

#else

compatUseAsPtr = useAsUCharPtr
compatAsForeignPtr = asUCharForeignPtr
compatFromPtr = fromUCharPtr

-- Stolen from a hidden module from https://github.com/haskell/text-icu
useAsUCharPtr :: Text -> (Ptr UChar -> I16 -> IO a) -> IO a
useAsUCharPtr t act = useAsPtr t $ \tptr tlen ->
    allocaArray (fromIntegral tlen) $ \ dst ->
        act dst =<< fromUtf8 dst tptr tlen

asUCharForeignPtr :: Text -> IO (ForeignPtr UChar, I16)
asUCharForeignPtr t = useAsPtr t $ \tptr tlen -> do
    fp <- mallocForeignPtrArray (fromIntegral tlen)
    withForeignPtr fp $ \ dst -> (fp,) <$> fromUtf8 dst tptr tlen

fromUtf8 :: Ptr UChar -> Ptr Word8 -> I8 -> IO I16
fromUtf8 dst tptr tlen =
    with 0 $ \ err ->
    with 0 $ \ dstLen -> do
        _ <- u_strFromUTF8Lenient dst (fromIntegral tlen) dstLen tptr
              (fromIntegral tlen) err
        fromIntegral <$> peek dstLen

fromUCharPtr :: Ptr UChar -> I16 -> IO Text
fromUCharPtr p l =
    with 0 $ \ err ->
    with 0 $ \ dstLen ->
    allocaArray capacity $ \ dst -> do
        _ <- u_strToUTF8 dst (fromIntegral capacity) dstLen p
            (fromIntegral l) err
        dl <- peek dstLen
        fromPtr dst (fromIntegral dl)
    where capacity = fromIntegral l * 3

foreign import ccall "text_icu.h __hs_u_strFromUTF8Lenient" u_strFromUTF8Lenient
    :: Ptr UChar -> Int32 -> Ptr Int32 -> Ptr Word8 -> Int32 -> Ptr UErrorCode
    -> IO CString

foreign import ccall "text_icu.h __hs_u_strToUTF8" u_strToUTF8
    :: Ptr Word8 -> Int32 -> Ptr Int32 -> Ptr UChar -> Int32 -> Ptr UErrorCode
    -> IO CString

newtype I16 = I16 Int
    deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

#endif