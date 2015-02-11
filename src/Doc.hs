{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE FlexibleInstances                        #-}
{-# LANGUAGE OverloadedStrings                        #-}

{-# OPTIONS -fno-warn-orphans #-}


module Doc where

import Control.Applicative (pure, (<*>))
import Data.Functor.Infix ((<$>))
import Data.Thyme (fromSeconds)
import Data.Thyme.Time ()  -- (instance Num NominalDiffTime)
import Servant.API (Capture)
import Servant.Docs (ToCapture(..), DocCapture(DocCapture), ToSample(toSample))

import Types
import Util


-- instances for generating docs

instance ToCapture (Capture "token" SessionToken) where
    toCapture _ = DocCapture "token" "Session Token"

instance ToCapture (Capture "sid" ServiceId) where
    toCapture _ = DocCapture "sid" "Service ID"

instance ToCapture (Capture "userid" UserId) where
    toCapture _ = DocCapture "userid" "User ID"

instance ToSample Service where
    toSample = Just $ Service "98761234foo" Nothing

instance ToSample Agent where
    toSample = Just . UserA . UserId $ 0

instance ToSample Session where
    toSample =
        Session <$> toSample
                <*> pure (TimeStamp $ read "1986-20-09 00:00:00 UTC")
                <*> pure (TimeStamp $ read "1986-27-09 00:00:00 UTC")
                <*> pure (Timeout 600)

instance ToSample SessionToken where
    toSample = Just "abde1234llkjh"

instance ToSample [SessionToken] where
    toSample = Just ["abde1234llkjh", "47202sdfsg"]

instance ToSample UserFormData where
    toSample = Just $ UserFormData (UserName "Kurt Cobain")
                                   (textToPassword "Hunter2")
                                   (UserEmail "cobain@nirvana.com")

instance ToSample UserName where
    toSample = Just $ UserName "Kurt Cobain"

instance ToSample UserEmail where
    toSample = Just $ UserEmail "cobain@nirvana.com"

instance ToSample UserId where
    toSample = Just $ UserId 12

instance ToSample [UserId] where
    toSample = Just [UserId 3, UserId 7, UserId 23]

instance ToSample ServiceId where
    toSample = Just "23t92ege0n"

instance ToSample [ServiceId] where
    toSample = Just ["23t92ege0n", "f4ghwgegin0"]

instance ToSample (UserId, Timeout) where
    toSample = (,) <$> toSample <*> pure (Timeout $ fromSeconds (123456.0 :: Double))

instance ToSample (UserId, ServiceId) where
    toSample = (,) <$> toSample <*> toSample

instance ToSample () where
    toSample = Just ()

instance ToSample Bool where
    toSample = Just True

{-
instance ToSample (UserId, User) where
    toSample = Nothing
-}

instance ToSample (ServiceId, Service) where
    toSample = Nothing

instance ToSample (ServiceId, ServiceKey) where
    toSample = Nothing

instance ToSample (SessionToken, Session) where
    toSample = Nothing
