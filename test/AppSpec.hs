
module AppSpec where

import           Control.Exception (throwIO)
import           Data.Either
import           Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import           Network.HTTP.Types
import           Network.Wai (Application)
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Client
import           Test.Hspec

import           App hiding (getItems)

getItems :: ClientM [Item]
getItem :: Integer -> ClientM Item
getItems :<|> getItem = client itemApi

shouldNotOccur :: IO ()
shouldNotOccur = False `shouldBe` True

spec :: Spec
spec = do
  describe "/item" $ do
    withClient mkApp $ do
      it "lists an example item" $ \ env -> do
        try env getItems `shouldReturn` [Item 0 "example item"]

      it "allows to show items by id" $ \ env -> do
        try env (getItem 0) `shouldReturn` Item 0 "example item"

      it "throws a 404 for missing items" $ \ env -> do
        respM <- runClientM (getItem 42) env
        (return $ isLeft respM) `shouldReturn` True
        case respM of
          Left(sError) ->
            case sError of
              FailureResponse (failureResp) -> responseStatusCode failureResp `shouldBe` status404
              _ -> shouldNotOccur
          Right(item) -> shouldNotOccur

        --responseStatusCode resp `shouldReturn` status404
        --runClientM (getItem 42) env `shouldReturn` FailureReponse(resp)

withClient :: IO Application -> SpecWith ClientEnv -> SpecWith ()
withClient x innerSpec =
  beforeAll (newManager defaultManagerSettings) $ do
    flip aroundWith innerSpec $ \ action -> \ manager -> do
      testWithApplication x $ \ port -> do
        let baseUrl = BaseUrl Http "localhost" port ""
        action (ClientEnv manager baseUrl Nothing)

type Host = (Manager, BaseUrl)

try :: ClientEnv -> ClientM a -> IO a
try clientEnv action = either throwIO return =<<
  runClientM action clientEnv
