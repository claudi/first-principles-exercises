module RegisteredUser where

newtype Username = UserName String
newtype AccountNumber = AccountNumber Integer

data User = UnregisteredUser
          | RegisteredUser Username AccountNumber
