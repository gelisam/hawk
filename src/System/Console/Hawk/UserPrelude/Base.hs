--   Copyright 2013 Mario Pastorelli (pastorelli.mario@gmail.com) Samuel Gélineau (gelisam@gmail.com)
--
--   Licensed under the Apache License, Version 2.0 (the "License");
--   you may not use this file except in compliance with the License.
--   You may obtain a copy of the License at
--
--       http://www.apache.org/licenses/LICENSE-2.0
--
--   Unless required by applicable law or agreed to in writing, software
--   distributed under the License is distributed on an "AS IS" BASIS,
--   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--   See the License for the specific language governing permissions and
--   limitations under the License.

-- | The types and defaults used by the rest of UserPrelude.
module System.Console.Hawk.UserPrelude.Base where

import Data.ByteString.Char8


-- | hint has `Interpreter.Extension`, but strings are simpler.
type ExtensionName = String

-- | import [qualified] <module-name> [as <qualified-name>]
type QualifiedModule = (String, Maybe String)

-- | The contents of the user prelude.
type Source = ByteString


-- | The user may, but probably won't, give a name to his prelude module.
defaultModuleName :: String
defaultModuleName = "System.Console.Hawk.CachedPrelude"
