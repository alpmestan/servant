module Servant.API (

  -- * Combinators
  -- | Type-level combinator for expressing subrouting: @':>'@
  module Servant.API.Sub,
  -- | Type-level combinator for alternative endpoints: @':<|>'@
  module Servant.API.Alternative,

  -- * Accessing information from the request
  -- | Capturing parts of the url path as parsed values: @'Capture'@
  module Servant.API.Capture,
  -- | Retrieving parameters from the query string of the 'URI': @'QueryParam'@
  module Servant.API.QueryParam,
  -- | Accessing the request body as a JSON-encoded type: @'ReqBody'@
  module Servant.API.ReqBody,

  -- * Actual endpoints, distinguished by HTTP method
  -- | GET requests
  module Servant.API.Get,
  -- | GET requests for static files
  module Servant.API.File,
  -- | GET requests on the content of a whole directory
  module Servant.API.Dir,
  -- | POST requests
  module Servant.API.Post,
  -- | DELETE requests
  module Servant.API.Delete,
  -- | PUT requests
  module Servant.API.Put,

  -- * Untyped endpoints
  -- | Plugging in a wai 'Network.Wai.Application'
  module Servant.API.Raw,

  -- * Utilities
  -- | QuasiQuotes for endpoints
  module Servant.Utils.ApiQuasiQuoting,
  -- | Type-safe internal URLs
  module Servant.Utils.Links,
  ) where

import Servant.API.Alternative
import Servant.API.Capture
import Servant.API.Delete
import Servant.API.Dir
import Servant.API.File
import Servant.API.Get
import Servant.API.Post
import Servant.API.Put
import Servant.API.QueryParam
import Servant.API.Raw
import Servant.API.ReqBody
import Servant.API.Sub
import Servant.Utils.ApiQuasiQuoting (sitemap)
import Servant.Utils.Links (mkLink)
