module SDLHelper
    ( pSurface
    ) where

import Foreign.Ptr (Ptr)
import SDL
import SDL.Raw.Types

-- | A helper for unmanaged 'Surface's, since it is not exposed by SDL itself.
pSurface :: Ptr SDL.Raw.Types.Surface -> SDL.Surface
pSurface p = SDL.Surface p Nothing