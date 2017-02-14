{-

Ezyang's gist of a module Bed.dump for a program

<bitonic> How hard would it be to create a program using the GHC API that just lists the dependencies, in terms of local files to compile, of a Haskell module? Is there some relevant reference I can read?
<ezyang> bitonic: Pretty easy; even easier if you use a frontend plugin
<bitonic> ezyang: what piece of text should I read? I've never used the GHC API before
<ezyang> depanal in GhcMake gives you ModSummary
<ezyang> those record the dependencies of each module
<ezyang> https://gist.github.com/ezyang/5bdcd632a6f983967dc98acdf50c4744 as an example
<bitonic> ezyang: that is exceedingly cool. Thank you.
<ezyang> bitonic: Build it with your favorite manager; and then invoke ghc -frontend GhcSort -plugin-package ghc-sort -package-db /path/to/db/it/was/installed
<ezyang> maybe it's --frontend
<bitonic> ezyang: I was about to ask -- what do I need to build and how?
<ezyang> you need to put it in a package, just like with other plugins
<bitonic> ezyang: OK, and the package its in does not matter? It'll just look up the module?
<ezyang> well, -plugin-package should identify the package you put this module Bed.in
<bitonic> Ah right
<bitonic> ezyang: I don't see the `--plugin-package` flag in the manual
<bitonic> can I use it compiling it manually somehow?
<ezyang> I think GHC understands how to load a plugin from the modules you are compiling atm
<ezyang> oops, -plugin-package really isn't in the docs :(
<carter> tjakaway  welcome
<carter> bgamari: I'll follow up after I write that email due tomorrow ;)
<bitonic> ezyang: holy moly it works!
<bitonic> You might have just simplified my next week so much
<ezyang> good!
<ezyang> note that ghc still understands all the normal options
<ezyang> so you can, e.g., -dppr-debug
<bitonic> ezyang: I did note!
<bitonic> Since I passed `-ithis -ithat` and it worked
<ezyang> yep
<ezyang> way better than implementing all that from scratch ;)
<bitonic> This is so damn cool
<bitonic> http://giphy.com/gifs/EldfH1VJdbrwY/html5
<bitonic> The tooling possibilities are endless
<ezyang> :)
<ezyang> if you like it, write a blog post about it! :)
<bitonic> ezyang: I'm not the greatest writer
<bitonic> But, who knows!
<bitonic> I am good at nagging people though, so I shall inform people of this
<bitonic> ezyang: BTW, I did not need the package flag
<ezyang> yeah, if you just compiled it in the same directory it will just pick it up
-}


module Bed.GhcSort where

import GHC
import GhcPlugins
import DriverPhases

import Control.Monad
import Data.List
import Data.Maybe
import System.FilePath
import Data.Graph

frontendPlugin :: FrontendPlugin
frontendPlugin = defaultFrontendPlugin {
        frontend = doDiff
    }

doDiff :: [String] -> [(String, Maybe Phase)] -> Ghc ()
doDiff _args srcs = do
    -- API infelicity: we should be CompManager by default...
    dflags <- getSessionDynFlags
    _ <- setSessionDynFlags dflags { ghcMode = CompManager }

    let (hs_srcs, _non_hs_srcs) = partition isHaskellishTarget srcs

    targets <- mapM (uncurry guessTarget) hs_srcs
    setTargets targets
    mod_graph <- depanal [] False

    let full_mg :: [SCC ModSummary]
        full_mg = topSortModuleGraph False mod_graph Nothing

    forM_ (flattenSCCs full_mg) $ \ms ->
        liftIO $ putStrLn (normalise (fromJust (ml_hs_file (ms_location ms))))
    return ()
