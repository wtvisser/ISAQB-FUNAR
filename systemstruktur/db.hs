module DB where

    import qualified Data.Map as Map
    import Data.Map (Map)

    {-
    put "Mike" 48
    x <- get "Mike"
    put "Mike" (x + 1)
    return (x * 2)
    -}

    -- Programm als Wert

    -- 1. Versuch
    -- data DBProgram = [DBCommand]
    -- data DBCommand = 
    --     Put String Integer
    --   | Get String

    -- p1 = [Put "Mike" 48, Get "Mike", Put "Mike" (??? + 1)]

    -- Der Mechanismus um Dingen ein Namen zu geben, ist Lambda

    -- 2. Versuch
    data DBProg a =  
        Get String (Integer -> DBProg a) -- Sagt, wie es weitergeht
      | Put String Integer (DBProg a) -- "Rest"
      | Done a

    p1 = Put "Mike" 48 
        (Get "Mike"
            (\ x ->  
                Put "Mike" (x + 1) 
                    (Done (show (x * 2)))))

    instance Functor DBProg where
        -- fmap :: (a -> b) -> f a -> f b
        fmap f (Get key fnext) = 
            Get key (\ result -> -- result :: Integer
                fmap f           -- DBProg a -> DBProg b
                (fnext result))  -- DBProg a
        fmap f (Put key value next) =
             Put key value 
             (fmap f next)       -- DBProg b
        fmap f (Done result) =
             Done (f result)

    runDB :: DBProg a -> Map String Integer -> a
    runDB (Get key fnext) db  =
        let (Just result) = Map.lookup key db
        in runDB (fnext result) db
    runDB (Put key value next) db = 
        let db' = Map.insert key value db
        in runDB next db'
    runDB (Done result) db    = result

    -- Wunsch

    get :: String -> DBProg Integer
    get key = Get key Done --(\ result -> Done result)

    put :: String -> Integer -> DBProg () -- void
    put key value = Put key value (Done ())

    -- zwei Progamme aneinanderspleissen
    splice :: DBProg a -> (a -> DBProg b) -> DBProg b
    splice (Get key fnext) f = 
        Get key (\ result -> 
            let dba = fnext result
            in splice dba f
        )
    splice (Put key value next) f =
        Put key value 
            (splice next f)
    splice (Done result) f = f result

    p1' = 
        let m = "Mike"
        in
            put m 48 `splice` (\ _ ->
            get m `splice` (\ x ->
            put m (x + 1) `splice` (\ _ ->
            Done (show (x * 2)))))

    instance Applicative DBProg where
        pure = Done                 -- pure :: a -> f a
        dbf <*> dba = undefined     -- (<*>) :: f (a -> b) -> f a -> f b
        
    instance Monad DBProg where
        (>>=) = splice              -- (>>=) :: m a -> (a -> m b) -> m b 
        return = Done               -- return :: a -> m a

    -- "do" can be used for Monads
    p1'' = 
        do
            put "Mike" 48
            x <- get "Mike"
            put "Mike" (x + 1)
            return (show (x * 2))