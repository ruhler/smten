
import Smten.Semantics

test :: (Monad m) => String -> Bool -> m ()
test m True = return ()
test m False = error m

p :: Bool -> Bool -> Bool -> Bool
p x y z =
    if x && y
        then not z
        else x == (y || z)

qcore :: SMT ()
qcore = do
    test "core.dummy" True

    -- Trivial test. Verifies we can make free booleans and assert things
    -- about them.
    ra <- query $ do
        a <- free
        assert a
        return a
    test "core.trivial" (ra == Just True)

    -- Verify we can handle functions in let expressions.
    ra2 <- query $
        assert (let f = (\x -> x)
                in f True)
    test "core.finlet" (ra2 == Just ())

    -- Test NOT
    rb <- query $ do
        b <- free
        assert (not b)
        return b
    test "core.not" (rb == Just False)

    -- Test OR
    rcd <- query $ do
        c <- free
        d <- free
        assert (c || d)
        assert (not c)
        return (c, d)
    test "core.or" (rcd == Just (False, True))

    -- Test AND
    ref <- query $ do
        e <- free
        f <- free
        assert (e && f)
        return (e, f)
    test "core.and" (ref == Just (True, True))

    -- Test EQ
    rgh <- query $ do
        g <- free
        h <- free
        assert (g == h)
        assert (not g)
        return (g, h)
    test "core.eq" (rgh == Just (False, False))

    -- Test IF
    ri <- query $ do
        i <- free
        assert (if i then False else True)
        return i
    test "core.if" (ri == Just False)

    -- Test more complex
    rjkl <- query $ do
        j <- free
        k <- free
        l <- free
        assert (p j k l)
        return (j, k, l)
    test "core.complex" $
        case rjkl of
            Just (jv, kv, lv) -> p jv kv lv
            _ -> False

    -- Test an issue with lambdas that we've had issues with in the past.
    rm <- query $ do
        m <- free
        assert ((if m then (==) True else (==) False) False)
        return m
    test "core.lambda" (rm == Just False)

    -- We should be able to using Integers, so long as they aren't free, even
    -- if the underlying solver doesn't support them.
    rno <- query $ do
        n <- free
        o <- free
        assert ((if n then 3 else (4 :: Integer)) == (if o then 4 else 5))
        return (n, o)
    test "core.integer" (rno == Just (False, True))

    -- Same with lists
    rp <- query $ do
        p <- free
        assert (null (if p then [1, 2, 3 :: Integer] else []))
        return p
    test "core.list" (rp == Just False)

    -- Same with char
    rq <- query $ do
        q <- free
        assert ('a' == (if q then 'a' else 'b'))
        return q
    test "core.char" (rq == Just True)

    qsubstitute

-- Test that primitives are reapplied after substitution.
qsubstitute :: SMT ()
qsubstitute = do
    r <- query $ do
        a <- free
        assert (not a)
        return (not a)
    test "core.substitute" (r == Just True)


qref :: SMT ()
qref = do
    nest $ do
        xref <- use free
        use $ used xref >>= assert
        rx <- query $ used xref
        test "qref.simple" (rx == Just True)

        use $ used xref >>= (assert . not)
        rx2 <- query $ used xref
        test "qref.reuse" (rx2 == Nothing)

    nest $ do
        xref <- use $ free
        use $ do
            v1 <- used xref
            assert v1
            v2 <- used xref
            assert (not v2)
        rx <- query (return ())
        test "qref.dupref" (rx == Nothing)

    nest $ do
        xref <- use $ do
           x <- free
           assert (not x)
           return x
        yref <- use free
        r <- query $ do
            vx <- used xref
            vy <- used yref
            assert (vx || vy)
            return vy
        test "qref.join" (r == Just True)
        
main :: IO ()
main = do
    runSMT qcore
    runSMT qref
    putStrLn "SEMTEST PASSED"
