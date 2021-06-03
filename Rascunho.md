``` haskell {curried versions}
out ∷ [a] × [b] → Either () ((a × b) × ([a] × [b]))
out = \case
  ([],_)           → Left ()
  (_,[])           → Left ()
  ((a:as), (b:bs)) → Right ((a,b) , (as,bs))

zip' ∷ [a] × [b] → [a × b]
zip' = anaList out

zipWith' ∷ ((a × b) → c) → ([a] × [b]) → [c]
zipWith' f = fmap f . zip'

sequenceA' ∷ [t → a] → t → [a]
sequenceA' = flip aux where
  aux a = cataList (either nil h)  where
    h (f, fs) = f a : fs

zipWithM' ∷ ((a × b) → t → c) → ([a] × [b]) → t → [c]
zipWithM' f = sequenceA' . zipWith' f

calcLine' ∷ [ℚ] → [ℚ] → Float → [ℚ]
calcLine' = curry aux where
  aux = zipWithM' (uncurry linear1d)
```

```haskell {uncurried}
out ∷ [a] → [b] → Either () ((a × b) × ([a] × [b]))
out [] _          = Left ()
out _ []          = Left ()
out (a:as) (b:bs) = Right ((a,b) , (as,bs))

zip' ∷ [a] → [b] → [a × b]
zip' = curry (anaList (uncurry out))

zipWith' ∷ (a → b → c) → [a] → [b] → [c]
zipWith' op l r = fmap Cp.ap (zip' (fmap op l) r)

zipWithM' ∷ (a  → b  → p  → d)  → [a]  → [b]  → p  → [d]
zipWithM' f xs ys = mySequenceA (myZipWith f xs ys)

calcLine' ∷ [ℚ] → [ℚ] → Float → [ℚ]
calcLine' = zipWithM' linear1d
```

```haskell {information given}
linear1d ∷ Rational → Rational → OverTime Rational
linear1d = formula
 where
  formula ∷ Rational → Rational → Float → Rational
  formula x y t = ((1.0 ∷ Rational) - (toRational t)) * x + (toRational t) * y
```

```haskell {pointwise}
out ∷ [a] → [b] → Either () ((a × b) × ([a] × [b]))
out [] _          = Left ()
out _ []          = Left ()
out (a:as) (b:bs) = Right ((a,b) , (as,bs))

zip' ∷ [a] → [b] → [a × b]
zip' = curry (anaList (uncurry out))

zipWith' ∷ (a → b → c) → [a] → [b] → [c]
zipWith' op l r = fmap Cp.ap (zip' (fmap op l) r)

sequenceA' ∷ [t → a] → t → [a]
sequenceA' = flip aux where
  aux a = cataList (either nil h)  where
    h (f, fs) = f a : fs

zipWithM' ∷ (a  → b  → p  → d)  → [a]  → [b]  → p  → [d]
zipWithM' f xs ys = sequenceA' (zipWith' f xs ys)

calcLine' ∷ [ℚ] → [ℚ] → Float → [ℚ]
calcLine' = zipWithM' linear1d
```

```haskell {trying to calculate}
out ∷ [a] × [b] → Either () ((a × b) × ([a] × [b]))
out = \case
  ([],_)           → Left ()
  (_,[])           → Left ()
  ((a:as), (b:bs)) → Right ((a,b) , (as,bs))

zip' ∷ [a] × [b] → [a × b]
zip' = anaList out

zipWith' ∷ ((a × b) → c) → ([a] × [b]) → [c]
zipWith' f = fmap f . zip'

sequenceA' ∷ [e → fe] → e → [fe]
sequenceA' = flip aux where
  aux a = cataList (either nil h)  where
    h (f, fs) = f a : fs

zipWithM' ∷ ((a × b) → t → c) → ([a] × [b]) → t → [c]
zipWithM' f = sequenceA' . zipWith' f

calcLine' ∷ [ℚ] → [ℚ] → Float → [ℚ]
calcLine' = curry aux where
  aux = zipWithM' (uncurry linear1d)
  {- zipWithM' f = sequenceA' ⋅ zipWith' f -}
    -- aux = sequenceA' ⋅ zipWith' (uncurry linear1d)
  {- zipWith' f = fmap f ⋅ zip' -}
    -- aux = sequenceA' ⋅ (fmap (uncurry linear1d) ⋅ zip')
  {- zip' = anaList out -}
    -- aux = sequenceA' ⋅ (fmap (uncurry linear1d) ⋅ (anaList out))
```

```haskell {making simple definitions}
out ∷ [a] × [b] → Either () ((a × b) × ([a] × [b]))
out = \case
  ([],_)           → Left ()
  (_,[])           → Left ()
  ((a:as), (b:bs)) → Right ((a,b) , (as,bs))

zip' ∷ [a] × [b] → [a × b]
zip' = anaList out

zipWith' ∷ (a → b → c) → [a] → [b] → [c]
zipWith' op l r = fmap op' t where
  op' = uncurry op
  t = zip' (l,r)

zipWithM' ∷ (a  → b  → c  → d)  → [a]  → [b]  → c  → [d]
zipWithM' f xs ys = sequenceA' (zipWith' f xs ys)

sequenceA' ∷ [e → fe] → e → [fe]
sequenceA' fs e = [f e | f ← fs]

calcLine' ∷ [ℚ] → [ℚ] → Float → [ℚ]
calcLine' = zipWithM' linear1d

- calcLine' = zipWithM' linear1d
≡ calcLine' xs ys = zipWithM' linear1d xs ys
≡ calcLine' xs ys = sequenceA' (zipWith' linear1d xs ys)
≡ calcLine' xs ys = sequenceA' (fmap (uncurry linear1d) $ zip' (xs,ys))
≡ calcLine' xs ys e = sequenceA' (fmap (uncurry linear1d) $ zip' (xs,ys)) e
≡ calcLine' xs ys e = [f e | f ← (fmap (uncurry linear1d) $ zip' (xs,ys))]
≡ calcLine' xs ys e = cataList (either nil h) ((fmap (uncurry linear1d) $ zip' (xs,ys))) where h (f, fs) = f e : fs

- zip' = anaList out
≡ zip' = inList . recList (anaList out) . out
≡ zip' ⋅ in = inList . recList (anaList out)

anaList ∷ (c → b ∐ (a × c)) → c → [a]
anaList g = inList . recList (anaList g) . g

calcLine' = zipWithM' linear1d
  ≡ {zipWithM' f = (sequenceA' .) . zipWith' f}
calcLine' = (sequenceA' .) . zipWith' linear1d
  ≡ {zipWith' f = (fmap f' .) . zip where f' = uncurry f}
calcLine' = (sequenceA' .) . ((fmap (uncurry linear1d) .) . zip)
```


```text
zipWithM f xs ys  =  sequenceA (zipWith f xs ys)
  traverse f = sequenceA ⋅ fmap f
  sequenceA = traverse id
  -- sequenceA = traverse id = sequenceA ⋅ (T id)
  -- sequenceA = sequenceA ⋅ (T id)
  -- ...
  -- ...
  -- sequenceA a = ⦇[nil, h]⦈ where h (f, fs) = f a : fs

```

```haskell {Working calcLine}
out ∷ ([a] , [b]) → Either () ((a × b) × ([a] × [b]))
out = \case
  ([],_)           → Left ()
  (_,[])           → Left ()
  ((a:as), (b:bs)) → Right ((a,b) , (as,bs))

zip' ∷ ([a] , [b]) → [(a, b)]
zip' = anaList out

zipWith' ∷ (a → b → c) → [a] → [b] → [c]
zipWith' op l r = fmap op' t where
  op' = uncurry op
  t = zip' (l,r)

zipWithM' ∷ (a  → b  → p  → d)  → [a]  → [b]  → p  → [d]
zipWithM' f xs ys = sequenceA' (zipWith' f xs ys)

sequenceA' ∷ [e → fe] → e → [fe]
sequenceA' fs e = [f e | f ← fs]

calcLine' :: [Rational] -> [Rational] -> Float -> [Rational]
calcLine' xs ys e = cataList (either nil h) ((fmap (uncurry linear1d) $ zip' (xs,ys))) where
    h (f, fs) = f e : fs
```