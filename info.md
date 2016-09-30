getPlantLinks con lim off = map plantPicturePicture_id <$> getPlantPictures con lim off
getPlantLinks con lim off = fmap (map plantPicturePicture_id) (getPlantPictures con lim off)
getPlantLinks con lim off = fmap (map plantPicturePicture_id) . (getPlantPictures con lim) $ off
getPlantLinks con lim = fmap (map plantPicturePicture_id) . (getPlantPictures con lim)
getPlantLinks con lim = (fmap (map plantPicturePicture_id) .) (getPlantPictures con lim)
getPlantLinks con lim = (fmap (map plantPicturePicture_id) .) . (getPlantPictures con) $ lim
getPlantLinks con = (fmap (map plantPicturePicture_id) .) . (getPlantPictures con)
getPlantLinks con = ((fmap (map plantPicturePicture_id) .) .) (getPlantPictures con)
getPlantLinks con = ((fmap (map plantPicturePicture_id) .) .) . getPlantPictures $ con
getPlantLinks = ((fmap (map plantPicturePicture_id) .) .) . getPlantPictures


<mniip> it's not "put inside"
<mniip> fmap :: (a -> b) -> (f a -> f b)
<mniip> we "lift" a -> b into f a -> f b
<mniip> liftA2 :: (a -> b -> c) -> (f a -> f b -> f c)
<mniip> ditto


<orion> Actually, what does foo (Foo f:fs) mean? Does fs get unwrapped in addition to f?
<lyxia> orion: coercion
<lyxia> orion: that's foo ((Foo f):fs)
<lyxia> only the head is unwrapped here
<orion> ah ok, thanks.
<orion> lyxia: So if I understand correctly, given newtype Foo = Foo [Text], mergeFoo :: [Foo] -> Foo can be defined as, "concat . coerce", correct?
<lyxia> coerce concat
<lyxia> or coerce . concat . coerce
<lyxia> with some type annotations I guess
<orion> hmm
<lyxia> Foo . concat . coerce
<lyxia> the one coerce you wrote is the most "useful" one
<lyxia> but you still need to convert back to a Foo
<orion> Doesn't typecheck for me. :(
<orion> Couldn't match representation of type ‘t0 [Text]’ with that of ‘[Foo]’ Relevant role signatures: type role [] representational
<lyxia> coerce (concat :: [[Text]] -> [Text])
<orion> I see.
<lyxia> because instance resolution doesn't do unification, something like that
<orion> Works, thanks!
<lyxia> yw
