{-# LANGUAGE PackageImports, OverloadedStrings #-}

module Network.Gitit.Mgnl (plugin) where

-- import Debug.Trace

import Data.Char (toUpper, toLower)
import Data.List (isPrefixOf, isInfixOf)
import Data.Maybe (catMaybes)
import System.FilePath (replaceFileName, replaceDirectory, addExtension, dropFileName, (</>), takeBaseName, dropExtension, addTrailingPathSeparator, isExtensionOf)
import Control.Monad (liftM, forM)
import Control.Monad.Catch (try, catch, SomeException)
import Data.FileStore (FileStoreError, retrieve, latest, index)
import Text.Pandoc (def, readMarkdown, runIO, ReaderOptions(..), Extension(..), getDefaultExtensions, extensionsFromList)
import Text.Pandoc.Walk (walk, walkM, query)
import Text.Pandoc.Writers.Shared (toTableOfContents)
import Network.Gitit.ContentTransformer (inlinesToString)
import Network.Gitit.Page (readKeywords)
import Network.Gitit.Interface
import Data.Text (pack, unpack, Text)
import Network.Http.Client
import qualified System.IO.Streams as Streams
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8 (pack, unpack)

replace :: Eq a => a -> a -> [a] -> [a]
replace a b = map $ \c -> if c == a then b else c

plugin :: Plugin
plugin = mkPageTransformM substituteIntoBlock

mydef :: ReaderOptions
mydef = def { readerExtensions = getDefaultExtensions "markdown"
                                 <> extensionsFromList [Ext_emoji]
                                 <> readerExtensions def }

-- NEXUSBASE = "https://nexus.magnolia-cms.com/service/local/"
-- 
-- nexusURL groupId artifactId version className = nexusrepobase </> nexusrepo </> "archive" </> explode groupId </> artifactId </> version </>
--                                                 artifactId ++ "-" ++ version ++ "-javadoc.jar/!" ++ explode className ++ ".html"
--   where
--     nexusrepobase = NEXUSBASE ++ "repositories"
--     nexusrepo = "magnolia.public.releases"
--     explode = replace '.' '/'

getURL' :: String -> IO BS.ByteString
getURL' url =
  let (host, req) = case url of
                      ('h':'t':'t':'p':':':'/':'/':s) -> break (=='/') s
                      ('h':'t':'t':'p':'s':':':'/':'/':s) -> break (=='/') s
                      s -> ("localhost", s)
  in getURL host req url

getURL :: String -> String -> String -> IO BS.ByteString
getURL host _ url = catch doStuff ((const $ return "") :: SomeException -> IO BS.ByteString)
  where
    doStuff = withConnection (establishConnection $ C8.pack url) $ \c -> do
      let q = buildRequest1 $ do
                http GET $ C8.pack url
                setHostname (C8.pack host) 443
                setAccept "*/*"
                -- setAuthorizationBasic "puppet" "g44maZtz"
      sendRequest c q emptyBody
      receiveResponse c wrapRedirect
    handler _ i = maybe "" id <$> Streams.read i
    wrapRedirect p i = do
      let s = getStatusCode p
      if (s == 301 || s == 302 || s == 303 || s == 307)
        then
          case getHeader p "Location" of
            Just l -> getURL' $ C8.unpack l
            Nothing -> handler p i
        else
          handler p i

-- getGAVfromClassName classname version = case (classname, version) of
--   ("", _) -> return Nothing
--   (_, "") ->
--   _       -> return $ Just (gav, classname, subclassname)
-- 
-- getJavadocURLfromClassName classname version = gav
--   where
--     gav <- getGAVfromClassName classname version
--     case gav of ->
--       Just (gav, classname, subclassname) ->
--       Nothing ->

-- def getJavadocURLfromClassName(classname, version):
--   gav, classname, subclassname = getGAVfromClassName(classname, version)
--   if gav is not None:
--     doc = ElementTree.XML(getURL(NEXUSURL + "data_index?c=javadoc&g=%(g)s&a=%(a)s&v=%(v)s" % gav).read())
--     for a in ElementPath.findall(doc, ".//artifact"):
--       return a.find("resourceURI").text, classname, subclassname
--   return None, None, None

-- def getGAV(a):
--   return { 'g': a.find("groupId").text,
--            'a': a.find("artifactId").text,
--            'v': a.find("version").text }
-- 
-- def getGAVfromClassName(classname, version = None):
--   if classname is None or classname == '':
--     return None
--   classname = classname.split('.')
--   subclassname = []
--   while classname[-1][0].isupper():
--     subclassname.insert(0,classname.pop())
--   classname=".".join(classname) + "." + subclassname.pop(0)
--   subclassname="".join([".%s" % x for x in subclassname])
-- 
--   doc = ElementTree.XML(getURL(NEXUSURL + 'lucene/search?cn=' + classname).read())
--   if version is None:
--     version = []
--     latestGAV = None
--     for a in ElementPath.findall(doc, ".//artifact"):
--       try:
--         myversion = map(int, a.find("version").text.split("."))
--         if myversion > version:
--           version = myversion
--           latestGAV = getGAV(a)
--       except: pass
--     return latestGAV, classname, subclassname
--   else:
--     for a in ElementPath.findall(doc, ".//artifact"):
--       if a.find("version").text == version:
--         return getGAV(a), classname, subclassname
--   return None, None, None

substituteIntoBlock :: [Block] -> PluginM [Block]
substituteIntoBlock (Para ts@(RawInline _ _:_):xs) = (mapToBlock ts ++) <$> substituteIntoBlock xs -- remove unwanted <p> tags around raw inline elements
  where
    mapToBlock [] = []
    mapToBlock ((RawInline f t'):ts') = RawBlock f t' : mapToBlock ts'
    mapToBlock (t':ts') = let ts'' = mapToBlock ts'
                          in  case ts'' of
                                (Plain ts''':rest) -> Plain (t':ts''') : rest
                                _                  -> Plain [t'] : ts''

substituteIntoBlock (Para  ys:xs) = sub' Para  ys xs
substituteIntoBlock (Plain ys:xs) = sub' Plain ys xs
substituteIntoBlock (x:xs) = (x:) <$> substituteIntoBlock xs -- not special, just pass on unchanged
substituteIntoBlock [] = return [] -- base case

sub' :: ([Inline] -> Block) -> [Inline] -> [Block] -> PluginM [Block]
sub' t ys xs = do
  ys' <- mapM (sub'' xs) ys
  xs' <- substituteIntoBlock xs
  return $ joinElems ys' ++ xs'
  where
    joinElems (Right bs1:Right bs2:es) = joinElems (Right (bs1 ++ bs2):es) -- combines inlines and blocks appropriately
    joinElems (Left  is1:Left  is2:es) = joinElems (Left  (is1 ++ is2):es)
    joinElems (Left is:es) = joinElems (Right [t is]:es)
    joinElems (Right bs:es) = bs ++ joinElems es
    joinElems [] = []

sub'' :: [Block] -> Inline -> PluginM (Either [Inline] [Block])
sub'' xs l@(Link attr ref (target, _)) = do
  r <- linkToBlocks attr ref target xs
  return $ case r of
    Nothing         -> Left [l]
    Just [Para  r'] -> Left r'
    Just [Plain r'] -> Left r'
    Just r'         -> Right r'
sub'' _ a = return $ Left [a]

linkToBlocks :: Attr -> [Inline] -> Text -> [Block] -> PluginM (Maybe [Block])
linkToBlocks _ _ "!toc" xs = return $ Just $ case toTableOfContents def xs of
  BulletList [] -> []
  toc           -> [Div ("TOC", [], []) [toc]] -- handle toc

linkToBlocks attr ref "!include-raw" _ = do -- handle include-raws
  let target = inlinesToString ref
  article <- liftIO $ C8.unpack <$> getURL' target
  return $ Just [CodeBlock attr $ pack article]

linkToBlocks attr ref "!include" _ = do -- handle includes
  let target = inlinesToString ref
  target' <- getRealTarget target
  fs <- askFileStore
  article <- try $ do
    i <- liftIO $ index fs
    let target'' = if target' `elem` i then target' else findci target' i
    revid <- liftIO $ latest fs target''
    liftIO $ retrieve fs target'' $ Just revid
  case article :: Either FileStoreError String of
    Left  e    -> let txt = Str $ pack $ "[" ++ target ++ "](!include)"
                      alt = pack $ "'" ++ target ++ "' doesn't exist (" ++ show e ++ "). Click here to create it."
                  in  return $ Just [Para [Link attr [txt] (pack target, alt)]]
    Right a    -> do
      p <- liftIO $ runIO $ readMarkdown mydef $ pack a
      case p of
        Left _ -> return $ Just [Para $ [Str "Error parsing markdown in include!"]]
        -- Right (Pandoc _ content) -> Just . removeOuterPTags . updateLinks (dropFileName target') <$> walkM substituteIntoBlock content
        Right (Pandoc _ content) -> Just . updateLinks (dropFileName target') <$> walkM substituteIntoBlock content
  where
    findci x [] = x
    findci x (y:ys) = if map toLower x == map toLower y then y else findci x ys
    getRealTarget ('/':s) = return $ addExtension (replace '+' ' ' s) "page"
    getRealTarget s = do
      (Context fname _ _ _ _ _ _) <- getContext
      return $ replace '+' ' ' $ replaceFileName fname $ addExtension s "page"
    -- removeOuterPTags [Para xs] = [Plain xs]
    -- removeOuterPTags [Para xs, RawBlock r s] = [Plain xs, RawBlock r s]
    -- removeOuterPTags x = x
    -- FIXME: sometimes we want to remove the outer p tags, sometimes we don't,
    --   probablhy depending on the parent tag

linkToBlocks attr ref "!excerpt-include" _ = do -- handle excerpt-includes
  let (target, anchor) = break (=='#') $ inlinesToString ref
  target' <- getRealTarget target
  fs <- askFileStore
  article <- try $ do
    revid <- liftIO $ latest fs target'
    liftIO $ retrieve fs target' $ Just revid
  case article :: Either FileStoreError String of
    Left  e    -> let txt = Str $ pack $ "[" ++ target ++ "](!excerpt-include)"
                      alt = pack $ "'" ++ target ++ "' doesn't exist (" ++ show e ++ "). Click here to create it."
                  in  return $ Just [Para [Link attr [txt] (pack target, alt)]]
    Right a    -> do
      p <- liftIO $ runIO $ readMarkdown mydef $ pack a
      case p of
        Left _ -> return $ Just [Para $ [Str "Error parsing markdown in include!"]]
        Right (Pandoc _ content) -> Just <$> walkM substituteIntoBlock (updateLinks (dropFileName target') $ query (findExcerpt anchor) content)
  where
    getRealTarget ('/':s) = return $ addExtension (replace '+' ' ' s) "page"
    getRealTarget s = do
      (Context fname _ _ _ _ _ _) <- getContext
      return $ replace '+' ' ' $ replaceFileName fname $ addExtension s "page"
    findExcerpt "" (Div ("", cs, []) bs) | "excerpt" `elem` cs = bs
    findExcerpt name (Div (name', cs, []) bs) | "excerpt" `elem` cs && name == ('#':unpack name') = bs
    findExcerpt _ _ = []

linkToBlocks _ ref "!children" _ = do -- handle children list
  let target = inlinesToString ref
  target' <- dropExtension <$> getRealTarget target
  fs <- askFileStore
  i <- filter (".page" `isExtensionOf`) <$> (liftIO $ index fs)
  return $ Just [Div ("", ["children"], []) (showChildren target' i)]
  where
    getRealTarget "" = ctxFile <$> getContext
    getRealTarget ('/':s) = return $ replace '+' ' ' s
    getRealTarget s = do
      fname <- ctxFile <$> getContext
      return $ replace '+' ' ' $ replaceFileName fname s
    showChildren path i' = let i'' = filter (not . ("/" `isInfixOf`) . (drop $ 1 + length path)) $ filter (addTrailingPathSeparator path `isPrefixOf`) i'
                           in  case i'' of
                                 [] -> []
                                 _  -> [BulletList $ map (showChildren' path) i'']
    showChildren' target path = let path' = takeBaseName path in [Plain [Link ("", [], []) [Str $ pack path'] (pack $ replace ' ' '+' $ '/':target </> path', "")]]

linkToBlocks _ ref "!youtube" _ = do -- handle youtube embedded iframe
  let target = inlinesToString ref
  let (videoId, rest) = span (/=' ') (dropWhile (==' ') target)
  return $ Just [RawBlock (Format "html") $ pack $ "<iframe src=\"https://www.youtube.com/embed/" ++ videoId ++ "?rel=0\"" ++ rest ++ " frameborder=\"0\" allowfullscreen=\"true\"></iframe>"]

linkToBlocks _ ref "!label" _ = do -- handle page list by label
  cfg <- askConfig
  let repoPath = repositoryPath cfg
  let target = words $ inlinesToString ref
  fs <- askFileStore
  i <- filter (".page" `isExtensionOf`) <$> (liftIO $ index fs)

  matches <- liftM catMaybes $
             forM i $ \f -> do
               (mtitle, keys) <- liftIO $ readKeywords $ repoPath </> f
               return $ if all (`elem` keys) target then Just (mtitle, f) else Nothing

  return $ case matches of
    [] -> Just []
    _  -> Just [BulletList $ map listItem matches]
  where
    listItem (Nothing, s) = listItem (Just $ takeBaseName s, s)
    listItem (Just t,  s) = [Plain [Link ("", [], []) [Str $ pack t] (pack $ replace ' ' '+' $ '/':dropExtension s, "")]]

linkToBlocks _ ref "!jira" _ = do -- handle jira link -- TODO - grab iconUrl for type to put at start, put status in a span and add some css
  let target = inlinesToString ref
  let req = "/sr/jira.issueviews:searchrequest-xml/temp/SearchRequest.xml?jql=" ++ encodeQuery target ++ "&field=type&field=summary&field=status&field=resolution&field=statuscategory&field=link&tempMax=20&returnMax=true"
  let host = "jira.magnolia-cms.com"
  xml <- liftIO $ C8.unpack <$> getURL host req ("https://" ++ host ++ "/" ++ req)
  return $ Just [Para $ map (mkLink . unlines) $ getItems $ lines xml]
    where
      getItems xs = case dropWhile (not . ("<item>" `isInfixOf`)) xs of
                      [] -> []
                      xs' -> let (ys', xs'') = span (not . ("</item>" `isInfixOf`)) (tail xs')
                             in  ys':getItems xs''
      findTag t ('<':xs) = if t `isPrefixOf` xs then findTag' t (tail $ dropWhile (not . (=='>')) xs) else findTag t xs
      findTag t (_:xs)   = findTag t xs
      findTag _ _ = ""
      findTag' t ('<':'/':xs) = if (t ++ ">") `isPrefixOf` xs then "" else '<':'/':findTag' t xs
      findTag' t (x:xs) = x:findTag' t xs
      findTag' _ _ = ""
      findAttr t a ('<':xs) = if t `isPrefixOf` xs then findAttr' a (tail $ dropWhile (not . (==' ')) xs) else findAttr t a xs
      findAttr t a (_:xs)   = findAttr t a xs
      findAttr _ _ _        = ""
      findAttr' a (x:xs)    = if (a ++ "=\"") `isPrefixOf` (x:xs) then takeWhile (not . (=='"')) $ (tail $ dropWhile (not . (=='"')) xs) else findAttr' a xs
      findAttr' _ _         = ""
      mkLink s = let typ = pack $ findTag "type" s
                     typIconUrl = pack $ decodeAmp $ findAttr "type" "iconUrl" s
                     key = pack $ findTag "key" s
                     link = pack $ findTag "link" s
                     summary = pack $ ' ':findTag "summary" s ++ " "
                     status = findTag "status" s
                     statusl = pack $ map toLower status
                     statusu = pack $ map toUpper status
                     statusColour = pack $ findAttr "statusCategory" "colorName" s
                 in  Span ("", ["jira"], []) [Link ("", [], []) [Image ("", ["inline"], []) [] (typIconUrl, typ), Str key] (link, ""), RawInline "html" $ summary, Span ("", ["status", statusColour, statusl], []) [Str statusu]]
      decodeAmp ('&':'a':'m':'p':';':xs) = '&':decodeAmp xs
      decodeAmp (x:xs) = x:decodeAmp xs
      decodeAmp "" = ""
      encodeQuery (' ':xs) = "%20" ++ encodeQuery xs
      encodeQuery ('.':xs) = "%2E" ++ encodeQuery xs
      encodeQuery ('=':xs) = "%3D" ++ encodeQuery xs
      encodeQuery (x:xs) = x:encodeQuery xs
      encodeQuery "" = ""

-- ["<type id=\"4\" iconUrl=\"https://jira.magnolia-cms.com/secure/viewavatar?size=xsmall&amp;avatarId=10890&amp;avatarType=issuetype\">Improvement</type>\n                       
--   <status id=\"6\" iconUrl=\"https://jira.magnolia-cms.com/images/icons/statuses/closed.png\" description=\"The issue is considered finished, the resolution is correct. Issues which are not closed can be reopened.\">Closed</status>
--   <statusCategory id=\"3\" key=\"done\" colorName=\"green\"/>
--   <resolution id=\"1\">Fixed</resolution>\n"]

linkToBlocks _ _ "!javadoc" _ = do -- handle javadoc link
  return $ Just [Para [Link ("", [], []) [Str "classname"] ("https://nexus.magnolia-cms.com/", "")]] -- TODO

linkToBlocks _ _ _ _ = return Nothing

-- URLs in links and images inside included blocks need to be normalised back to the include page
-- e.g. a link to example.png on include /_i/example.page should be found at /_i/example.png
updateLinks :: String -> [Block] -> [Block]
updateLinks newdir = walk updateLinks'
  where
    updateLinks' :: Inline -> Inline
    updateLinks' (Link a es (u, t)) = Link a es (updateUrl' u, t)
    updateLinks' (Image a es (u, t)) = Image a es (updateUrl' u, t)
    updateLinks' i = i
    updateUrl' = pack . updateUrl . unpack
    updateUrl s@('h':'t':'t':'p':':':_) = s
    updateUrl s@('h':'t':'t':'p':'s':':':_) = s
    updateUrl s@('f':'t':'p':':':_) = s
    updateUrl s@('/':_) = s
    updateUrl s@('#':_) = s
    updateUrl s = replaceDirectory s ('/':replace ' ' '+' newdir)
