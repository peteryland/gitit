{-# LANGUAGE ScopedTypeVariables, DeriveGeneric #-}
{-
Copyright (C) 2008-9 John MacFarlane <jgm@berkeley.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- Handlers for wiki functions.
-}

module Network.Gitit.Handlers (
                        handleAny
                      , debugHandler
                      , randomPage
--                       , discussPage
--                       , createPage
--                       , showActivity
                      , goToPage
                      , searchResults
                      , notFoundPage
--                       , uploadForm
--                       , uploadFile
                      , indexPage
                      , categoryPage
                      , categoryListPage
                      , preview
                      , showRawPage
                      , showFileAsText
                      , showPageHistory
                      , showFileHistory
                      , showPage
                      , showPageDiff
                      , showFileDiff
                      , exportPage
--                       , updatePage
--                       , editPage
--                       , deletePage
--                       , confirmDelete
                      , showHighlightedSource
                      , expireCache
--                       , feedHandler
                      , contentsPage
                      )
where
import Safe
import GHC.Generics (Generic)
import Network.Gitit.Server
import Network.Gitit.Framework
import Network.Gitit.Layout
import Network.Gitit.Types
-- import Network.Gitit.Feed (filestoreToXmlFeed, FeedConfig(..))
import Network.Gitit.Util (orIfNull)
import Network.Gitit.Cache (expireCachedFile, lookupCache, cacheContents)
import Network.Gitit.ContentTransformer (showRawPage, showFileAsText, showPage,
        exportPage, showHighlightedSource, preview) -- , applyPreCommitPlugins)
import Network.Gitit.Page (readCategories, stringToPage)
import qualified Control.Exception as E
import System.FilePath
import Network.Gitit.State
import Text.XHtml hiding ( (</>), dir, method, password, rev )
-- import qualified Text.XHtml as X ( method )
import Data.List (intercalate, delete, nub, sortBy, find, isPrefixOf, isInfixOf, isSuffixOf, inits, sort, (\\), partition) -- intersperse,
import Data.List.Split (wordsBy)
import Data.Maybe (fromMaybe, mapMaybe, catMaybes) -- isJust,
import Data.Ord (comparing)
import Data.Char (toLower) --, isSpace)
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString as S
import Network.HTTP (urlEncodeVars)
import Data.Time (getCurrentTime) --, addUTCTime)
import Data.Time.Clock (UTCTime(..))
import Data.FileStore
import System.Log.Logger (logM, Priority(..))
import qualified Data.Aeson as J

isValidPath :: String -> Bool
isValidPath "index" = False
isValidPath ('_':_) = False
isValidPath _       = True

handleAny :: Handler
handleAny = withData $ \(params :: Params) -> uriRest $ \uri ->
  let path' = uriPath uri
  in  do fs <- getFileStore
         let rev = pRevision params
         mimetype <- getMimeTypeForExtension
                      (takeExtension path')
         res <- liftIO $ E.try
                (retrieve fs path' rev :: IO B.ByteString)
         case res of
                Right contents -> ignoreFilters >>  -- don't compress
                                  (ok $ setContentType mimetype $
                                    (toResponse noHtml) {rsBody = contents})
                                    -- ugly hack
                Left NotFound  -> mzero
                Left e         -> error (show e)

debugHandler :: Handler
debugHandler = withData $ \(params :: Params) -> do
  req <- askRq
  liftIO $ logM "gitit" DEBUG (show req)
  page <- getPage
  liftIO $ logM "gitit" DEBUG $ "Page = '" ++ page ++ "'\n" ++
              show params
  mzero

randomPage :: Handler
randomPage = do
  fs <- getFileStore
  base' <- getWikiBase
  prunedFiles <- liftIO (index fs) >>= filterM isPageFile >>= filterM isNotDiscussPageFile
  let pages = map dropExtension prunedFiles
  if null pages
     then error "No pages found!"
     else do
       secs <- liftIO (fmap utctDayTime getCurrentTime)
       let newPage = pages !!
                     (truncate (secs * 1000000) `mod` length pages)
       seeOther (base' ++ urlForPage newPage) $ toResponse $
         p << "Redirecting to a random page"

{-
discussPage :: Handler
discussPage = do
  page <- getPage
  base' <- getWikiBase
  seeOther (base' ++ urlForPage (if isDiscussPage page then page else ('@':page))) $
                     toResponse "Redirecting to discussion page"

createPage :: Handler
createPage = do
  page <- getPage
  base' <- getWikiBase
  case page of
       ('_':_) -> mzero   -- don't allow creation of _index, etc.
       _       -> formattedPage defaultPageLayout{
                                      pgPageName = page
                                    , pgTabs = []
                                    , pgTitle = "Create " ++ page ++ "?"
                                    } $
                    (p << stringToHtml
                        ("There is no page named '" ++ page ++ "'. You can:"))
                        +++
                    (unordList $
                      [ anchor !
                            [href $ base' ++ "/_edit" ++ urlForPage page] <<
                              ("Create the page '" ++ page ++ "'")
                      , anchor !
                            [href $ base' ++ "/_search?" ++
                                (urlEncodeVars [("q", page)])] <<
                              ("Search for pages containing the text '" ++
                                page ++ "'")])

uploadForm :: Handler
uploadForm = withData $ \(params :: Params) -> do
  let origPath = pFilename params
  let wikiname = pWikiname params `orIfNull` takeFileName origPath
  let logMsg = pLogMsg params
  let upForm = form ! [X.method "post", enctype "multipart/form-data"] <<
       fieldset <<
       [ p << [label ! [thefor "file"] << "File to upload:"
              , br
              , afile "file" ! [value origPath] ]
       , p << [ label ! [thefor "wikiname"] << "Name on wiki, including extension"
              , noscript << " (leave blank to use the same filename)"
              , stringToHtml ":"
              , br
              , textfield "wikiname" ! [value wikiname]
              , primHtmlChar "nbsp"
              , checkbox "overwrite" "yes"
              , label ! [thefor "overwrite"] << "Overwrite existing file" ]
       , p << [ label ! [thefor "logMsg"] << "Description of content or changes:"
              , br
              , textfield "logMsg" ! [size "60", value logMsg]
              , submit "upload" "Upload" ]
       ]
  formattedPage defaultPageLayout{
                   pgMessages = pMessages params,
                   pgScripts = ["uploadForm.js"],
                   pgShowPageTools = False,
                   pgTabs = [],
                   pgTitle = "Upload a file"} upForm

uploadFile :: Handler
uploadFile = withData $ \(params :: Params) -> do
  let origPath = pFilename params
  let filePath = pFilePath params
  let wikiname = normalise
                 $ dropWhile (=='/')
                 $ pWikiname params `orIfNull` takeFileName origPath
  let logMsg = pLogMsg params
  cfg <- getConfig
  wPF <- isPageFile wikiname
  mbUser <- getLoggedInUser
  (user, email) <- case mbUser of
                        Nothing -> return ("Anonymous", "")
                        Just u  -> return (uUsername u, uEmail u)
  let overwrite = pOverwrite params
  fs <- getFileStore
  exists <- liftIO $ E.catch (latest fs wikiname >> return True) $ \e ->
                      if e == NotFound
                         then return False
                         else E.throwIO e >> return True
  let inStaticDir = staticDir cfg `isPrefixOf` (repositoryPath cfg </> wikiname)
  let inTemplatesDir = templatesDir cfg `isPrefixOf` (repositoryPath cfg </> wikiname)
  let dirs' = splitDirectories $ takeDirectory wikiname
  let imageExtensions = [".png", ".jpg", ".gif"]
  let errors = validate
                 [ (null . filter (not . isSpace) $ logMsg,
                    "Description cannot be empty.")
                 , (".." `elem` dirs', "Wikiname cannot contain '..'")
                 , (null origPath, "File not found.")
                 , (inStaticDir,  "Destination is inside static directory.")
                 , (inTemplatesDir,  "Destination is inside templates directory.")
                 , (not overwrite && exists, "A file named '" ++ wikiname ++
                    "' already exists in the repository: choose a new name " ++
                    "or check the box to overwrite the existing file.")
                 , (wPF,
                    "This file extension is reserved for wiki pages.")
                 ]
  if null errors
     then do
       expireCachedFile wikiname `mplus` return ()
       fileContents <- liftIO $ B.readFile filePath
       let len = B.length fileContents
       liftIO $ save fs wikiname (Author user email) logMsg fileContents
       let contents = thediv <<
             [ h2 << ("Uploaded " ++ show len ++ " bytes")
             , if takeExtension wikiname `elem` imageExtensions
                  then p << "To add this image to a page, use:" +++
                       pre << ("![alt text](/" ++ wikiname ++ ")")
                  else p << "To link to this resource from a page, use:" +++
                       pre << ("[link label](/" ++ wikiname ++ ")") ]
       formattedPage defaultPageLayout{
                       pgMessages = pMessages params,
                       pgShowPageTools = False,
                       pgTabs = [],
                       pgTitle = "Upload successful"}
                     contents
     else withMessages errors uploadForm
-}

goToPage :: Handler
goToPage = withData $ \(params :: Params) -> do
  let gotopage = pGotoPage params
  fs <- getFileStore
  pruned_files <- liftIO (index fs) >>= filterM isPageFile
  let allPageNames = map dropExtension pruned_files
  let findPage f = find f allPageNames
  let exactMatch f = gotopage == f
  let insensitiveMatch f = (map toLower gotopage) == (map toLower f)
  let prefixMatch f = (map toLower gotopage) `isPrefixOf` (map toLower f)
  base' <- getWikiBase
  case findPage exactMatch of
       Just m  -> seeOther (base' ++ urlForPage m) $ toResponse
                     "Redirecting to exact match"
       Nothing -> case findPage insensitiveMatch of
                       Just m  -> seeOther (base' ++ urlForPage m) $ toResponse
                                    "Redirecting to case-insensitive match"
                       Nothing -> case findPage prefixMatch of
                                       Just m  -> seeOther (base' ++ urlForPage m) $
                                                  toResponse $ "Redirecting" ++
                                                    " to partial match"
                                       Nothing -> searchResults

searchResults :: Handler
searchResults = searchResults' "Search results"

notFoundPage :: Handler
notFoundPage = searchResults' "Page Not Found"

searchResults' :: String -> Handler
searchResults' myTitle = withData $ \(params :: Params) -> do
  let patterns = pPatterns params `orIfNull` [pGotoPage params]
  fs <- getFileStore
  matchLines <- if null patterns
                   then return []
                   else liftIO $ E.catch (search fs SearchQuery{
                                                  queryPatterns = patterns
                                                , queryWholeWords = False
                                                , queryMatchAll = True
                                                , queryIgnoreCase = True })
                                       -- catch error, because newer versions of git
                                       -- return 1 on no match, and filestore <=0.3.3
                                       -- doesn't handle this properly:
                                       (\(_ :: FileStoreError)  -> return [])
  let contentMatches = filter isValidPath $ map matchResourceName matchLines
  allPages <- filter isValidPath <$> liftIO (index fs) >>= filterM isPageFile
  let slashToSpace = map (\x -> if x == '/' then ' ' else x)
  let inPageName pageName' x = x `elem` (words $ slashToSpace $ dropExtension pageName')
  let matchesPatterns pageName' = not (null patterns) &&
       all (inPageName (map toLower pageName')) (map (map toLower) patterns)
  let pageNameMatches = filter matchesPatterns allPages
  let partialMatchesPatterns pageName' = not (null patterns) &&
       any (inPageName (map toLower pageName')) (map (map toLower) patterns)
  let partialPageNameMatches = filter partialMatchesPatterns allPages
  prunedFiles <- filterM isPageFile (pageNameMatches ++ contentMatches ++ partialPageNameMatches)
  let allMatchedFiles = nub $ prunedFiles
  let matchesInFile f =  mapMaybe (\x -> if matchResourceName x == f
                                            then Just (matchLine x)
                                            else Nothing) matchLines
  let matches = map (\f -> (f, matchesInFile f)) allMatchedFiles
  let relevance (f, ms) = length ms + (if f `elem` partialPageNameMatches then 100 else 0)
                                    + (if f `elem` pageNameMatches then 1000 else 0)
                                    + (if map toLower (unwords patterns) `isInfixOf` map toLower f then 10000 else 0)
                                    + (if ('/':map toLower (unwords patterns) ++ "/") `isInfixOf` map toLower f then 20000 else 0)
                                    + (if ('/':map toLower (unwords patterns)) `isSuffixOf` map toLower (dropExtension f) then 1000000 else 0)
  let preamble = if null patterns
                    then h3 << ["Please enter a search term."]
                    else h3 << [ stringToHtml "Showing top matches for "
                               , thespan ! [identifier "q"] << unwords patterns]
  base' <- getWikiBase
  let prettyFile f = case f of
                       ""       -> ""
                       ('/':xs) -> " » " ++ prettyFile xs
                       (x:xs)   -> x : prettyFile xs
  let toMatchListItem (file, _) = li <<
        [ anchor ! [href $ base' ++ urlForPage (dropExtension file)] << prettyFile (dropExtension file) ]
--         , stringToHtml (" (" ++ show (length contents) ++ " matching lines)")
--         , stringToHtml " "
--         , anchor ! [href "#", theclass "showmatch",
--                     thestyle "display: none;"] << if length contents > 0
--                                                      then "[show matches]"
--                                                      else ""
--         , pre ! [theclass "matches"] << unlines contents]
  let htmlMatches = preamble +++
                    ulist ! [theclass "searchresults"] << take 100 (map toMatchListItem $ reverse $ sortBy (comparing relevance) matches)
  formattedPage defaultPageLayout{
                  pgMessages = pMessages params,
                  pgShowPageTools = False,
                  pgTabs = [],
                  pgScripts = ["search.js"],
                  pgTitle = myTitle}
                htmlMatches

showPageHistory :: Handler
showPageHistory = withData $ \(params :: Params) -> do
  page <- getPage
  cfg <- getConfig
  showHistory (pathForPage page $ defaultExtension cfg) page params

showFileHistory :: Handler
showFileHistory = withData $ \(params :: Params) -> do
  file <- getPage
  showHistory file file params

showHistory :: String -> String -> Params -> Handler
showHistory file page params =  do
  fs <- getFileStore
  hist <- liftIO $ history fs [file] (TimeRange Nothing Nothing)
            (Just $ pLimit params)
  base' <- getWikiBase
  let versionToHtml rev pos = li ! [theclass "difflink", intAttr "order" pos,
                                    strAttr "revision" (revId rev),
                                    strAttr "diffurl" (base' ++ "/_diff/" ++ page)] <<
        [ thespan ! [theclass "date"] << (show $ revDateTime rev)
        , stringToHtml " ("
        , thespan ! [theclass "author"] << anchor ! [href $ base' ++ "/_activity?" ++
            urlEncodeVars [("forUser", authorName $ revAuthor rev)]] <<
              (authorName $ revAuthor rev)
        , stringToHtml "): "
        , anchor ! [href (base' ++ urlForPage page ++ "?revision=" ++ revId rev)] <<
           thespan ! [theclass "subject"] <<  revDescription rev
        , noscript <<
            ([ stringToHtml " [compare with "
             , anchor ! [href $ base' ++ "/_diff" ++ urlForPage page ++ "?to=" ++ revId rev] <<
                  "previous" ] ++
             (if pos /= 1
                  then [ primHtmlChar "nbsp"
                       , primHtmlChar "bull"
                       , primHtmlChar "nbsp"
                       , anchor ! [href $ base' ++ "/_diff" ++ urlForPage page ++ "?from=" ++
                                  revId rev] << "current"
                       ]
                  else []) ++
             [stringToHtml "]"])
        ]
  let contents = if null hist
                    then noHtml
                    else ulist ! [theclass "history"] <<
                           zipWith versionToHtml hist
                           [length hist, (length hist - 1)..1]
  let more = if length hist == pLimit params
                then anchor ! [href $ base' ++ "/_history" ++ urlForPage page
                                 ++ "?limit=" ++ show (pLimit params + 100)] <<
                                 "Show more..."
                else noHtml
  let tabs = if file == page  -- source file, not wiki page
                then [ViewTab,HistoryTab]
                else pgTabs defaultPageLayout
  formattedPage defaultPageLayout{
                   pgPageName = page,
                   pgMessages = pMessages params,
                   pgScripts = ["dragdiff.js"],
                   pgTabs = tabs,
                   pgSelectedTab = HistoryTab,
                   pgTitle = ("Changes to " ++ page)
                   } $ contents +++ more

{-
showActivity :: Handler
showActivity = withData $ \(params :: Params) -> do
  cfg <- getConfig
  currTime <- liftIO getCurrentTime
  let defaultDaysAgo = fromIntegral (recentActivityDays cfg)
  let daysAgo = addUTCTime (defaultDaysAgo * (-60) * 60 * 24) currTime
  let since = case pSince params of
                   Nothing -> Just daysAgo
                   Just x  -> Just x
  let forUser = pForUser params
  fs <- getFileStore
  hist <- liftIO $ history fs [] (TimeRange since Nothing)
                     (Just $ pLimit params)
  let hist' = case forUser of
                   Nothing -> hist
                   Just u  -> filter (\r -> authorName (revAuthor r) == u) hist
  let fileFromChange (Added f)    = f
      fileFromChange (Modified f) = f
      fileFromChange (Deleted f)  = f
  base' <- getWikiBase
  let fileAnchor revis file = if takeExtension file == "." ++ (defaultExtension cfg)
        then anchor ! [href $ base' ++ "/_diff" ++ urlForPage (dropExtension file) ++ "?to=" ++ revis] << dropExtension file
        else anchor ! [href $ base' ++ urlForPage file ++ "?revision=" ++ revis] << file
  let filesFor changes revis = intersperse (stringToHtml " ") $
        map (fileAnchor revis . fileFromChange) changes
  let heading = h1 << ("Recent changes by " ++ fromMaybe "all users" forUser)
  let revToListItem rev = li <<
        [ thespan ! [theclass "date"] << (show $ revDateTime rev)
        , stringToHtml " ("
        , thespan ! [theclass "author"] <<
            anchor ! [href $ base' ++ "/_activity?" ++
              urlEncodeVars [("forUser", authorName $ revAuthor rev)]] <<
                (authorName $ revAuthor rev)
        , stringToHtml "): "
        , thespan ! [theclass "subject"] << revDescription rev
        , stringToHtml " ("
        , thespan ! [theclass "files"] << filesFor (revChanges rev) (revId rev)
        , stringToHtml ")"
        ]
  let contents = ulist ! [theclass "history"] << map revToListItem hist'
  formattedPage defaultPageLayout{
                  pgMessages = pMessages params,
                  pgShowPageTools = False,
                  pgTabs = [],
                  pgTitle = "Recent changes"
                  } (heading +++ contents)
-}

showPageDiff :: Handler
showPageDiff = withData $ \(params :: Params) -> do
  page <- getPage
  cfg <- getConfig
  showDiff (pathForPage page $ defaultExtension cfg) page params

showFileDiff :: Handler
showFileDiff = withData $ \(params :: Params) -> do
  page <- getPage
  showDiff page page params

showDiff :: String -> String -> Params -> Handler
showDiff file page params = do
  let from = pFrom params
  let to = pTo params
  -- 'to' or 'from' must be given
  when (from == Nothing && to == Nothing) mzero
  fs <- getFileStore
  -- if 'to' is not specified, defaults to current revision
  -- if 'from' is not specified, defaults to revision immediately before 'to'
  from' <- case (from, to) of
              (Just _, _)        -> return from
              (Nothing, Nothing) -> return from
              (Nothing, Just x)  -> do
                pageHist <- liftIO $ history fs [file]
                                     (TimeRange Nothing Nothing)
                                     Nothing
                let (_, upto) = break (\r -> idsMatch fs (revId r) x)
                                  pageHist
                return $ if length upto >= 2
                            -- immediately preceding revision
                            then Just $ revId $ upto !! 1
                            else Nothing
  result' <- liftIO $ E.try $ getDiff fs file from' to
  case result' of
       Left NotFound  -> mzero
       Left e         -> liftIO $ E.throwIO e
       Right htmlDiff -> formattedPage defaultPageLayout{
                                          pgPageName = page,
                                          pgRevision = from' `mplus` to,
                                          pgMessages = pMessages params,
                                          pgTabs = DiffTab :
                                                   pgTabs defaultPageLayout,
                                          pgSelectedTab = DiffTab,
                                          pgTitle = page
                                          }
                                       htmlDiff

getDiff :: FileStore -> FilePath -> Maybe RevisionId -> Maybe RevisionId
        -> IO Html
getDiff fs file from to = do
  rawDiff <- diff fs file from to
  let diffLineToHtml (Both xs _) = thespan << unlines xs
      diffLineToHtml (First xs) = thespan ! [theclass "deleted"] << unlines xs
      diffLineToHtml (Second xs) = thespan ! [theclass "added"]  << unlines xs
  return $ h2 ! [theclass "revision"] <<
             ("Changes from " ++ fromMaybe "beginning" from ++
              " to " ++ fromMaybe "current" to) +++
           pre ! [theclass "diff"] << map diffLineToHtml rawDiff

{-
editPage :: Handler
editPage = withData editPage'

editPage' :: Params -> Handler
editPage' params = do
  let rev = pRevision params  -- if this is set, we're doing a revert
  fs <- getFileStore
  page <- getPage
  cfg <- getConfig
  let getRevisionAndText = E.catch
        (do x <- liftIO $ retrieve fs (pathForPage page $ defaultExtension cfg) rev
            -- even if pRevision is set, we return revId of latest
            -- saved version (because we're doing a revert and
            -- we don't want gitit to merge the changes with the
            -- latest version)
            r <- liftIO $ latest fs (pathForPage page $ defaultExtension cfg) >>= revision fs
            return (Just $ revId r, x))
        (\e -> if e == NotFound
                  then return (Nothing, "")
                  else E.throwIO e)
  (mbRev, raw) <- case pEditedText params of
                         Nothing -> liftIO getRevisionAndText
                         Just x  -> let r = if null (pSHA1 params)
                                               then Nothing
                                               else Just (pSHA1 params)
                                    in return (r, x)
  let messages = pMessages params
  let logMsg = pLogMsg params
  let sha1Box = case mbRev of
                 Just r  -> textfield "sha1" ! [thestyle "display: none",
                                                value r]
                 Nothing -> noHtml
  let readonly = if isJust (pRevision params)
                    -- disable editing of text box if it's a revert
                    then [strAttr "readonly" "yes",
                          strAttr "style" "color: gray"]
                    else []
  base' <- getWikiBase
  let editForm = gui (base' ++ urlForPage page) ! [identifier "editform"] <<
                   [ sha1Box
                   , textarea ! (readonly ++ [cols "80", name "editedText",
                                  identifier "editedText"]) << raw
                   , br
                   , label ! [thefor "logMsg"] << "Description of changes:"
                   , br
                   , textfield "logMsg" ! (readonly ++ [value (logMsg `orIfNull` defaultSummary cfg) ])
                   , submit "update" "Save"
                   , primHtmlChar "nbsp"
                   , submit "cancel" "Discard"
                   , primHtmlChar "nbsp"
                   , input ! [thetype "button", theclass "editButton",
                              identifier "previewButton",
                              strAttr "onClick" "updatePreviewPane();",
                              strAttr "style" "display: none;",
                              value "Preview" ]
                   , thediv ! [ identifier "previewpane" ] << noHtml
                   ]
  let pgScripts' = ["preview.js"]
  let pgScripts'' = case mathMethod cfg of
       MathML       -> "MathMLinHTML.js" : pgScripts'
       MathJax url  -> url : pgScripts'
       _            -> pgScripts'
  formattedPage defaultPageLayout{
                  pgPageName = page,
                  pgMessages = messages,
                  pgRevision = rev,
                  pgShowPageTools = False,
                  pgShowSiteNav = False,
                  pgMarkupHelp = Just $ markupHelp cfg,
                  pgSelectedTab = EditTab,
                  pgScripts = pgScripts'',
                  pgTitle = ("Editing " ++ page)
                  } editForm

confirmDelete :: Handler
confirmDelete = do
  page <- getPage
  fs <- getFileStore
  cfg <- getConfig
  -- determine whether there is a corresponding page, and if not whether there
  -- is a corresponding file
  pageTest <- liftIO $ E.try $ latest fs (pathForPage page $ defaultExtension cfg)
  fileToDelete <- case pageTest of
                       Right _        -> return $ pathForPage page $ defaultExtension cfg -- a page
                       Left  NotFound -> do
                         fileTest <- liftIO $ E.try $ latest fs page
                         case fileTest of
                              Right _       -> return page  -- a source file
                              Left NotFound -> return ""
                              Left e        -> fail (show e)
                       Left e        -> fail (show e)
  let confirmForm = gui "" <<
        [ p << "Are you sure you want to delete this page?"
        , input ! [thetype "text", name "filetodelete",
                   strAttr "style" "display: none;", value fileToDelete]
        , submit "confirm" "Yes, delete it!"
        , stringToHtml " "
        , submit "cancel" "No, keep it!"
        , br ]
  formattedPage defaultPageLayout{ pgTitle = "Delete " ++ page ++ "?" } $
    if null fileToDelete
       then ulist ! [theclass "messages"] << li <<
            "There is no file or page by that name."
       else confirmForm

deletePage :: Handler
deletePage = withData $ \(params :: Params) -> do
  page <- getPage
  cfg <- getConfig
  let file = pFileToDelete params
  mbUser <- getLoggedInUser
  (user, email) <- case mbUser of
                        Nothing -> return ("Anonymous", "")
                        Just u  -> return (uUsername u, uEmail u)
  let author = Author user email
  let descrip = deleteSummary cfg
  base' <- getWikiBase
  if pConfirm params && (file == page || file == page <.> (defaultExtension cfg))
     then do
       fs <- getFileStore
       liftIO $ Data.FileStore.delete fs file author descrip
       seeOther (base' ++ "/") $ toResponse $ p << "File deleted"
     else seeOther (base' ++ urlForPage page) $ toResponse $ p << "Not deleted"

updatePage :: Handler
updatePage = withData $ \(params :: Params) -> do
  page <- getPage
  cfg <- getConfig
  mbUser <- getLoggedInUser
  (user, email) <- case mbUser of
                        Nothing -> return ("Anonymous", "")
                        Just u  -> return (uUsername u, uEmail u)
  editedText <- case pEditedText params of
                     Nothing -> error "No body text in POST request"
                     Just b  -> applyPreCommitPlugins b
  let logMsg = pLogMsg params `orIfNull` defaultSummary cfg
  let oldSHA1 = pSHA1 params
  fs <- getFileStore
  base' <- getWikiBase
  if null . filter (not . isSpace) $ logMsg
     then withMessages ["Description cannot be empty."] editPage
     else do
       when (length editedText > fromIntegral (maxPageSize cfg)) $
          error "Page exceeds maximum size."
       -- check SHA1 in case page has been modified, merge
       modifyRes <- if null oldSHA1
                       then liftIO $ create fs (pathForPage page $ defaultExtension cfg)
                                       (Author user email) logMsg editedText >>
                                     return (Right ())
                       else do
                         expireCachedFile (pathForPage page $ defaultExtension cfg) `mplus` return ()
                         liftIO $ E.catch (modify fs (pathForPage page $ defaultExtension cfg)
                                            oldSHA1 (Author user email) logMsg
                                            editedText)
                                     (\e -> if e == Unchanged
                                               then return (Right ())
                                               else E.throwIO e)
       case modifyRes of
            Right () -> seeOther (base' ++ urlForPage page) $ toResponse $ p << "Page updated"
            Left (MergeInfo mergedWithRev conflicts mergedText) -> do
               let mergeMsg = "The page has been edited since you checked it out. " ++
                      "Changes from revision " ++ revId mergedWithRev ++
                      " have been merged into your edits below. " ++
                      if conflicts
                         then "Please resolve conflicts and Save."
                         else "Please review and Save."
               editPage' $
                 params{ pEditedText = Just mergedText,
                         pSHA1       = revId mergedWithRev,
                         pMessages   = [mergeMsg] }
-}

data ContentTree = ContentTree { t :: String, c :: [ContentTree] } deriving (Generic, Show)

instance J.ToJSON ContentTree where

contentsPage :: Handler
contentsPage = do
  mbCached <- lookupCache "_contents"
  let emptyResponse = setContentType "application/json; charset=utf-8" $ toResponse noHtml
  case mbCached of
    Nothing -> do
      fs <- getFileStore
      i <- filter (not . null) . filter isValidPath . map dropExtension . filter (".page" `isExtensionOf`) <$> (liftIO $ index fs)
      i' <- mapM (getTitles "") $ mkContents i
      let resp' = setContentType "application/json; charset=utf-8" (toResponse noHtml) {rsBody = BLU.fromString $ BLU.toString $ J.encode $ i'}
      cacheContents "_contents" $ S.concat $ B.toChunks $ rsBody resp'
      ok resp'
    Just (_, contents) -> ok emptyResponse {rsBody = B.fromChunks [contents]}
  where
    getTitles s ct = do
      conf <- getConfig
      fs <- getFileStore
      let targetPage = s ++ t ct
      let targetFile = targetPage ++ ".page"
      revid <- liftIO $ latest fs targetFile
      raw <- liftIO $ retrieve fs targetFile $ Just revid
      let t' = pageTitle $ stringToPage conf (t ct) raw
      c' <- mapM (getTitles (targetPage ++ "/")) (c ct)
      return $ ContentTree t' c'

    mkContents :: [String] -> [ContentTree]
    mkContents (s:ss) = let (children, other) = partition ((s ++ "/") `isPrefixOf`) ss
                            children' = mkContents $ map (drop $ length s + 1) children
                            other'    = mkContents other
                            s'        = splitOn (== '/') s
                        in  diveDeep s' children' : other'
    mkContents []     = []
    splitOn :: (a -> Bool) -> [a] -> [[a]]
    splitOn p' (x:xs) = case (p' x, splitOn p' xs) of
                          (True,  yss)      -> []:yss
                          (False, [])       -> [[x]]
                          (False, (ys:yss)) -> (x:ys):yss
    splitOn _ []     = []
    diveDeep :: [String] -> [ContentTree] -> ContentTree
    diveDeep [s]    cs = ContentTree s cs
    diveDeep (s:ss) cs = ContentTree s [diveDeep ss cs]
    diveDeep []     cs = ContentTree "" cs -- Probably shouldn't happen

indexPage :: Handler
indexPage = do
  path' <- getPath
  base' <- getWikiBase
  cfg <- getConfig
  let ext = defaultExtension cfg
  let prefix' = if null path' then "" else path' ++ "/"
  fs <- getFileStore
  listing <- liftIO $ directory fs prefix'
  let isNotDiscussionPage (FSFile f) = isNotDiscussPageFile f
      isNotDiscussionPage (FSDirectory _) = return True
  prunedListing <- filterM isNotDiscussionPage listing
  let htmlIndex = fileListToHtml base' prefix' ext prunedListing
  formattedPage defaultPageLayout{
                  pgPageName = prefix',
                  pgShowPageTools = False,
                  pgTabs = [],
                  pgScripts = [],
                  pgTitle = "Contents"} htmlIndex

fileListToHtml :: String -> String -> String -> [Resource] -> Html
fileListToHtml base' prefix ext files =
  let fileLink (FSFile f) | takeExtension f == "." ++ ext =
        li ! [theclass "page"  ] <<
          anchor ! [href $ base' ++ urlForPage (prefix ++ dropExtension f)] <<
            dropExtension f
      fileLink (FSFile f) = li ! [theclass "upload"] << concatHtml
        [ anchor ! [href $ base' ++ urlForPage (prefix ++ f)] << f
        , anchor ! [href $ base' ++ "_delete" ++ urlForPage (prefix ++ f)] << "(delete)"
        ]
      fileLink (FSDirectory f) =
        li ! [theclass "folder"] <<
          anchor ! [href $ base' ++ urlForPage (prefix ++ f) ++ "/"] << f
      updirs = drop 1 $ inits $ splitPath $ '/' : prefix
      uplink = foldr (\d accum ->
                  concatHtml [ anchor ! [theclass "updir",
                                         href $ if length d <= 1
                                                   then base' ++ "/_index"
                                                   else base' ++
                                                        urlForPage (joinPath $ drop 1 d)] <<
                  lastNote "fileListToHtml" d, accum]) noHtml updirs
  in uplink +++ ulist ! [theclass "index"] << map fileLink files

-- NOTE:  The current implementation of categoryPage does not go via the
-- filestore abstraction.  That is bad, but can only be fixed if we add
-- more sophisticated searching options to filestore.
categoryPage :: Handler
categoryPage = do
  path' <- getPath
  cfg <- getConfig
  let pcategories = wordsBy (==',') path'
  let repoPath = repositoryPath cfg
  let categoryDescription = "Category: " ++ (intercalate " + " pcategories)
  fs <- getFileStore
  pages <- liftIO (index fs) >>= filterM isPageFile >>= filterM isNotDiscussPageFile
  matches <- liftM catMaybes $
             forM pages $ \f -> do
               categories <- liftIO $ readCategories $ repoPath </> f
               return $ if all ( `elem` categories) pcategories
                           then Just (f, categories \\ pcategories)
                           else Nothing
  base' <- getWikiBase
  let toMatchListItem file = li <<
        [ anchor ! [href $ base' ++ urlForPage (dropExtension file)] << dropExtension file ]
  let toRemoveListItem cat = li << 
        [ anchor ! [href $ base' ++
        (if null (tail pcategories)
         then "/_categories"
         else "/_category" ++ urlForPage (intercalate "," $ Data.List.delete cat pcategories)) ]
        << ("-" ++ cat) ]
  let toAddListItem cat = li <<
        [ anchor ! [href $ base' ++
          "/_category" ++ urlForPage (path' ++ "," ++ cat) ]
        << ("+" ++ cat) ]
  let matchList = ulist << map toMatchListItem (fst $ unzip matches) +++
                  thediv ! [ identifier "categoryList" ] <<
                  ulist << (++) (map toAddListItem (nub $ concat $ snd $ unzip matches)) 
                                (map toRemoveListItem pcategories) 
  formattedPage defaultPageLayout{
                  pgPageName = categoryDescription,
                  pgShowPageTools = False,
                  pgTabs = [],
                  pgScripts = ["search.js"],
                  pgTitle = categoryDescription }
                matchList

categoryListPage :: Handler
categoryListPage = do
  cfg <- getConfig
  let repoPath = repositoryPath cfg
  fs <- getFileStore
  pages <- liftIO (index fs) >>= filterM isPageFile >>= filterM isNotDiscussPageFile
  categories <- liftIO $ liftM (nub . sort . concat) $ forM pages $ \f ->
                  readCategories (repoPath </> f)
  base' <- getWikiBase
  let toCatLink ctg = li <<
        [ anchor ! [href $ base' ++ "/_category" ++ urlForPage ctg] << ctg ]
  let htmlMatches = ulist << map toCatLink categories
  formattedPage defaultPageLayout{
                  pgPageName = "Categories",
                  pgShowPageTools = False,
                  pgTabs = [],
                  pgScripts = ["search.js"],
                  pgTitle = "Categories" } htmlMatches

expireCache :: Handler
expireCache = do
  page <- getPage
  cfg <- getConfig
  -- try it as a page first, then as an uploaded file
  expireCachedFile (pathForPage page $ defaultExtension cfg)
  expireCachedFile page
  ok $ toResponse ()

-- feedHandler :: Handler
-- feedHandler = do
--   cfg <- getConfig
--   when (not $ useFeed cfg) mzero
--   base' <- getWikiBase
--   feedBase <- if null (baseUrl cfg)  -- if baseUrl blank, try to get it from Host header
--                  then do
--                    mbHost <- getHost
--                    case mbHost of
--                         Nothing    -> error "Could not determine base URL"
--                         Just hn    -> return $ "http://" ++ hn ++ base'
--                  else case baseUrl cfg ++ base' of
--                            w@('h':'t':'t':'p':'s':':':'/':'/':_) -> return w
--                            x@('h':'t':'t':'p':':':'/':'/':_) -> return x
--                            y                                 -> return $ "http://" ++ y
--   let fc = FeedConfig{
--               fcTitle = wikiTitle cfg
--             , fcBaseUrl = feedBase
--             , fcFeedDays = feedDays cfg }
--   path' <- getPath     -- e.g. "foo/bar" if they hit /_feed/foo/bar
--   let file = (path' `orIfNull` "_site") <.> "feed"
--   let mbPath = if null path' then Nothing else Just path'
--   -- first, check for a cached version that is recent enough
--   now <- liftIO getCurrentTime
--   let isRecentEnough x = truncate (diffUTCTime now x) < 60 * feedRefreshTime cfg
--   mbCached <- lookupCache file
--   case mbCached of
--        Just (modtime, contents) | isRecentEnough modtime -> do
--             let emptyResponse = setContentType "application/atom+xml; charset=utf-8" . toResponse $ ()
--             ok $ emptyResponse{rsBody = B.fromChunks [contents]}
--        _ -> do
--             fs <- getFileStore
--             resp' <- liftM toResponse $ liftIO (filestoreToXmlFeed fc fs mbPath)
--             cacheContents file $ S.concat $ B.toChunks $ rsBody resp'
--             ok . setContentType "application/atom+xml; charset=UTF-8" $ resp'
