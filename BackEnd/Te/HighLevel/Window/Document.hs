module Te.HighLevel.Window.Document
  (DocumentWindow,
   DocumentWindowID,
   documentWindowID)
  where

import Te.HighLevel.Window
import Te.LowLevel.Database
import Te.LowLevel.Identifiers
import Te.Types


instance WindowType DocumentWindow where
  toWindow documentWindow =
             Window {
                 windowID = toWindowID $ documentWindowID documentWindow,
                 windowProject = documentWindowProject documentWindow
               }
  getFromWindow window = do
    kind <- lookupWindowKind window
    case kind of
      WindowKindDocument ->
        return $ Just $ DocumentWindow {
                            documentWindowID = fromWindowID $ windowID window,
                            documentWindowProject = windowProject window
                          }
      _ -> return Nothing
