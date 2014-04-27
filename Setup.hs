import Distribution.Simple
import Distribution.MacOSX

main = defaultMainWithHooks $ simpleUserHooks {
    buildHook = appBundleBuildHook apps,
    installHook = appBundleInstallHook apps
  }


apps :: [MacApp]
apps =
  [MacApp {
       appName = "Te",
       appIcon = Just "Mac/Resources/Application.icns",
       appPlist = Just "Mac/Info.plist",
       resources =
         ["Mac/Resources/Project.icns",
          "Mac/Resources/Te.png",
          "Mac/Resources/FirstRunAttentionArrow.png",
          "Mac/Resources/File.png",
          "Mac/Resources/Folder.png",
          "Mac/Resources/BadgeNew.png",
          "Mac/Resources/BadgeChanged.png",
          "Mac/Resources/BadgeMoved.png",
          "Mac/Resources/MainMenu.xib",
          "Mac/Resources/BrowserWindow.xib"],
       otherBins = [],
       appDeps = ChaseWithDefaults
     }]
