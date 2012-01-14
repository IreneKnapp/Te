#import "AppDelegate.h"
#import <HsFFI.h>
#import "Te/LowLevel/ForeignInterface_stub.h"
#import "Window.h"
#import "BrowserWindow.h"
#import "DocumentWindow.h"

@implementation AppDelegate
@synthesize applicationState;
@synthesize baseFont;
@synthesize emWidth;
@synthesize lineHeight;
@synthesize lineNumberFont;
@synthesize lineNumberEmWidth;
@synthesize lineNumberLineHeight;
@synthesize captionFont;
@synthesize captionEmWidth;
@synthesize captionLineHeight;

- (NSMapTable *) newMapTable {
    if(!keyFunctions) {
        keyFunctions
            = [NSPointerFunctions
                pointerFunctionsWithOptions: NSMapTableCopyIn
                                             | NSMapTableStrongMemory];
        [keyFunctions setHashFunction: uuidHashPointerFunction];
        [keyFunctions setIsEqualFunction: uuidIsEqualPointerFunction];
        [keyFunctions setSizeFunction: uuidSizePointerFunction];
        [keyFunctions setDescriptionFunction: uuidDescriptionPointerFunction];
        [keyFunctions setAcquireFunction: opaqueAcquirePointerFunction];
        [keyFunctions setRelinquishFunction: opaqueRelinquishPointerFunction];
    }
    
    if(!valueFunctions) {
        valueFunctions
            = [NSPointerFunctions
                pointerFunctionsWithOptions:
                 NSMapTableStrongMemory
                 | NSMapTableObjectPointerPersonality];
    }
    
    return [[NSMapTable alloc]
            initWithKeyPointerFunctions: keyFunctions
            valuePointerFunctions: valueFunctions
            capacity: 1024];
}


- (CGFloat) measureEmWidth: (NSFont *) font {
    NSTextStorage *textStorage = [[NSTextStorage alloc] init];
    NSMutableDictionary *attributes
        = [NSMutableDictionary dictionaryWithCapacity: 1];
    [attributes setObject: font forKey: NSFontAttributeName];
    NSAttributedString *attributedString
        = [[NSAttributedString alloc] initWithString: @"M"
                                      attributes: attributes];
    [textStorage setAttributedString: attributedString];
    NSLayoutManager *layoutManager = [[NSLayoutManager alloc] init];
    [textStorage addLayoutManager: layoutManager];
    NSTextContainer *textContainer = [[NSTextContainer alloc] init];
    [textContainer setLineFragmentPadding: 0.0];
    [layoutManager addTextContainer: textContainer];
    NSUInteger numberOfGlyphs = [layoutManager numberOfGlyphs];
    NSRange allGlyphRange = NSMakeRange(0, numberOfGlyphs);
    [layoutManager ensureLayoutForGlyphRange: allGlyphRange];
    CGFloat insertionPointBuffer[2];
    NSUInteger characterIndexBuffer[2];
    NSUInteger nInsertionPoints
        = [layoutManager getLineFragmentInsertionPointsForCharacterAtIndex: 0
                         alternatePositions: NO
                         inDisplayOrder: NO
                         positions: insertionPointBuffer
                         characterIndexes: characterIndexBuffer];
    CGFloat result = insertionPointBuffer[nInsertionPoints - 1];
    return result;
}


- (CGFloat) measureLineHeight: (NSFont *) font {
    CGFloat result = 0.0;
    result += [font ascender];
    result -= [font descender];
    result += [font leading];
    result = ceil(result);
    return result;
}


- (void) applicationWillFinishLaunching: (NSNotification *) notification {
    hsArgc = 1;
    hsArgv = malloc(sizeof(char *));
    hsArgv[0] = "Te";
    hs_init(&hsArgc, &hsArgv);
    
    applicationState = teApplicationInit((HsFunPtr) exception,
                                         (HsFunPtr) confirm,
                                         (HsFunPtr) noteRecentProjectsChanged,
                                         (HsFunPtr) getEmWidth,
                                         (HsFunPtr) getLineHeight,
                                         (HsFunPtr) getLineNumberEmWidth,
                                         (HsFunPtr) getScrollerWidth,
                                         (HsFunPtr) getVisibleWidth,
                                         (HsFunPtr) getVisibleHeight,
                                         (HsFunPtr) noteDeletedWindow,
                                         (HsFunPtr) activateWindow,
                                         (HsFunPtr) noteNewBrowserWindow,
                                         (HsFunPtr) noteBrowserItemsChanged,
                                         (HsFunPtr) editBrowserItemName,
                                         (HsFunPtr) noteNewDocumentWindow);
    
    keyFunctions = nil;
    valueFunctions = nil;
    windows = [self newMapTable];
    
    char *versionLabelCString = teVersionString();
    NSString *versionLabelString = @"";
    if(versionLabelCString) {
        versionLabelString
            = [NSString stringWithUTF8String: versionLabelCString];
        teStringFree(versionLabelCString);
    }
    [metaProjectVersionLabel setStringValue: versionLabelString];
    
    [metaProjectRecentList setIntercellSpacing: NSMakeSize(0.0, 0.0)];
    [metaProjectRecentList setTarget: self];
    [metaProjectRecentList setDoubleAction: @selector(openRecentProject:)];
    
    openPanel = [NSOpenPanel openPanel];
    NSMutableArray *fileTypes = [NSMutableArray arrayWithCapacity: 1];
    [fileTypes addObject: @"com.dankna.te.project"];
    [openPanel setAllowedFileTypes: fileTypes];
    [openPanel setCanChooseFiles: YES];
    [openPanel setCanChooseDirectories: NO];
    [openPanel setAllowsMultipleSelection: YES];
    [openPanel setResolvesAliases: YES];
    
    baseFont = [NSFont fontWithName: @"Monaco" size: 14.0];
    emWidth = [self measureEmWidth: baseFont];
    lineHeight = [self measureLineHeight: baseFont];
    
    lineNumberFont = [NSFont fontWithName: @"Monaco" size: 10.0];
    lineNumberEmWidth = [self measureEmWidth: lineNumberFont];
    lineNumberLineHeight = [self measureLineHeight: lineNumberFont];
    
    captionFont = lineNumberFont;
    captionEmWidth = lineNumberEmWidth;
    captionLineHeight = lineNumberLineHeight;
}


- (void) applicationWillTerminate: (NSNotification *) notification {
    teApplicationExit(applicationState);
    hs_exit();
    applicationState = NULL;
}


- (void) applicationDidFinishLaunching: (NSNotification *) notification {
    [metaProjectWindow makeKeyAndOrderFront: self];
}


- (BOOL) applicationShouldOpenUntitledFile: (NSApplication *) sender {
    return NO;
}


- (NSInteger) numberOfRowsInTableView: (NSTableView *) tableView {
    if([tableView isEqual: metaProjectRecentList]) {
        if(!applicationState)
            return 0;
        
        uint64_t count = teApplicationRecentProjectCount(applicationState);
        if(count == 0) {
            return 1;
        } else {
            return count;
        }
    } else {
        return 0;
    }
}


- (id)            tableView: (NSTableView *) tableView
  objectValueForTableColumn: (NSTableColumn *) tableColumn
                        row: (NSInteger) rowIndex
{
    if([tableView isEqual: metaProjectRecentList]) {
        if(!applicationState)
            return nil;
        
        if(teApplicationRecentProjectCount(applicationState) == 0) {
            if([tableColumn isEqual: metaProjectRecentListIconColumn]) {
                return [NSImage imageNamed: @"FirstRunAttentionArrow"];
            } else if([tableColumn isEqual: metaProjectRecentListTextColumn]) {
                NSString *string = @"Recent projects\nwill appear here.";
                
                NSMutableDictionary *attributes
                    = [NSMutableDictionary dictionaryWithCapacity: 1];
                
                NSColor *foregroundColor = [NSColor disabledControlTextColor];
                [attributes setObject: foregroundColor
                            forKey: NSForegroundColorAttributeName];
                
                NSAttributedString *attributedString
                    = [[NSAttributedString alloc] initWithString: string
                                                  attributes: attributes];
                return attributedString;
            } else {
                return nil;
            }
        } else {
            if([tableColumn isEqual: metaProjectRecentListIconColumn]) {
                return [NSImage imageNamed: @"Project"];
            } else if([tableColumn isEqual: metaProjectRecentListTextColumn]) {
                return @"Foo";
            } else {
                return nil;
            }
        }
    } else {
        return nil;
    }
}


- (BOOL) tableView: (NSTableView *) tableView
   shouldSelectRow: (NSInteger) rowIndex
{
    if([tableView isEqual: metaProjectRecentList]) {
        if(YES) {
            return NO;
        } else {
            return YES;
        }
    } else {
        return YES;
    }
}


- (IBAction) newProject: (id) sender {
    if(!applicationState)
        return;
    
    teApplicationNewProject(applicationState);
}


- (IBAction) openProject: (id) sender {
    if(!applicationState)
        return;
    
    [openPanel beginWithCompletionHandler:
        ^(NSInteger result) {
            if(result != NSFileHandlingPanelOKButton) 
                return;
            
            for(NSURL *url in [openPanel URLs]) {
                if(![url isFileURL])
                    continue;
                
                NSString *path = [url path];
                char *pathCString = (char *) [path UTF8String];
                teApplicationOpenProject(applicationState, pathCString);
            }
        }];
}


- (IBAction) openRecentProject: (id) sender {
    if(!applicationState)
        return;
    
    if([sender isEqual: metaProjectRecentList]) {
        NSInteger row = [metaProjectRecentList clickedRow];
        if(-1 != row) {
            uint64_t index = (uint64_t) row;
            teApplicationOpenRecentProject(applicationState, index);
        }
    }
}


- (IBAction) newFolder: (id) sender {
    if(!applicationState)
        return;
    
    NSWindow *window = [NSApp mainWindow];
    if(!window)
        return;
    
    NSWindowController *windowController = [window windowController];
    if(!windowController)
        return;
    
    if([windowController isKindOfClass: [BrowserWindow class]]) {
        BrowserWindow *browserWindowObject
            = (BrowserWindow *) windowController;
        uuid_t *browserWindowID = [browserWindowObject windowID];
        uuid_t inodeID;
        if([browserWindowObject getCurrentFolderInodeID: &inodeID]) {
            teBrowserItemNewFolderInside(applicationState,
                                         browserWindowID,
                                         inodeID);
        }
    }
}


- (IBAction) newHaskellModule: (id) sender {
    if(!applicationState)
        return;
    
    NSWindow *window = [NSApp mainWindow];
    if(!window)
        return;
    
    NSWindowController *windowController = [window windowController];
    if(!windowController)
        return;
    
    if([windowController isKindOfClass: [BrowserWindow class]]) {
        BrowserWindow *browserWindowObject
            = (BrowserWindow *) windowController;
        uuid_t *browserWindowID = [browserWindowObject windowID];
        uuid_t inodeID;
        if([browserWindowObject getCurrentFolderInodeID: &inodeID]) {
            teBrowserItemNewFileInside(applicationState,
                                       browserWindowID,
                                       inodeID);
        }
    }
}


- (IBAction) deleteFolderOrFile: (id) sender {
    if(!applicationState)
        return;
    
    NSWindow *window = [NSApp mainWindow];
    if(!window)
        return;
    
    NSWindowController *windowController = [window windowController];
    if(!windowController)
        return;
    
    if([windowController isKindOfClass: [BrowserWindow class]]) {
        BrowserWindow *browserWindowObject
            = (BrowserWindow *) windowController;
        uuid_t *browserWindowID = [browserWindowObject windowID];
        
        void *inodeList = [browserWindowObject getSelectedInodeList];
        
        if(!inodeList)
            return;
        
        void *inodeListKernel = teInodeListKernel(inodeList);
        
        teInodeListFree(inodeList);
        
        if(!inodeListKernel)
            return;
        
        teInodeListDelete(applicationState, browserWindowID, inodeListKernel);
        
        teInodeListFree(inodeListKernel);
    }
}


- (void) exceptionWithMessage: (NSString *) messageString
                      details: (NSString *) detailsString
{
    NSAlert *alert = [[NSAlert alloc] init];
    [alert setMessageText: messageString];
    [alert setInformativeText: detailsString];
    [alert addButtonWithTitle: NSLocalizedString(@"Okay", @"Okay")];
    [alert runModal];
}


void exception(char *messageCString, char *detailsCString) {
    if(messageCString && detailsCString) {
        NSString *messageString
            = [NSString stringWithUTF8String: messageCString];
        NSString *detailsString
            = [NSString stringWithUTF8String: detailsCString];
        [(AppDelegate *) [NSApp delegate] exceptionWithMessage: messageString
                                          details: detailsString];
    }
}


- (uint64_t) confirm: (void *) confirmationDialog
   completionHandler: (void (*)(uint64_t result)) completionHandler
{
    Window *windowObject = nil;
    uuid_t windowID;
    teConfirmationDialogWindow(confirmationDialog, &windowID);
    if(!uuidIsNull(&windowID)) {
        windowObject = [windows objectForKey: (void *) windowID];
    }
    
    char *messageCString = teConfirmationDialogMessage(confirmationDialog);
    NSString *message = [NSString stringWithUTF8String: messageCString];
    teStringFree(messageCString);
    
    char *detailsCString = teConfirmationDialogDetails(confirmationDialog);
    NSString *details = [NSString stringWithUTF8String: detailsCString];
    teStringFree(detailsCString);
    
    uint64_t defaultButtonIndex
        = teConfirmationDialogDefaultButtonIndex(confirmationDialog);
    
    uint64_t cancelButtonIndex
        = teConfirmationDialogCancelButtonIndex(confirmationDialog);
    
    uint64_t buttonCount
        = teConfirmationDialogButtonCount(confirmationDialog);
    
    NSAlert *alert = [[NSAlert alloc] init];
    [alert setMessageText: message];
    [alert setInformativeText: details];
    
    for(uint64_t i = 0; i < buttonCount; i++) {
        char *buttonTitleCString
            = teConfirmationDialogButton(confirmationDialog, i);
        if(buttonTitleCString) {
            NSString *buttonTitle
                = [NSString stringWithUTF8String: buttonTitleCString];
            teStringFree(buttonTitleCString);
            
            NSButton *button = [alert addButtonWithTitle: buttonTitle];
            
            if(i == defaultButtonIndex) {
                [button setKeyEquivalent: @"\r"];
            } else if(i == cancelButtonIndex) {
                [button setKeyEquivalent: @"\x1B"];
            } else {
                [button setKeyEquivalent: @""];
            }
            
            [button setTag: i];
        }
    }
    
    if(windowObject) {
        [windowObject runSheetModalAlert: alert
                      completionHandler: completionHandler];
    } else {
        uint64_t result = [alert runModal];
        completionHandler(result);
        teCompletionHandlerFree((HsFunPtr) completionHandler);
    }
}


void confirm(void *confirmationDialog,
             void (*completionHandler)(uint64_t result))
{
    [(AppDelegate *) [NSApp delegate] confirm: confirmationDialog
                                      completionHandler: completionHandler];
}


- (void) noteRecentProjectsChanged {
    [metaProjectRecentList reloadData];
}


void noteRecentProjectsChanged() {
    [(AppDelegate *) [NSApp delegate] noteRecentProjectsChanged];
}


double getEmWidth() {
    return [(AppDelegate *) [NSApp delegate] emWidth];
}


double getLineHeight() {
    return [(AppDelegate *) [NSApp delegate] lineHeight];
}


double getLineNumberEmWidth() {
    return [(AppDelegate *) [NSApp delegate] lineNumberEmWidth];
}


double getScrollerWidth() {
    NSScrollerStyle scrollerStyle = [NSScroller preferredScrollerStyle];
    if(scrollerStyle == NSScrollerStyleLegacy) {
        CGFloat scrollerWidth
            = [NSScroller scrollerWidthForControlSize: NSRegularControlSize
                          scrollerStyle: scrollerStyle];
        return (double) scrollerWidth;
    } else {
        return 0.0;
    }
}


double getVisibleWidth() {
    return [[NSScreen mainScreen] visibleFrame].size.width;
}


double getVisibleHeight() {
    return [[NSScreen mainScreen] visibleFrame].size.height;
}


- (void) noteDeletedWindow: (uuid_t *) windowID {
    Window *windowObject
        = [windows objectForKey: (void *) windowID];
    if(windowObject) {
        [windowObject forceClose];
        [windows removeObjectForKey: (void *) windowID];
    }
}


void noteDeletedWindow(uuid_t *windowID) {
    [(AppDelegate *) [NSApp delegate] noteDeletedWindow: windowID];
}


- (void) activateWindow: (uuid_t *) windowID {
    Window *windowObject
        = [windows objectForKey: (void *) windowID];
    if(windowObject) {
        [windowObject showWindow: self];
    }
}


void activateWindow(uuid_t *windowID) {
    [(AppDelegate *) [NSApp delegate] activateWindow: windowID];
}


- (void) noteNewBrowserWindow: (uuid_t *) browserWindowID {
    BrowserWindow *browserWindowObject
        = [[BrowserWindow alloc] initWithWindowID: browserWindowID];
    if(browserWindowObject)
        [windows setObject: browserWindowObject
                 forKey: (void *) browserWindowID];
}


void noteNewBrowserWindow(uuid_t *browserWindowID) {
    [(AppDelegate *) [NSApp delegate] noteNewBrowserWindow: browserWindowID];
}


- (void) noteBrowserItemsChangedInBrowserWindow: (uuid_t *) browserWindowID {
    Window *windowObject = [windows objectForKey: (void *) browserWindowID];
    if(windowObject && [windowObject isKindOfClass: [BrowserWindow class]]) {
        BrowserWindow *browserWindowObject = (BrowserWindow *) windowObject;
        [browserWindowObject noteItemsChanged];
    }
}


void noteBrowserItemsChanged(uuid_t *browserWindowID) {
    [(AppDelegate *) [NSApp delegate]
     noteBrowserItemsChangedInBrowserWindow: browserWindowID];
}


- (void) editBrowserItemNameInBrowserWindow: (uuid_t *) browserWindowID
                                      inode: (uuid_t *) inodeID
{
    Window *windowObject = [windows objectForKey: (void *) browserWindowID];
    if(windowObject && [windowObject isKindOfClass: [BrowserWindow class]]) {
        BrowserWindow *browserWindowObject = (BrowserWindow *) windowObject;
        [browserWindowObject editItemName: inodeID];
    }
}


void editBrowserItemName(uuid_t *browserWindowID, uuid_t *inodeID) {
    [(AppDelegate *) [NSApp delegate]
     editBrowserItemNameInBrowserWindow: browserWindowID inode: inodeID];
}


- (void) noteNewDocumentWindow: (uuid_t *) documentWindowID {
    DocumentWindow *documentWindowObject
        = [[DocumentWindow alloc] initWithWindowID: documentWindowID];
    if(documentWindowObject)
        [windows setObject: documentWindowObject
                 forKey: (void *) documentWindowID];
}


void noteNewDocumentWindow(uuid_t *documentWindowID) {
    [(AppDelegate *) [NSApp delegate] noteNewDocumentWindow: documentWindowID];
}

@end
