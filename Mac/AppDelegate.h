#import <Cocoa/Cocoa.h>
#import "Utilities.h"

@interface AppDelegate : NSDocumentController
<NSApplicationDelegate, NSTableViewDataSource, NSTableViewDelegate>
{
    IBOutlet NSWindow *metaProjectWindow;
    IBOutlet NSTextField *metaProjectVersionLabel;
    IBOutlet NSTableView *metaProjectRecentList;
    IBOutlet NSTableColumn *metaProjectRecentListIconColumn;
    IBOutlet NSTableColumn *metaProjectRecentListTextColumn;
    
    NSOpenPanel *openPanel;
    
    void *applicationState;
    NSPointerFunctions *keyFunctions;
    NSPointerFunctions *valueFunctions;
    NSMapTable *windows;
    
    NSFont *baseFont;
    CGFloat emWidth;
    CGFloat lineHeight;
    
    NSFont *lineNumberFont;
    CGFloat lineNumberEmWidth;
    CGFloat lineNumberLineHeight;
    
    NSFont *captionFont;
    CGFloat captionEmWidth;
    CGFloat captionLineHeight;
        
    int hsArgc;
    char **hsArgv;
}
@property (assign) void *applicationState;
@property (assign) NSFont *baseFont;
@property (assign) CGFloat emWidth;
@property (assign) CGFloat lineHeight;
@property (assign) NSFont *lineNumberFont;
@property (assign) CGFloat lineNumberEmWidth;
@property (assign) CGFloat lineNumberLineHeight;
@property (assign) NSFont *captionFont;
@property (assign) CGFloat captionEmWidth;
@property (assign) CGFloat captionLineHeight;

- (NSMapTable *) newMapTable;
- (CGFloat) measureEmWidth: (NSFont *) font;
- (CGFloat) measureLineHeight: (NSFont *) font;
- (void) applicationWillFinishLaunching: (NSNotification *) notification;
- (void) applicationWillTerminate: (NSNotification *) notification;
- (void) applicationDidFinishLaunching: (NSNotification *) notification;
- (BOOL) applicationShouldOpenUntitledFile: (NSApplication *) sender;
- (NSInteger) numberOfRowsInTableView: (NSTableView *) tableView;
- (id)            tableView: (NSTableView *) tableView
  objectValueForTableColumn: (NSTableColumn *) tableColumn
                        row: (NSInteger) rowIndex;
- (BOOL) tableView: (NSTableView *) tableView
   shouldSelectRow: (NSInteger) rowIndex;
- (IBAction) newProject: (id) sender;
- (IBAction) openProject: (id) sender;
- (IBAction) openRecentProject: (id) sender;
- (IBAction) newFolder: (id) sender;
- (IBAction) newHaskellModule: (id) sender;
- (IBAction) deleteFolderOrFile: (id) sender;
- (void) exceptionWithMessage: (NSString *) messageString
                      details: (NSString *) detailsString;
void exception(char *messageCString, char *detailsCString);
- (uint64_t) confirm: (void *) confirmationDialog
   completionHandler: (void (*)(uint64_t result)) completionHandler;
void confirm(void *confirmationDialog,
             void (*completionHandler)(uint64_t result));
- (void) noteRecentProjectsChanged;
void noteRecentProjectsChanged();
double getEmWidth();
double getLineHeight();
double getLineNumberEmWidth();
double getScrollerWidth();
void getVisibleFrame
    (int64_t *left, int64_t *top, int64_t *width, int64_t *height);
void getDocumentContentFromFrame
    (int64_t *left, int64_t *top, int64_t *width, int64_t *height);
- (void) noteDeletedWindow: (uuid_t *) windowID;
void noteDeletedWindow(uuid_t *windowID);
- (void) activateWindow: (uuid_t *) windowID;
void activateWindow(uuid_t *windowID);
- (void) noteNewBrowserWindow: (uuid_t *) browserWindowID;
void noteNewBrowserWindow(uuid_t *browserWindowID);
- (void) noteBrowserItemsChangedInBrowserWindow: (uuid_t *) browserWindowID;
void noteBrowserItemsChanged(uuid_t *browserWindowID);
- (void) editBrowserItemNameInBrowserWindow: (uuid_t *) browserWindowID
                                      inode: (uuid_t *) inodeID;
void editBrowserItemName(uuid_t *browserWindowID, uuid_t *inodeID);
- (void) noteNewDocumentWindow: (uuid_t *) documentWindowID
              contentRectangle: (NSRect) contentRectangle;
void noteNewDocumentWindow
    (uuid_t *documentWindowID,
     int64_t left, int64_t top, int64_t width, int64_t height);
- (void) noteNewDocumentPane: (uuid_t *) documentPaneID
                    inWindow: (uuid_t *) documentWindowID
                   withFrame: (NSRect) frame;
void noteNewDocumentPane(uuid_t *documentWindowID,
                         uuid_t *documentPaneID,
                         int64_t left,
                         int64_t top,
                         int64_t width,
                         int64_t height);
- (void) noteNewHorizontalGhostDividerInWindow: (uuid_t *) documentWindowID
                                     withFrame: (NSRect) frame
                                      location: (NSPoint) location;
void noteNewHorizontalGhostDivider(uuid_t *documentWindowID,
                                   int64_t left,
                                   int64_t top,
                                   int64_t width,
                                   int64_t height,
                                   int64_t x,
                                   int64_t y);
- (void) noteNewVerticalGhostDividerInWindow: (uuid_t *) documentWindowID
                                   withFrame: (NSRect) frame
                                    location: (NSPoint) location;
void noteNewVerticalGhostDivider(uuid_t *documentWindowID,
                                 int64_t left,
                                 int64_t top,
                                 int64_t width,
                                 int64_t height,
                                 int64_t x,
                                 int64_t y);
- (void) cleanupGhostWindow;
@end
