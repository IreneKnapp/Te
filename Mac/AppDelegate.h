#import <Cocoa/Cocoa.h>

@interface AppDelegate : NSObject
<NSApplicationDelegate,NSTableViewDataSource, NSTableViewDelegate>
{
    IBOutlet NSWindow *metaProjectWindow;
    IBOutlet NSTextField *metaProjectVersionLabel;
    IBOutlet NSTableView *metaProjectRecentList;
    IBOutlet NSTableColumn *metaProjectRecentListIconColumn;
    IBOutlet NSTableColumn *metaProjectRecentListTextColumn;
    
    void *applicationState;
}

- (void) applicationWillFinishLaunching: (NSNotification *) notification;
- (void) applicationWillTerminate: (NSNotification *) notification;
- (void) applicationDidFinishLaunching: (NSNotification *) notification;
- (NSInteger) numberOfRowsInTableView: (NSTableView *) tableView;
- (id)            tableView: (NSTableView *) tableView
  objectValueForTableColumn: (NSTableColumn *) tableColumn
                        row: (NSInteger) rowIndex;
- (BOOL) tableView: (NSTableView *) tableView
   shouldSelectRow: (NSInteger) rowIndex;
@end
