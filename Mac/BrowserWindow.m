#import "BrowserWindow.h"
#import <HsFFI.h>
#import "Te/ForeignInterface_stub.h"
#import "AppDelegate.h"
#import "BrowserItem.h"
#import "Utilities.h"


@implementation BrowserWindow

- (id) initWithBrowserWindowID: (uuid_t *) newBrowserWindowID {
    void *applicationState = getApplicationState();
    if(!applicationState)
        return nil;
    
    self = [super initWithWindowNibName: @"BrowserWindow"];
    if(self) {
        copyUUID(&browserWindowID, newBrowserWindowID);
        
        browserItems = [(AppDelegate *) [NSApp delegate] newMapTable];
        
        alreadyClosing = NO;
        
        char *titleCString
            = teBrowserWindowTitle(applicationState, &browserWindowID);
        NSString *title = @"";
        if(titleCString) {
            title = [NSString stringWithUTF8String: titleCString];
            teStringFree(titleCString);
        }
        
        char *titleIconCString
            = teBrowserWindowTitleIcon(applicationState, &browserWindowID);
        NSString *titleIcon = @"";
        if(titleIconCString) {
            titleIcon = [NSString stringWithUTF8String: titleIconCString];
            teStringFree(titleIconCString);
        }
        
        NSWindow *window = [self window];
        [window makeKeyAndOrderFront: self];
        [window setTitle: title];
        [window setRepresentedURL: [NSURL URLWithString: @""]];
        [[window standardWindowButton: NSWindowDocumentIconButton]
         setImage: [NSImage imageNamed: titleIcon]];
        
        [filesOutlineViewNameColumn setDataCell: [[NSBrowserCell alloc] init]];
        [filesOutlineView reloadData];
    } else {
        teFrontEndInternalFailure(applicationState, __FILE__, __LINE__);
    }
    return self;
}


- (BrowserItem *) getBrowserItemWithInodeID: (uuid_t *) inodeID {
    BrowserItem *browserItem = [browserItems objectForKey: (void *) inodeID];
    if(!browserItem) {
        browserItem
            = [[BrowserItem alloc] initWithBrowserWindowObject: self
                                   inodeID: inodeID];
        [browserItems setObject: browserItem
                      forKey: (void *) inodeID];
    }
    return browserItem;
}


- (void) forceClose {
    if(!alreadyClosing)
        [[self window] close];
}


- (void) noteItemsChanged {
    [filesOutlineView reloadData];
}


- (void) editItemName: (uuid_t *) inodeID {
}


- (BOOL) windowShouldClose: (id) sender {
    return YES;
}


- (void) windowWillClose: (NSNotification *) notification {
    alreadyClosing = YES;
    
    void *applicationState = getApplicationState();
    if(!applicationState)
        return;
    
    teBrowserWindowClose(applicationState, &browserWindowID);
}


- (BOOL)               window: (NSWindow *) window
  shouldDragDocumentWithEvent: (NSEvent *) event
                         from: (NSPoint) dragImageLocation
               withPasteboard: (NSPasteboard *) pasteboard
{
    return NO;
}


- (BOOL)               window: (NSWindow *) window
  shouldPopUpDocumentPathMenu: (NSMenu *) menu
{
    return NO;
}


- (id) outlineView: (NSOutlineView *) outlineView
             child: (NSInteger) index
            ofItem: (id) item
{
    if([outlineView isEqual: filesOutlineView]) {
        void *applicationState = getApplicationState();
        if(!applicationState)
            return nil;
        
        uuid_t inodeChildID;
        
        if(!item) {
            uuid_t inodeID;
            teBrowserWindowRoot(applicationState,
                                &browserWindowID,
                                &inodeID);
            
            teInodeChild(applicationState,
                         &browserWindowID,
                         &inodeID,
                         index,
                         &inodeChildID);
        } else if([item isKindOfClass: [BrowserItem class]]) {
            BrowserItem *browserItem = (BrowserItem *) item;
            
            uuid_t *inodeID = [browserItem inodeID];
            
            teInodeChild(applicationState,
                         &browserWindowID,
                         inodeID,
                         index,
                         &inodeChildID);
        } else {
            return nil;
        }
        
        return [self getBrowserItemWithInodeID: &inodeChildID];
    } else {
        return nil;
    }
}


- (BOOL) outlineView: (NSOutlineView *) outlineView
    isItemExpandable: (id) item
{
    if([outlineView isEqual: filesOutlineView]) {
        void *applicationState = getApplicationState();
        if(!applicationState)
            return NO;
        
        if([item isKindOfClass: [BrowserItem class]]) {
            BrowserItem *browserItem = (BrowserItem *) item;
            
            uuid_t *inodeID = [browserItem inodeID];
            uint64_t result
                = teInodeExpandable(applicationState,
                                    &browserWindowID,
                                    inodeID);
            if(result)
                return YES;
            else
                return NO;
        } else {
            return NO;
        }
    } else {
        return NO;
    }
}


- (NSInteger) outlineView: (NSOutlineView *) outlineView
   numberOfChildrenOfItem: (id) item
{
    if([outlineView isEqual: filesOutlineView]) {
        void *applicationState = getApplicationState();
        if(!applicationState)
            return 0;
        
        if(!item) {
            uuid_t inodeID;
            teBrowserWindowRoot(applicationState,
                                &browserWindowID,
                                &inodeID);
            
            uint64_t count
                = teInodeChildCount(applicationState,
                                    &browserWindowID,
                                    &inodeID);
            return count;
        } else if([item isKindOfClass: [BrowserItem class]]) {
            BrowserItem *browserItem = (BrowserItem *) item;
            
            uuid_t *inodeID = [browserItem inodeID];
            uint64_t count
                = teInodeChildCount(applicationState,
                                    &browserWindowID,
                                    inodeID);
            return count;
        } else {
            return 0;
        }
    } else {
        return 0;
    }
}


- (id)          outlineView: (NSOutlineView *) outlineView
  objectValueForTableColumn: (NSTableColumn *) tableColumn
                     byItem: (id) item
{
    if([outlineView isEqual: filesOutlineView]) {
        if([tableColumn isEqual: filesOutlineViewNameColumn]) {
            return @"Foo.";
        } else if([tableColumn isEqual: filesOutlineViewDateModifiedColumn]) {
            return @"5 seconds ago";
        } else if([tableColumn isEqual: filesOutlineViewSizeColumn]) {
            return @"32KiB";
        } else if([tableColumn isEqual: filesOutlineViewKindColumn]) {
            return @"Haskell";
        } else {
            return nil;
        }
    } else {
        return nil;
    }
}


- (void) outlineView: (NSOutlineView *) outlineView
     willDisplayCell: (id) cell
      forTableColumn: (NSTableColumn *) tableColumn
                item: (id) item
{
    if([outlineView isEqual: filesOutlineView]) {
        if([tableColumn isEqual: filesOutlineViewNameColumn]) {
            [cell setImage: [NSImage imageNamed: @"File"]];
        }
    }
}


- (void) outlineView: (NSOutlineView *) outlineView
      setObjectValue: (id) object
      forTableColumn: (NSTableColumn *) tableColumn
              byItem: (id) item
{
    if([outlineView isEqual: filesOutlineView]) {
        if([tableColumn isEqual: filesOutlineViewNameColumn]) {
        }
    }
}

@end
