#import <Cocoa/Cocoa.h>


@class TransparentHelperWindow;
@interface WindowDocumentPaneManager : NSObject
@property (retain) NSMutableArray *panesInformation;
@property (retain) NSTextStorage *textStorage;
@property (retain) NSLayoutManager *layoutManager;
@property (retain) NSTextContainer *textContainer;

+ (id) sharedManager;
+ (id) allocWithZone: (NSZone *) zone;
- (id) copyWithZone: (NSZone *) zone;
- (void) addPane: (uuid_t *) documentPaneID
        toWindow: (NSWindow *) window
       withFrame: (NSRect) frame;
- (void) repackScrollbars;
- (void) drawRect: (NSRect) dirtyRect
         ofWindow: (NSWindow *) window;
- (IBAction) scrollerActivated: (id) sender;
- (void) showScrollersForFrame: (NSUInteger) frameIndex;
- (void) hideScrollersAfterDelayForFrame: (NSUInteger) frameIndex;
- (void) hideScrollersAfterDelayTimerFired: (NSTimer *) timer;
- (void) flashScrollersForFrame: (NSUInteger) frameIndex;
- (void) preferredScrollerStyleDidChange: (NSNotification *) notification;
- (void) mouseDown: (NSEvent *) event;
@end
