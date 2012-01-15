#import <Cocoa/Cocoa.h>


@class TransparentHelperWindow;
@interface WindowDocumentPaneManager : NSObject
{
    NSMutableArray *panesInformation;
    NSTextStorage *textStorage;
    NSLayoutManager *layoutManager;
    NSTextContainer *textContainer;
}
@property (assign) NSMutableArray *panesInformation;
@property (assign) NSTextStorage *textStorage;
@property (assign) NSLayoutManager *layoutManager;
@property (assign) NSTextContainer *textContainer;

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
