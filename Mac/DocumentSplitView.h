#import <Cocoa/Cocoa.h>


@class DocumentContentView;
@interface DocumentSplitView : NSView
{
    NSMutableArray *contentSubviews;
    NSMutableArray *dividerSubviews;
    
    BOOL trackingDividerDrag;
    NSUInteger dividerBeingTracked;
    NSPoint previousDragPoint;
    BOOL collapsedAbove;
    BOOL collapsedBelow;
    BOOL creatingNewDivider;
}
@property (readonly) NSArray *contentSubviews;

+ (CGFloat) minimumDividerThickness;
- (id) initWithFrame: (NSRect) frame;
- (DocumentContentView *) newContentSubviewAtIndex: (NSUInteger) index;
- (void) removeContentSubviewAtIndex: (NSUInteger) index;
- (NSView *) dividerSubviewAtIndex: (NSUInteger) dividerIndex;
- (void) drawRect: (NSRect) dirtyRect;
- (NSView *) hitTest: (NSPoint) point;
- (void) mouseDown: (NSEvent *) event;
- (void) mouseDragged: (NSEvent *) event;
- (void) mouseUp: (NSEvent *) event;
- (void) adjustSubviews;
- (void) adjustSubviewsToEqualSizes;
- (CGFloat) dividerThickness;
- (CGFloat) absoluteMinCoordinateOfDividerAt: (NSUInteger) dividerIndex;
- (CGFloat) absoluteMaxCoordinateOfDividerAt: (NSUInteger) dividerIndex;
- (CGFloat) constrainMinCoordinate: (CGFloat) proposedPosition
                       ofDividerAt: (NSUInteger) dividerIndex;
- (CGFloat) constrainMaxCoordinate: (CGFloat) proposedPosition
                       ofDividerAt: (NSUInteger) dividerIndex;
- (CGFloat) constrainSplitPosition: (CGFloat) proposedPosition
                       ofDividerAt: (NSUInteger) dividerIndex;
- (CGFloat) subviewMinimumSize;
- (CGFloat) subviewCollapseThresholdSize;
@end
