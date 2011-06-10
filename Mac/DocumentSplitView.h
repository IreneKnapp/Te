#import <Cocoa/Cocoa.h>


enum SplitAxis {
    UncommittedSplitAxis,
    VerticalSplitAxis,
    HorizontalSplitAxis
};

@class DocumentContentView;
@class TransparentHelperWindow;
@interface DocumentSplitView : NSView
{
    NSMutableArray *contentSubviews;
    NSMutableArray *dividerSubviewsForVerticalContent;
    NSMutableArray *dividerSubviewsForHorizontalContent;
    
    enum SplitAxis committedAxis;
    
    NSMutableDictionary *captionAttributes;
    NSMutableDictionary *titleAttributes;
    NSMutableDictionary *titleUnderprintAttributes;
    NSColor *topBorderColor;
    NSColor *bottomBorderColor;
    NSColor *curvyBorderColor;
    NSGradient *captionGradient;
    NSGradient *titleGradient;
    CGFloat baselineOffset;
    CGFloat captionInset;
    CGFloat captionLineHeight;
    CGFloat titleLineHeight;
    
    BOOL trackingDividerDrag;
    NSUInteger dividerIndexBeingTracked;
    enum SplitAxis dividerAxisBeingTracked;
    NSPoint previousDragPoint;
    BOOL collapsedAbove;
    BOOL collapsedBelow;
    BOOL createdAbove;
    BOOL createdBelow;
    BOOL creatingNewDivider;
    TransparentHelperWindow *ghostWindow;
}
@property (readonly) NSArray *contentSubviews;

+ (CGFloat) minimumDividerThicknessForAxis: (enum SplitAxis) dividerAxis;
- (id) initWithFrame: (NSRect) frame;
- (DocumentContentView *) newContentSubviewAtIndex: (NSUInteger) index
                          alongAxis: (enum SplitAxis) alongAxis;
- (void) removeContentSubviewAtIndex: (NSUInteger) index;
- (void) drawRect: (NSRect) dirtyRect;
- (void) drawGhostForHorizontalContent: (NSRect) frame;
- (void) drawGhostForVerticalContent: (NSRect) frame;
- (void) drawDividerForHorizontalContentInFrame: (NSRect) dividerFrame;
- (void) drawDividerForVerticalContentInFrame: (NSRect) dividerFrame
                                     isBottom: (BOOL) isBottom
                                      caption: (NSString *) caption
                                documentTitle: (NSString *) documentTitle;
- (NSView *) hitTest: (NSPoint) point;
- (void) mouseDown: (NSEvent *) event;
- (void) mouseDragged: (NSEvent *) event;
- (void) mouseUp: (NSEvent *) event;
- (void) createGhostWindowWithDividerAt: (NSUInteger) dividerIndex
                                   axis: (enum SplitAxis) dividerAxis;
- (void) cleanupGhostWindow;
- (void) adjustSubviews;
- (void) adjustSubviewsUncommittedAxis;
- (void) adjustSubviewsHorizontalAxis;
- (void) adjustSubviewsVerticalAxis;
- (void) adjustSubviewsToEqualSizes;
- (CGFloat) dividerThicknessForAxis: (enum SplitAxis) dividerAxis;
- (CGFloat) absoluteMinCoordinateOfDividerAt: (NSUInteger) dividerIndex
                                        axis: (enum SplitAxis) dividerAxis;
- (CGFloat) absoluteMaxCoordinateOfDividerAt: (NSUInteger) dividerIndex
                                        axis: (enum SplitAxis) dividerAxis;
- (CGFloat) constrainMinCoordinate: (CGFloat) proposedPosition
                       ofDividerAt: (NSUInteger) dividerIndex
                              axis: (enum SplitAxis) dividerAxis;
- (CGFloat) constrainMaxCoordinate: (CGFloat) proposedPosition
                       ofDividerAt: (NSUInteger) dividerIndex
                              axis: (enum SplitAxis) dividerAxis;
- (CGFloat) constrainSplitPosition: (CGFloat) proposedPosition
                       ofDividerAt: (NSUInteger) dividerIndex
                              axis: (enum SplitAxis) dividerAxis;
- (CGFloat) subviewMinimumSizeForAxis: (enum SplitAxis) dividerAxis;
- (CGFloat) subviewCollapseThresholdSizeForAxis: (enum SplitAxis) dividerAxis;
@end
