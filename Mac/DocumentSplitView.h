#import <Cocoa/Cocoa.h>
#import "SizeConstraintParticipant.h"


enum SplitAxis {
    UncommittedSplitAxis,
    VerticalSplitAxis,
    HorizontalSplitAxis
};

@class DocumentContentView;
@class DocumentWindow;
@class TransparentHelperWindow;
@interface DocumentSplitView : NSView <SizeConstraintParticipant>
{
    NSMutableArray *contentSubviews;
    NSMutableArray *dividerSubviewsForVerticalContent;
    NSMutableArray *dividerSubviewsForHorizontalContent;
    
    enum SplitAxis committedAxis;
    enum SplitAxis enforcedAxis;
    BOOL usingChildWrappers;
    BOOL resizingTipsVisible;
    
    NSMutableDictionary *captionAttributes;
    NSMutableDictionary *titleAttributes;
    NSMutableDictionary *titleUnderprintAttributes;
    NSColor *topBorderColor;
    NSColor *bottomBorderColor;
    NSColor *curvyBorderColor;
    NSGradient *captionGradient;
    NSGradient *titleGradient;
    NSColor *resizeIndicatorDarkColor;
    NSColor *resizeIndicatorLightColor;
    CGFloat baselineOffset;
    CGFloat captionInset;
    CGFloat captionLineHeight;
    CGFloat titleLineHeight;
    
    NSMutableDictionary *inactiveTitleAttributes;
    NSColor *inactiveTopBorderColor;
    NSColor *inactiveBottomBorderColor;
    NSColor *inactiveCurvyBorderColor;
    NSGradient *inactiveTitleGradient;
    NSColor *inactiveResizeIndicatorDarkColor;
    NSColor *inactiveResizeIndicatorLightColor;
    
    BOOL trackingDividerDrag;
    NSUInteger dividerIndexBeingTracked;
    enum SplitAxis dividerAxisBeingTracked;
    NSPoint previousDragPoint;
    BOOL collapsedBefore;
    BOOL collapsedAfter;
    BOOL createdBefore;
    BOOL createdAfter;
    BOOL creatingNewDivider;
    TransparentHelperWindow *ghostWindow;
}
@property (readonly) NSArray *contentSubviews;
@property (readonly) enum SplitAxis committedAxis;

+ (CGFloat) minimumDividerThicknessForAxis: (enum SplitAxis) dividerAxis;
- (id) initWithFrame: (NSRect) frame;
- (id) initWithFrame: (NSRect) frame
        enforcedAxis: (enum SplitAxis) newEnforcedAxis;
- (DocumentWindow *) documentWindow;
- (void) newContentSubviewAtIndex: (NSUInteger) index
         alongAxis: (enum SplitAxis) alongAxis;
- (void) addContentSubview: (NSView *) newChild;
- (void) recreateDividerSubviews;
- (void) removeContentSubviewAtIndex: (NSUInteger) index;
- (void) wrapChildrenWithEnforcedAxis: (enum SplitAxis) childEnforcedAxis;
- (void) unwrapChildren;
- (void) drawRect: (NSRect) dirtyRect;
- (void) drawGhostForHorizontalContent: (NSRect) frame;
- (void) drawGhostForVerticalContent: (NSRect) frame;
- (void) drawDividerForHorizontalContentInFrame: (NSRect) dividerFrame
                                     isLeftmost: (BOOL) isLeftmost;
- (void) drawDividerForVerticalContentInFrame: (NSRect) dividerFrame
                                     isBottom: (BOOL) isBottom
                                   isTopLevel: (BOOL) isTopLevel
                                  activeState: (BOOL) activeState
                                      caption: (NSString *) caption
                                documentTitle: (NSString *) documentTitle;
- (void) drawBottomRightCornerResizeIndicatorActiveState: (BOOL) activeState;
- (void) drawVerticalResizeIndicatorInFrame: (NSRect) frame
                                activeState: (BOOL) activeState;
- (NSView *) hitTest: (NSPoint) point;
- (void) mouseDown: (NSEvent *) event;
- (void) mouseDragged: (NSEvent *) event;
- (void) mouseUp: (NSEvent *) event;
- (void) createGhostWindowWithDividerAt: (NSUInteger) dividerIndex
                                   axis: (enum SplitAxis) dividerAxis;
- (void) cleanupGhostWindow;
- (void) showResizingTips;
- (void) hideResizingTips;
- (void) resizeSubviewsWithOldSize: (NSSize) oldBoundsSize;
- (void) adjustSubviews;
- (void) adjustSubviewsUncommittedAxis;
- (void) adjustSubviewsHorizontalAxis;
- (void) adjustSubviewsVerticalAxis;
- (void) adjustSubviewsToEqualSizes;
- (NSSize) minimumSize;
- (NSSize) desiredSize;
- (NSString *) caption;
- (NSString *) sizeReport;
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
- (BOOL) contentExistsBeforeDividerIndex: (NSUInteger) dividerIndex
                                    axis: (enum SplitAxis) dividerAxis;
- (BOOL) contentExistsAfterDividerIndex: (NSUInteger) dividerIndex
                                   axis: (enum SplitAxis) dividerAxis;
- (NSUInteger) contentIndexBeforeDividerIndex: (NSUInteger) dividerIndex
                                         axis: (enum SplitAxis) dividerAxis;
- (NSUInteger) contentIndexAfterDividerIndex: (NSUInteger) dividerIndex
                                        axis: (enum SplitAxis) dividerAxis;
@end
