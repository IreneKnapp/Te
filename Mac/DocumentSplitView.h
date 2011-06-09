#import <Cocoa/Cocoa.h>


@interface DocumentSplitView : NSView
{
}

- (id) initWithFrame: (NSRect) frame;
- (void) drawRect: (NSRect) dirtyRect;
- (void) enumerateDividerRectangles: (void (^)(NSRect rect)) block;
- (void) adjustSubviews;
- (CGFloat) dividerThickness;
- (CGFloat) constrainMinCoordinate: (CGFloat) proposedMin
                       ofDividerAt: (NSUInteger) dividerIndex;
- (CGFloat) constrainMaxCoordinate: (CGFloat) proposedMax
                       ofDividerAt: (NSUInteger) dividerIndex;
- (CGFloat) constrainSplitPosition: (CGFloat) proposedPosition
                       ofDividerAt: (NSUInteger) dividerIndex;
@end
