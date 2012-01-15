#import "DocumentSplitView.h"
#import <HsFFI.h>
#import "Te/LowLevel/ForeignInterface_stub.h"
#import "AppDelegate.h"
#import "DocumentContentView.h"
#import "DocumentWindow.h"
#import "TransparentHelperWindow.h"
#import "Utilities.h"


@implementation DocumentSplitView
@synthesize contentSubviews;
@synthesize committedAxis;


+ (CGFloat) minimumDividerThicknessForAxis: (enum SplitAxis) dividerAxis {
    if(dividerAxis == HorizontalSplitAxis) {
        return 1.0;
    } else if(dividerAxis == VerticalSplitAxis) {
        return 22.0;
    }
}


- (id) initWithFrame: (NSRect) frame {
    return [self initWithFrame: frame enforcedAxis: UncommittedSplitAxis];
}


- (id) initWithFrame: (NSRect) frame
        enforcedAxis: (enum SplitAxis) newEnforcedAxis
{
    self = [super initWithFrame: frame];
    if(self) {
        enforcedAxis = newEnforcedAxis;
        committedAxis = enforcedAxis;
        usingChildWrappers = NO;
        resizingTipsVisible = NO;
        
        trackingDividerDrag = NO;
        contentSubviews = [NSMutableArray arrayWithCapacity: 16];
        dividerSubviewsForHorizontalContent
            = [NSMutableArray arrayWithCapacity: 16];
        dividerSubviewsForVerticalContent
            = [NSMutableArray arrayWithCapacity: 16];
        ghostWindow = nil;
    }
    return self;
}


- (DocumentWindow *) documentWindow {
    NSWindow *window = [self window];
    if(window) {
        id delegate = [window delegate];
        if(delegate && [delegate isKindOfClass: [DocumentWindow class]]) {
            DocumentWindow *documentWindow = (DocumentWindow *) delegate;
            return documentWindow;
        } else {
            return nil;
        }
    } else {
        return nil;
    }
}


- (void) newContentSubviewAtIndex: (NSUInteger) index
         alongAxis: (enum SplitAxis) alongAxis
{
    if(!usingChildWrappers
       && (committedAxis == UncommittedSplitAxis)
       && (alongAxis != UncommittedSplitAxis))
    {
        if(alongAxis == HorizontalSplitAxis) {
            [dividerSubviewsForVerticalContent removeAllObjects];
        } else if(alongAxis == VerticalSplitAxis) {
            [dividerSubviewsForHorizontalContent removeAllObjects];
        }
        
        committedAxis = alongAxis;
        
        enum SplitAxis otherAxis;
        if(committedAxis == HorizontalSplitAxis) {
            otherAxis = VerticalSplitAxis;
        } else if(committedAxis == VerticalSplitAxis) {
            otherAxis = HorizontalSplitAxis;
        }
        
        [self wrapChildrenWithEnforcedAxis: otherAxis];
    }
    
    NSRect initialFrame;
    BOOL setToZeroRectAfterward;
    if([contentSubviews count] == 0) {
        initialFrame = [self bounds];
        setToZeroRectAfterward = NO;
    } else if(index == 0) {
        initialFrame = [[contentSubviews objectAtIndex: 0] frame];
        setToZeroRectAfterward = YES;
    } else {
        initialFrame = [[contentSubviews objectAtIndex: index - 1] frame];
        setToZeroRectAfterward = YES;
    }
    
    if(!usingChildWrappers) {
        DocumentContentView *newContentSubview
            = [[DocumentContentView alloc] initWithFrame: initialFrame];
        if(setToZeroRectAfterward)
            [newContentSubview setFrame: NSZeroRect];
        
        [contentSubviews insertObject: newContentSubview atIndex: index];
        [self addSubview: newContentSubview];
    } else {
        enum SplitAxis otherAxis;
        if(committedAxis == HorizontalSplitAxis) {
            otherAxis = VerticalSplitAxis;
        } else if(committedAxis == VerticalSplitAxis) {
            otherAxis = HorizontalSplitAxis;
        }
        
        DocumentSplitView *wrapper
            = [[DocumentSplitView alloc] initWithFrame: initialFrame
                                         enforcedAxis: otherAxis];
        [wrapper newContentSubviewAtIndex: 0 alongAxis: otherAxis];
        if(setToZeroRectAfterward)
            [wrapper setFrame: NSZeroRect];
        
        [contentSubviews insertObject: wrapper atIndex: index];
        [self addSubview: wrapper];
        
        if(resizingTipsVisible) {
            [wrapper showResizingTips];
        }
    }
    
    if((alongAxis == UncommittedSplitAxis)
       || (alongAxis == HorizontalSplitAxis))
    {
        NSView *newDividerSubview = [[NSView alloc] initWithFrame: NSZeroRect];
        [dividerSubviewsForHorizontalContent insertObject: newDividerSubview
                                             atIndex: index];
        [self addSubview: newDividerSubview];
    }
    
    if((alongAxis == UncommittedSplitAxis)
       || (alongAxis == VerticalSplitAxis))
    {
        NSView *newDividerSubview = [[NSView alloc] initWithFrame: NSZeroRect];
        [dividerSubviewsForVerticalContent insertObject: newDividerSubview
                                           atIndex: index];
        [self addSubview: newDividerSubview];
    }
}


- (void) removeContentSubviewAtIndex: (NSUInteger) index {
    [contentSubviews removeObjectAtIndex: index];
    if(committedAxis == UncommittedSplitAxis) {
        [dividerSubviewsForHorizontalContent removeObjectAtIndex: index];
        [dividerSubviewsForVerticalContent removeObjectAtIndex: index];
    } else if(committedAxis == HorizontalSplitAxis) {
        [dividerSubviewsForHorizontalContent removeObjectAtIndex: index];
    } else if(committedAxis == VerticalSplitAxis) {
        [dividerSubviewsForVerticalContent removeObjectAtIndex: index];
        index--;
    }
    
    if([contentSubviews count] < 2) {
        if((enforcedAxis == UncommittedSplitAxis)
           && (committedAxis != UncommittedSplitAxis))
        {
            NSView *newDividerSubview
                = [[NSView alloc] initWithFrame: NSZeroRect];
            if(committedAxis == HorizontalSplitAxis) {
                [dividerSubviewsForVerticalContent
                  insertObject: newDividerSubview
                  atIndex: index];
            } else if(committedAxis == VerticalSplitAxis) {
                [dividerSubviewsForHorizontalContent
                  insertObject: newDividerSubview
                  atIndex: index];
            }
            [self addSubview: newDividerSubview];
        }
        
        committedAxis = enforcedAxis;
        
        [self unwrapChildren];
    }
}


- (void) addContentSubview: (NSView *) newChild {
    [contentSubviews addObject: newChild];
    [self addSubview: newChild];
}


- (void) recreateDividerSubviews {
    [dividerSubviewsForHorizontalContent removeAllObjects];
    [dividerSubviewsForVerticalContent removeAllObjects];
    
    if(committedAxis == UncommittedSplitAxis) {
        NSView *newHorizontalDividerSubview
            = [[NSView alloc] initWithFrame: NSZeroRect];
        [dividerSubviewsForHorizontalContent
          insertObject: newHorizontalDividerSubview
          atIndex: 0];
        
        NSView *newVerticalDividerSubview
            = [[NSView alloc] initWithFrame: NSZeroRect];
        [dividerSubviewsForVerticalContent
          insertObject: newVerticalDividerSubview
          atIndex: 0];
    } else if(committedAxis == HorizontalSplitAxis) {
        NSUInteger count = [contentSubviews count];
        for(NSUInteger i = 0; i < count; i++) {
            NSView *newDividerSubview
                = [[NSView alloc] initWithFrame: NSZeroRect];
            [dividerSubviewsForHorizontalContent addObject: newDividerSubview];
        }
    } else if(committedAxis == VerticalSplitAxis) {
        NSUInteger count = [contentSubviews count];
        for(NSUInteger i = 0; i < count; i++) {
            NSView *newDividerSubview
                = [[NSView alloc] initWithFrame: NSZeroRect];
            [dividerSubviewsForVerticalContent addObject: newDividerSubview];
        }
    }
}


- (void) wrapChildrenWithEnforcedAxis: (enum SplitAxis) childEnforcedAxis {
    if(usingChildWrappers)
        return;
    
    NSArray *children = [contentSubviews copyWithZone: nil];
    
    [contentSubviews removeAllObjects];
    
    for(NSView *child in children) {
        [child removeFromSuperview];
        
        DocumentSplitView *wrapper
            = [[DocumentSplitView alloc] initWithFrame: [child frame]
                                         enforcedAxis: childEnforcedAxis];
        
        [wrapper addContentSubview: child];
        [self addSubview: wrapper];
        [wrapper recreateDividerSubviews];
        
        [contentSubviews addObject: wrapper];
    }
    
    [self adjustSubviews];
    for(DocumentSplitView *wrapper in contentSubviews) {
        [wrapper adjustSubviews];
    }
    
    usingChildWrappers = YES;
}


- (void) unwrapChildren {
    if([contentSubviews count] > 1)
        return;
    
    if(!usingChildWrappers)
        return;
    
    NSLog(@"You're on your own!");
    /*
    DocumentSplitView *wrapper = [contentSubviews objectAtIndex: 0];
    
    [contentSubviews removeAllObjects];
    
    [dividerSubviewsForVerticalContent removeAllObjects];
    [dividerSubviewsForHorizontalContent removeAllObjects];
    
    NSView *children = [wrapper contentSubviews];
    
    for(NSView *child in children) {
        [child removeFromSuperview];
        
        NSPoint originInWrapper = [child frame].origin;
        NSPoint originInBase = [wrapper convertPointToBase: originInWrapper];
        NSPoint originInSelf = [self convertPointFromBase: originInBase];
        
        [child setFrameOrigin: originInSelf];
        
        [self addSubview: child];
        
        [contentSubviews addObject: child];
        
        
    }
    
    [wrapper removeFromSuperview];
    usingChildWrappers = NO;
    */
}


- (void) drawRect: (NSRect) dirtyRect {
    BOOL isTopLevel = NO;
    NSWindow *window = [self window];
    if(window && [[self superview] isEqual: [window contentView]])
        isTopLevel = YES;
    
    BOOL activeState = YES;
    if(window && ![window isMainWindow])
        activeState = NO;
    
    NSUInteger nDividersForHorizontalContent
        = [dividerSubviewsForHorizontalContent count];
    for(NSUInteger i = 0; i < nDividersForHorizontalContent; i++) {
        NSRect dividerFrame
            = [[dividerSubviewsForHorizontalContent objectAtIndex: i] frame];
        BOOL isLeftmost = i == 0;
        
        [self drawDividerForHorizontalContentInFrame: dividerFrame
              isLeftmost: isLeftmost
              activeState: activeState];
    }
    
    NSUInteger nDividersForVerticalContent
        = [dividerSubviewsForVerticalContent count];
    for(NSUInteger i = 0; i < nDividersForVerticalContent; i++) {        
        NSRect dividerFrame
            = [[dividerSubviewsForVerticalContent objectAtIndex: i] frame];
        id <SizeConstraintParticipant> contentSubviewAbove
            = [contentSubviews objectAtIndex: i];
        BOOL isBottom = i + 1 == nDividersForVerticalContent;
        
        if(trackingDividerDrag
           && !creatingNewDivider
           && collapsedAfter
           && (dividerAxisBeingTracked == VerticalSplitAxis)
           && (i == dividerIndexBeingTracked + 1))
        {
            contentSubviewAbove = [contentSubviews objectAtIndex: i - 1];
        }
        
        NSString *caption;
        if(!resizingTipsVisible) {
            caption = [contentSubviewAbove caption];
        } else {
            caption = [contentSubviewAbove sizeReport];
        }
        
        [self drawDividerForVerticalContentInFrame: dividerFrame
              isBottom: isBottom
              isTopLevel: isTopLevel
              activeState: activeState
              caption: caption
              documentTitle: @"Document Title"];
    }
}


- (void) drawGhostForHorizontalContent: (NSRect) frame {
    [self drawDividerForHorizontalContentInFrame: frame
          isLeftmost: NO
          activeState: YES];
}


- (void) drawGhostForVerticalContent: (NSRect) frame {
    [self drawDividerForVerticalContentInFrame: frame
          isBottom: NO
          isTopLevel: NO
          activeState: YES
          caption: nil
          documentTitle: nil];
}


- (void) drawDividerForHorizontalContentInFrame: (NSRect) dividerFrame
                                     isLeftmost: (BOOL) isLeftmost
                                    activeState: (BOOL) activeState
{
    if(isLeftmost) {
        [[NSColor whiteColor] set];
    } else if(activeState) {
        [[NSColor colorWithDeviceWhite: 0.25 alpha: 1.0] set];
    } else {
        [[NSColor colorWithDeviceWhite: 0.50 alpha: 1.0] set];
    }
    [NSBezierPath fillRect: dividerFrame];
}


- (NSView *) hitTest: (NSPoint) point {
    NSView *superResult = [super hitTest: point];
    if([dividerSubviewsForHorizontalContent containsObject: superResult]
       || [dividerSubviewsForVerticalContent containsObject: superResult])
    {
        return self;
    } else if([superResult isKindOfClass: [DocumentContentView class]]) {
        NSPoint basePoint = [self convertPointToBase: point];
        NSPoint contentPoint = [superResult convertPointFromBase: basePoint];
        CGFloat lineNumberAreaWidth
            = [DocumentContentView lineNumberAreaWidth];
        if(contentPoint.x < lineNumberAreaWidth) {
            return self;
        } else {
            return superResult;
        }
    } else {
        return superResult;
    }
}


- (void) mouseDown: (NSEvent *) event {
    NSPoint location = [self convertPoint: [event locationInWindow]
                             fromView: nil];
        
    CGFloat lineNumberAreaWidth = [DocumentContentView lineNumberAreaWidth];
    
    BOOL found = NO;
    NSUInteger foundIndex;
    enum SplitAxis foundAxis;
    
    {
        NSUInteger nDividersForHorizontalContent
            = [dividerSubviewsForHorizontalContent count];
        for(NSUInteger i = 0; i < nDividersForHorizontalContent; i++) {
            NSView *dividerSubview 
                = [dividerSubviewsForHorizontalContent objectAtIndex: i];
            NSRect dividerFrame = [dividerSubview frame];
            dividerFrame.size.width += lineNumberAreaWidth;
            if(NSPointInRect(location, dividerFrame)) {
                found = YES;
                foundIndex = i;
                foundAxis = HorizontalSplitAxis;
                break;
            }
        }
    }
    
    if(!found) {
        NSUInteger nDividersForVerticalContent
            = [dividerSubviewsForVerticalContent count];
        for(NSUInteger i = 0; i < nDividersForVerticalContent; i++) {
            NSView *dividerSubview
                = [dividerSubviewsForVerticalContent objectAtIndex: i];
            NSRect dividerFrame = [dividerSubview frame];
            if(NSPointInRect(location, dividerFrame)) {
                found = YES;
                foundIndex = i;
                foundAxis = VerticalSplitAxis;
                break;
            }
        }
    }
    
    if(found) {
        trackingDividerDrag = YES;
        dividerIndexBeingTracked = foundIndex;
        dividerAxisBeingTracked = foundAxis;
        previousDragPoint = location;
        createdBefore = NO;
        createdAfter = NO;
        
        BOOL isTerminalDivider;
        if(foundAxis == HorizontalSplitAxis) {
            isTerminalDivider = foundIndex == 0;
        } else if(foundAxis == VerticalSplitAxis) {
            NSUInteger nDividers = [dividerSubviewsForVerticalContent count];
            isTerminalDivider = foundIndex + 1 == nDividers;
        }
        
        BOOL optionDown;
        if([event modifierFlags] & NSAlternateKeyMask)
            optionDown = YES;
        else
            optionDown = NO;
        
        DocumentWindow *documentWindow = [self documentWindow];
        if(documentWindow)
            [documentWindow showResizingTips];
        
        if(isTerminalDivider || optionDown) {
            creatingNewDivider = YES;
            
            enum MouseTrackingAxes trackingAxis;
            if(foundAxis == HorizontalSplitAxis) {
                trackingAxis = TrackMouseHorizontalAxis;
            } else if(foundAxis == VerticalSplitAxis) {
                trackingAxis = TrackMouseVerticalAxis;
            }
            
            [self createGhostWindowWithDividerAt: foundIndex
                                            axis: foundAxis];
            [ghostWindow startTrackingMouse: [event locationInWindow]
                         onAxes: trackingAxis];
        } else {
            creatingNewDivider = NO;
        }
    }
}


- (void) mouseDragged: (NSEvent *) event {
    if(!trackingDividerDrag)
        return;
    
    if(ghostWindow) {
        [ghostWindow updateMouse: [event locationInWindow]];
    }
    
    BOOL contentExistsBefore
        = [self contentExistsBeforeDividerIndex: dividerIndexBeingTracked
                                           axis: dividerAxisBeingTracked];
    BOOL contentExistsAfter
        = [self contentExistsAfterDividerIndex: dividerIndexBeingTracked
                                          axis: dividerAxisBeingTracked];
    NSUInteger contentIndexBefore
        = [self contentIndexBeforeDividerIndex: dividerIndexBeingTracked
                                          axis: dividerAxisBeingTracked];
    NSUInteger contentIndexAfter
        = [self contentIndexAfterDividerIndex: dividerIndexBeingTracked
                                         axis: dividerAxisBeingTracked];
    
    NSView *subviewBefore = nil;
    if(contentExistsBefore)
        subviewBefore = [contentSubviews objectAtIndex: contentIndexBefore];
    NSView *subviewAfter = nil;
    if(contentExistsAfter)
        subviewAfter = [contentSubviews objectAtIndex: contentIndexAfter];
    NSView *dividerSubview;
    if(dividerAxisBeingTracked == HorizontalSplitAxis) {
        dividerSubview
            = [dividerSubviewsForHorizontalContent
                objectAtIndex: dividerIndexBeingTracked];
    } else if(dividerAxisBeingTracked == VerticalSplitAxis) {
        dividerSubview
            = [dividerSubviewsForVerticalContent
                objectAtIndex: dividerIndexBeingTracked];
    }
    
    NSPoint location = [self convertPoint: [event locationInWindow]
                             fromView: nil];
    CGFloat offset;
    if(dividerAxisBeingTracked == HorizontalSplitAxis) {
        offset = location.x - previousDragPoint.x;
    } else if(dividerAxisBeingTracked == VerticalSplitAxis) {
        offset = location.y - previousDragPoint.y;
    }
    
    NSRect placeholderFrame;
    if(dividerAxisBeingTracked == HorizontalSplitAxis) {
        placeholderFrame
            = NSMakeRect(0.0, 0.0, 0.0, [self bounds].size.height);
    } else if(dividerAxisBeingTracked == VerticalSplitAxis) {
        placeholderFrame
            = NSMakeRect(0.0, 0.0, [self bounds].size.width, 0.0);
    }
    
    NSRect frameBefore;
    if(subviewBefore) {
        frameBefore = [subviewBefore frame];
    } else {
        frameBefore = placeholderFrame;
    }
    
    NSRect frameAfter;
    if(subviewAfter) {
        frameAfter = [subviewAfter frame];
    } else {
        frameAfter = placeholderFrame;
    }
    
    CGFloat oldPosition;
    if(dividerAxisBeingTracked == HorizontalSplitAxis) {
        if(subviewBefore) {
            oldPosition = frameBefore.origin.x + frameBefore.size.width;
        } else {
            oldPosition = 0.0;
        }
    } else if(dividerAxisBeingTracked == VerticalSplitAxis) {
        if(subviewAfter) {
            oldPosition = frameAfter.origin.y + frameAfter.size.height;
        } else {
            oldPosition = 0.0;
        }
    }
    
    CGFloat proposedNewPosition = oldPosition + offset;
    
    CGFloat constrainedNewPosition
        = [self constrainSplitPosition: proposedNewPosition
                ofDividerAt: dividerIndexBeingTracked
                axis: dividerAxisBeingTracked];
    
    CGFloat minConstrainedPosition
        = [self constrainMinCoordinate: constrainedNewPosition
                ofDividerAt: dividerIndexBeingTracked
                axis: dividerAxisBeingTracked];
    if(constrainedNewPosition < minConstrainedPosition)
        constrainedNewPosition = minConstrainedPosition;
    
    CGFloat maxConstrainedPosition
        = [self constrainMaxCoordinate: constrainedNewPosition
                ofDividerAt: dividerIndexBeingTracked
                axis: dividerAxisBeingTracked];
    if(constrainedNewPosition > maxConstrainedPosition)
        constrainedNewPosition = maxConstrainedPosition;
    
    CGFloat dividerThickness
        = [self dividerThicknessForAxis: dividerAxisBeingTracked];
    CGFloat absoluteMax
        = [self absoluteMaxCoordinateOfDividerAt: dividerIndexBeingTracked
                                            axis: dividerAxisBeingTracked];
    CGFloat absoluteMin
        = [self absoluteMinCoordinateOfDividerAt: dividerIndexBeingTracked
                                            axis: dividerAxisBeingTracked];
    
    CGFloat proposedEdgeBefore;
    CGFloat proposedSizeBefore;
    CGFloat constrainedEdgeBefore;
    CGFloat constrainedSizeBefore;
    CGFloat proposedEdgeAfter;
    CGFloat proposedSizeAfter;
    CGFloat constrainedEdgeAfter;
    CGFloat constrainedSizeAfter;
    if(dividerAxisBeingTracked == HorizontalSplitAxis) {
        proposedEdgeBefore = proposedNewPosition;
        proposedSizeBefore = proposedEdgeBefore - absoluteMin;
        
        constrainedEdgeBefore = constrainedNewPosition;
        constrainedSizeBefore = constrainedEdgeBefore - absoluteMin;
        
        proposedEdgeAfter = proposedNewPosition + dividerThickness;
        proposedSizeAfter
            = absoluteMax + dividerThickness - proposedEdgeAfter;
        
        constrainedEdgeAfter = constrainedNewPosition + dividerThickness;
        constrainedSizeAfter
            = absoluteMax + dividerThickness - constrainedEdgeAfter;
    } else if(dividerAxisBeingTracked == VerticalSplitAxis) {
        proposedEdgeBefore = proposedNewPosition + dividerThickness;
        proposedSizeBefore
            = absoluteMax + dividerThickness - proposedEdgeBefore;
        
        constrainedEdgeBefore = constrainedNewPosition + dividerThickness;
        constrainedSizeBefore
            = absoluteMax + dividerThickness - constrainedEdgeBefore;
         
        proposedEdgeAfter = proposedNewPosition;
        proposedSizeAfter = proposedEdgeAfter - absoluteMin;
        
        constrainedEdgeAfter = constrainedNewPosition;
        constrainedSizeAfter = constrainedEdgeAfter - absoluteMin;
    }
    
    collapsedBefore = NO;
    collapsedAfter = NO;
    
    if(!creatingNewDivider) {
        CGFloat actualNewPosition = constrainedNewPosition;
        
        CGFloat subviewCollapseThresholdSize
            = [self subviewCollapseThresholdSizeForAxis:
                     dividerAxisBeingTracked];
        
        {
            if(proposedSizeBefore < subviewCollapseThresholdSize) {
                if(dividerAxisBeingTracked == HorizontalSplitAxis) {
                    actualNewPosition = absoluteMin;
                } else if(dividerAxisBeingTracked == VerticalSplitAxis) {
                    actualNewPosition = absoluteMax + dividerThickness;
                }
                collapsedBefore = YES;
            }
        }
        
        if(!collapsedBefore) {
            if(proposedSizeAfter < subviewCollapseThresholdSize) {
                if(dividerAxisBeingTracked == HorizontalSplitAxis) {
                    actualNewPosition = absoluteMax + dividerThickness;
                } else if(dividerAxisBeingTracked == VerticalSplitAxis) {
                    actualNewPosition = absoluteMin;
                }
                collapsedAfter = YES;
            }
        }
        
        CGFloat effectiveDividerThickness;
        if(collapsedBefore || collapsedAfter) {
            effectiveDividerThickness = 0.0;
        } else {
            effectiveDividerThickness = dividerThickness;
        }
        
        NSRect dividerFrame = [dividerSubview frame];
        
        if(dividerAxisBeingTracked == HorizontalSplitAxis) {
            CGFloat actualEdgeBefore = actualNewPosition;
            CGFloat actualSizeBefore = actualEdgeBefore - absoluteMin;
            
            CGFloat actualEdgeAfter
                = actualNewPosition + effectiveDividerThickness;
            CGFloat actualSizeAfter
                = absoluteMax + dividerThickness - actualEdgeAfter;
            
            frameBefore.size.width = actualSizeBefore;
            frameAfter.origin.x = actualEdgeAfter;
            frameAfter.size.width = actualSizeAfter;
            dividerFrame.origin.x = actualEdgeBefore;
            dividerFrame.size.width = actualEdgeAfter - actualEdgeBefore;
        } else if(dividerAxisBeingTracked == VerticalSplitAxis) {
            CGFloat actualEdgeBefore
                = actualNewPosition + effectiveDividerThickness;
            CGFloat actualSizeBefore
                = absoluteMax + dividerThickness - actualEdgeBefore;
            
            CGFloat actualEdgeAfter = actualNewPosition;
            CGFloat actualSizeAfter = actualEdgeAfter - absoluteMin;
            
            frameBefore.origin.y = actualEdgeBefore;
            frameBefore.size.height = actualSizeBefore;
            frameAfter.size.height = actualSizeAfter;
            dividerFrame.origin.y = actualEdgeAfter;
            dividerFrame.size.height = actualEdgeBefore - actualEdgeAfter;
        }
        
        if(subviewBefore)
            [subviewBefore setFrame: frameBefore];
        if(subviewAfter)
            [subviewAfter setFrame: frameAfter];
        [dividerSubview setFrame: dividerFrame];
        
        DocumentWindow *documentWindow = [self documentWindow];
        if(documentWindow)
            [documentWindow updateResizingTips];
        
        if(dividerAxisBeingTracked == HorizontalSplitAxis) {
            previousDragPoint.y = location.y;
            if(actualNewPosition != oldPosition) {
                previousDragPoint.x += actualNewPosition - oldPosition;
            }
        } else if(dividerAxisBeingTracked == VerticalSplitAxis) {
            previousDragPoint.x = location.x;
            if(actualNewPosition != oldPosition) {
                previousDragPoint.y += actualNewPosition - oldPosition;
            }
        }
    } else {
        CGFloat subviewMinimumSize
            = [self subviewMinimumSizeForAxis: dividerAxisBeingTracked];
        
        CGFloat semiConstrainedNewPosition
            = [self constrainSplitPosition: proposedNewPosition
                    ofDividerAt: dividerIndexBeingTracked
                    axis: dividerAxisBeingTracked];
        
        CGFloat semiConstrainedEdgeBefore;
        CGFloat semiConstrainedSizeBefore;
        CGFloat semiConstrainedEdgeAfter;
        CGFloat semiConstrainedSizeAfter;
        if(dividerAxisBeingTracked == HorizontalSplitAxis) {
            semiConstrainedEdgeBefore = semiConstrainedNewPosition;
            semiConstrainedSizeBefore
                = semiConstrainedEdgeBefore - absoluteMin;
            semiConstrainedEdgeAfter
                = semiConstrainedNewPosition + dividerThickness;
            semiConstrainedSizeAfter
                = absoluteMax - semiConstrainedEdgeAfter;
        } else if(dividerAxisBeingTracked == VerticalSplitAxis) {
            semiConstrainedEdgeBefore
                = semiConstrainedNewPosition + dividerThickness;
            semiConstrainedSizeBefore
                = absoluteMax - semiConstrainedEdgeBefore;
            semiConstrainedEdgeAfter = semiConstrainedNewPosition;
            semiConstrainedSizeAfter
                = semiConstrainedEdgeAfter - absoluteMin;
        }
                
        {
            CGFloat thresholdSizeBefore;
            if(dividerAxisBeingTracked == HorizontalSplitAxis) {
                thresholdSizeBefore
                    = frameBefore.size.width - subviewMinimumSize;
            } else if(dividerAxisBeingTracked == VerticalSplitAxis) {
                thresholdSizeBefore
                    = frameBefore.size.height
                      - (subviewMinimumSize + dividerThickness);
            }
            
            if(proposedSizeBefore <= thresholdSizeBefore) {                
                [self cleanupGhostWindow];
                                
                NSUInteger contentIndexForNew;
                if(dividerAxisBeingTracked == HorizontalSplitAxis) {
                    contentIndexForNew = contentIndexBefore;
                } else if(dividerAxisBeingTracked == VerticalSplitAxis) {
                    contentIndexForNew = contentIndexAfter;
                }
                
                [self newContentSubviewAtIndex: contentIndexForNew
                      alongAxis: dividerAxisBeingTracked];
                contentExistsBefore
                    = [self contentExistsBeforeDividerIndex:
                             dividerIndexBeingTracked
                            axis: dividerAxisBeingTracked];
                contentExistsAfter
                    = [self contentExistsAfterDividerIndex:
                             dividerIndexBeingTracked
                            axis: dividerAxisBeingTracked];
                subviewBefore = nil;
                if(contentExistsBefore)
                    subviewBefore
                        = [contentSubviews objectAtIndex: contentIndexBefore];
                subviewAfter = nil;
                if(contentExistsAfter)
                    subviewAfter
                        = [contentSubviews objectAtIndex: contentIndexAfter];
                
                dividerThickness
                    = [self dividerThicknessForAxis: dividerAxisBeingTracked];
                
                NSRect newDividerFrame = [dividerSubview frame];
                
                NSView *dividerSubview;
                if(dividerAxisBeingTracked == HorizontalSplitAxis) {
                    frameAfter = frameBefore;
                    frameAfter.origin.x = semiConstrainedEdgeAfter;
                    frameAfter.size.width = semiConstrainedSizeAfter;
                    
                    frameBefore.size.width = semiConstrainedSizeBefore;
                    
                    newDividerFrame.origin.x
                        = frameAfter.origin.x - dividerThickness;
                    
                    dividerSubview
                        = [dividerSubviewsForHorizontalContent
                            objectAtIndex: dividerIndexBeingTracked + 1];
                } else if(dividerAxisBeingTracked == VerticalSplitAxis) {
                    frameAfter = frameBefore;
                    frameAfter.size.height
                        -= semiConstrainedSizeBefore + dividerThickness;
                    
                    frameBefore.size.height = semiConstrainedSizeBefore;
                    frameBefore.origin.y
                        += frameAfter.size.height + dividerThickness;
                    
                    newDividerFrame.origin.y
                        = frameBefore.origin.y - dividerThickness;
                    
                    dividerSubview
                        = [dividerSubviewsForVerticalContent
                            objectAtIndex: dividerIndexBeingTracked];
                }
                
                if(subviewBefore)
                    [subviewBefore setFrame: frameBefore];
                if(subviewAfter)
                    [subviewAfter setFrame: frameAfter];
                [dividerSubview setFrame: newDividerFrame];
                
                previousDragPoint = location;
                createdBefore = YES;
                creatingNewDivider = NO;
                
                [self adjustSubviews];
            }
        }
        
        if(!createdBefore) {
            CGFloat thresholdSizeAfter;
            if(dividerAxisBeingTracked == HorizontalSplitAxis) {
                thresholdSizeAfter
                    = frameAfter.size.width
                      - (subviewMinimumSize + dividerThickness);
            } else if(dividerAxisBeingTracked == VerticalSplitAxis) {
                thresholdSizeAfter
                    = frameAfter.size.height - subviewMinimumSize;
            }
            
            if(proposedSizeAfter <= thresholdSizeAfter) {
                [self cleanupGhostWindow];
                
                dividerIndexBeingTracked++;
                
                contentIndexBefore
                    = [self contentIndexBeforeDividerIndex:
                             dividerIndexBeingTracked
                            axis: dividerAxisBeingTracked];
                contentIndexAfter
                    = [self contentIndexAfterDividerIndex:
                             dividerIndexBeingTracked
                            axis: dividerAxisBeingTracked];
                
                NSUInteger contentIndexForNew;
                if(dividerAxisBeingTracked == HorizontalSplitAxis) {
                    contentIndexForNew = contentIndexBefore;
                } else if(dividerAxisBeingTracked == VerticalSplitAxis) {
                    contentIndexForNew = contentIndexAfter;
                }
                
                [self newContentSubviewAtIndex: contentIndexForNew
                      alongAxis: dividerAxisBeingTracked];
                
                contentExistsBefore
                    = [self contentExistsBeforeDividerIndex:
                             dividerIndexBeingTracked
                            axis: dividerAxisBeingTracked];
                contentExistsAfter
                    = [self contentExistsAfterDividerIndex:
                             dividerIndexBeingTracked
                            axis: dividerAxisBeingTracked];
                subviewBefore = nil;
                if(contentExistsBefore)
                    subviewBefore
                        = [contentSubviews objectAtIndex: contentIndexBefore];
                subviewAfter = nil;
                if(contentExistsAfter)
                    subviewAfter
                        = [contentSubviews objectAtIndex: contentIndexAfter];
                
                dividerThickness
                    = [self dividerThicknessForAxis: dividerAxisBeingTracked];
                
                NSRect newDividerFrame = [dividerSubview frame];
                
                NSView *dividerSubview;
                if(dividerAxisBeingTracked == HorizontalSplitAxis) {
                    frameBefore = frameAfter;
                    frameBefore.size.width = semiConstrainedSizeBefore;
                    
                    frameAfter.size.width = semiConstrainedSizeAfter;
                    frameAfter.origin.x
                        += frameBefore.size.width + dividerThickness;
                    
                    newDividerFrame.origin.x
                        = frameAfter.origin.x - dividerThickness;
                    
                    dividerSubview
                        = [dividerSubviewsForHorizontalContent
                            objectAtIndex: dividerIndexBeingTracked];
                } else if(dividerAxisBeingTracked == VerticalSplitAxis) {
                    frameBefore = frameAfter;
                    frameBefore.origin.y += semiConstrainedSizeAfter;
                    frameBefore.size.height -= semiConstrainedSizeAfter;
                    
                    frameAfter.size.height = semiConstrainedSizeAfter;
                    
                    newDividerFrame.origin.y
                        = frameBefore.origin.y - dividerThickness;
                    
                    dividerSubview
                        = [dividerSubviewsForVerticalContent
                            objectAtIndex: dividerIndexBeingTracked];
                }
                
                if(subviewBefore)
                    [subviewBefore setFrame: frameBefore];
                if(subviewAfter)
                    [subviewAfter setFrame: frameAfter];
                [dividerSubview setFrame: newDividerFrame];
                
                previousDragPoint = location;
                createdAfter = YES;
                creatingNewDivider = NO;
                
                [self adjustSubviews];
            }
        }
    }
}


- (void) mouseUp: (NSEvent *) event {
    if(!trackingDividerDrag)
        return;
    
    DocumentWindow *documentWindow = [self documentWindow];
    if(documentWindow)
        [documentWindow hideResizingTips];
    
    if(ghostWindow) {
        [self cleanupGhostWindow];
    }
    
    if(!creatingNewDivider) {
        NSUInteger contentSubviewIndex;
        if(collapsedBefore) {
            contentSubviewIndex
                = [self contentIndexBeforeDividerIndex:
                         dividerIndexBeingTracked
                        axis: dividerAxisBeingTracked];
        } else if(collapsedAfter) {
            contentSubviewIndex
                = [self contentIndexAfterDividerIndex:
                         dividerIndexBeingTracked
                        axis: dividerAxisBeingTracked];
        }
        
        if(collapsedBefore || collapsedAfter) {
            [self removeContentSubviewAtIndex: contentSubviewIndex];
        }
        
        if(collapsedBefore || collapsedAfter
           || createdBefore || createdAfter)
        {
            [documentWindow adjustSizePerContentConstraints];
        }
    }
    trackingDividerDrag = NO;
}


- (void) createGhostWindowWithDividerAt: (NSUInteger) dividerIndex
                                   axis: (enum SplitAxis) dividerAxis
{
    [self cleanupGhostWindow];
    
    NSView *dividerSubview;
    if(dividerAxis == HorizontalSplitAxis)
        dividerSubview
            = [dividerSubviewsForHorizontalContent objectAtIndex: dividerIndex];
    else if(dividerAxis == VerticalSplitAxis)
        dividerSubview
            = [dividerSubviewsForVerticalContent objectAtIndex: dividerIndex];
    
    NSRect dividerFrame
        = [self convertRectToBase: [dividerSubview frame]];
    
    void (^drawHelper)(NSRect drawFrame) = nil;
    if(dividerAxis == HorizontalSplitAxis) {
        dividerFrame.size.width += [DocumentContentView leftMarginWidth];
        drawHelper = ^(NSRect drawFrame)
                     {
                         [self drawGhostForHorizontalContent: drawFrame];
                     };
    } else if(dividerAxis == VerticalSplitAxis) {
        drawHelper = ^(NSRect drawFrame)
                     {
                         [self drawGhostForVerticalContent: drawFrame];
                     };
    }
    
    ghostWindow
        = [[TransparentHelperWindow alloc]
            initWithContentRect: dividerFrame
            drawHelper: drawHelper
            aboveWindow: [self window]];
}


- (void) cleanupGhostWindow {
    if(ghostWindow) {
        [ghostWindow remove];
        ghostWindow = nil;
    }
}


- (void) showResizingTips {
    resizingTipsVisible = YES;
    [self setNeedsDisplay: YES];
    
    for(id subview in contentSubviews) {
        if([subview isKindOfClass: [DocumentSplitView class]]) {
            DocumentSplitView *documentSplitView
                = (DocumentSplitView *) subview;
            [documentSplitView showResizingTips];
        }
    }
}


- (void) hideResizingTips {
    resizingTipsVisible = NO;
    [self setNeedsDisplay: YES];
    
    for(id <NSObject, SizeConstraintParticipant> subview in contentSubviews) {
        if([subview isKindOfClass: [DocumentSplitView class]]) {
            DocumentSplitView *documentSplitView
                = (DocumentSplitView *) subview;
            [documentSplitView hideResizingTips];
        }
    }
}


- (void) updateResizingTips {
    [self setNeedsDisplay: YES];
    
    for(id <NSObject, SizeConstraintParticipant> subview in contentSubviews) {
        if([subview isKindOfClass: [DocumentSplitView class]]) {
            DocumentSplitView *documentSplitView
                = (DocumentSplitView *) subview;
            [documentSplitView updateResizingTips];
        }
    }
}


- (void) resizeSubviewsWithOldSize: (NSSize) oldBoundsSize {
    [super resizeSubviewsWithOldSize: oldBoundsSize];
    [self adjustSubviews];
}


- (void) adjustSubviews {
    if(committedAxis == UncommittedSplitAxis) {
        [self adjustSubviewsUncommittedAxis];
    } else if(committedAxis == HorizontalSplitAxis) {
        [self adjustSubviewsHorizontalAxis];
    } else if(committedAxis == VerticalSplitAxis) {
        [self adjustSubviewsVerticalAxis];
    }
    
    [self setNeedsDisplay: YES];
}


- (void) adjustSubviewsUncommittedAxis {
    NSUInteger nSubviews = [contentSubviews count];
    
    if(nSubviews == 1) {
        CGFloat dividerWidth
            = [self dividerThicknessForAxis: HorizontalSplitAxis];
        CGFloat dividerHeight
            = [self dividerThicknessForAxis: VerticalSplitAxis];
        
        NSRect contentFrame = [self bounds];
        contentFrame.origin.y += dividerHeight;
        contentFrame.size.height -= dividerHeight;
        contentFrame.origin.x += dividerWidth;
        contentFrame.size.width -= dividerWidth;
        [[contentSubviews objectAtIndex: 0] setFrame: contentFrame];
        
        NSRect verticalDividerFrame = [self bounds];
        verticalDividerFrame.size.height = dividerHeight;
        verticalDividerFrame.origin.x += dividerWidth;
        verticalDividerFrame.size.width -= dividerWidth;
        [[dividerSubviewsForVerticalContent objectAtIndex: 0]
          setFrame: verticalDividerFrame];
        
        NSRect horizontalDividerFrame = [self bounds];
        horizontalDividerFrame.size.width = dividerWidth;
        horizontalDividerFrame.origin.y += dividerHeight;
        horizontalDividerFrame.size.height -= dividerHeight;
        [[dividerSubviewsForHorizontalContent objectAtIndex: 0]
          setFrame: horizontalDividerFrame];
    }
}


- (void) adjustSubviewsHorizontalAxis {
    NSUInteger nSubviews = [contentSubviews count];
    
    if(!nSubviews)
        return;
    
    CGFloat totalSubviewWidthBefore = 0.0;
    for(NSView *subview in contentSubviews) {
        totalSubviewWidthBefore += [subview frame].size.width;
    }
    
    CGFloat *proportions = malloc(nSubviews * sizeof(CGFloat));
    if(totalSubviewWidthBefore == 0.0) {
        for(NSUInteger i = 0; i < nSubviews; i++) {
            proportions[i] = 1.0 / nSubviews;
        }
    } else {
        for(NSUInteger i = 0; i < nSubviews; i++) {
            NSView *subview = [contentSubviews objectAtIndex: i];
            proportions[i]
                = [subview frame].size.width / totalSubviewWidthBefore;
        }
    }
    
    CGFloat dividerWidth
        = [self dividerThicknessForAxis: HorizontalSplitAxis];
    CGFloat dividerHeight
        = [self dividerThicknessForAxis: VerticalSplitAxis];
    NSRect bounds = [self bounds];
    CGFloat totalSubviewWidthAfter
        = bounds.size.width - nSubviews * dividerWidth;
    
    CGFloat subviewRight = [self bounds].size.width;
    for(NSUInteger nMinusI = 0; nMinusI < nSubviews; nMinusI++) {
        NSUInteger i = nSubviews - nMinusI - 1;
        NSView *subview = [contentSubviews objectAtIndex: i];
        CGFloat subviewWidth = floor(proportions[i] * totalSubviewWidthAfter);
        CGFloat subviewLeft = subviewRight - subviewWidth;
        
        NSRect subviewFrame = bounds;
        subviewFrame.origin.x = subviewLeft;
        subviewFrame.size.width = subviewWidth;
        [subview setFrame: subviewFrame];
        
        CGFloat dividerLeft = subviewLeft - dividerWidth;
        
        dividerLeft = [self constrainSplitPosition: dividerLeft
                            ofDividerAt: i
                            axis: HorizontalSplitAxis];
        
        CGFloat minDividerLeft = [self constrainMinCoordinate: dividerLeft
                                       ofDividerAt: i
                                       axis: HorizontalSplitAxis];
        if(dividerLeft < minDividerLeft)
            dividerLeft = minDividerLeft;
        
        CGFloat maxDividerLeft = [self constrainMaxCoordinate: dividerLeft
                                       ofDividerAt: i
                                       axis: HorizontalSplitAxis];
        if(dividerLeft > maxDividerLeft)
            dividerLeft = maxDividerLeft;
        
        subviewLeft = dividerLeft + dividerWidth;
        subviewWidth = subviewRight - subviewLeft;
        
        subviewFrame.origin.x = subviewLeft;
        subviewFrame.size.width = subviewWidth;
        [subview setFrame: subviewFrame];
        
        subviewRight -= subviewWidth;
        subviewRight -= dividerWidth;
    }
    
    free(proportions);
    
    if(subviewRight > 0.0) {
        NSView *leftmostSubview = [contentSubviews objectAtIndex: 0];
        
        NSRect leftmostSubviewFrame = [leftmostSubview frame];
        if(subviewRight < dividerWidth - 1.0) {
            leftmostSubviewFrame.origin.x -= subviewRight;
            leftmostSubviewFrame.size.width += subviewRight;
        } else {
            leftmostSubviewFrame.origin.x -= dividerWidth;
            leftmostSubviewFrame.size.width += dividerWidth;
        }
        [leftmostSubview setFrame: leftmostSubviewFrame];
    }
    
    for(NSUInteger i = 0; i < nSubviews; i++) {
        NSView *subviewToLeft = nil;
        if(i > 0)
            subviewToLeft = [contentSubviews objectAtIndex: i - 1];
        NSView *subviewToRight = [contentSubviews objectAtIndex: i];
        NSView *dividerSubview
            = [dividerSubviewsForHorizontalContent objectAtIndex: i];
        
        NSRect dividerFrame = bounds;
        if(subviewToLeft) {
            NSRect frameToLeft = [subviewToLeft frame];
            dividerFrame.origin.x
                = frameToLeft.origin.x + frameToLeft.size.width;
        } else {
            dividerFrame.origin.x = 0.0;
        }
        
        NSRect frameToRight = [subviewToRight frame];
        dividerFrame.size.width
            = frameToRight.origin.x - dividerFrame.origin.x;
        
        [dividerSubview setFrame: dividerFrame];
    }
}


- (void) adjustSubviewsVerticalAxis {
    NSUInteger nSubviews = [contentSubviews count];
    
    if(!nSubviews)
        return;
    
    CGFloat totalSubviewHeightBefore = 0.0;
    for(NSView *subview in contentSubviews) {
        totalSubviewHeightBefore += [subview frame].size.height;
    }
    
    CGFloat *proportions = malloc(nSubviews * sizeof(CGFloat));
    if(totalSubviewHeightBefore == 0.0) {
        for(NSUInteger i = 0; i < nSubviews; i++) {
            proportions[i] = 1.0 / nSubviews;
        }
    } else {
        for(NSUInteger i = 0; i < nSubviews; i++) {
            NSView *subview = [contentSubviews objectAtIndex: i];
            proportions[i]
                = [subview frame].size.height / totalSubviewHeightBefore;
        }
    }
    
    CGFloat dividerWidth
        = [self dividerThicknessForAxis: HorizontalSplitAxis];
    CGFloat dividerHeight
        = [self dividerThicknessForAxis: VerticalSplitAxis];
    NSRect bounds = [self bounds];
    CGFloat totalSubviewHeightAfter
        = bounds.size.height - nSubviews * dividerHeight;
    
    CGFloat subviewTop = bounds.size.height;
    for(NSUInteger i = 0; i < nSubviews; i++) {
        NSView *subview = [contentSubviews objectAtIndex: i];
        CGFloat subviewHeight = floor(proportions[i] * totalSubviewHeightAfter);
        CGFloat subviewBottom = subviewTop - subviewHeight;
        
        NSRect subviewFrame = bounds;
        subviewFrame.origin.y = subviewBottom;
        subviewFrame.size.height = subviewHeight;
        [subview setFrame: subviewFrame];
        
        CGFloat dividerBottom = subviewBottom - dividerHeight;
        
        dividerBottom = [self constrainSplitPosition: dividerBottom
                              ofDividerAt: i
                              axis: VerticalSplitAxis];
        
        CGFloat minDividerBottom = [self constrainMinCoordinate: dividerBottom
                                         ofDividerAt: i
                                         axis: VerticalSplitAxis];
        if(dividerBottom < minDividerBottom)
            dividerBottom = minDividerBottom;
        
        CGFloat maxDividerBottom = [self constrainMaxCoordinate: dividerBottom
                                         ofDividerAt: i
                                         axis: VerticalSplitAxis];
        if(dividerBottom > maxDividerBottom)
            dividerBottom = maxDividerBottom;
        
        subviewBottom = dividerBottom + dividerHeight;
        subviewHeight = subviewTop - subviewBottom;
        
        subviewFrame.origin.y = subviewBottom;
        subviewFrame.size.height = subviewHeight;
        [subview setFrame: subviewFrame];
        
        subviewTop -= subviewHeight;
        subviewTop -= dividerHeight;
    }
    
    free(proportions);
    
    if(subviewTop > 0.0) {
        NSView *bottomSubview = [contentSubviews lastObject];
        
        NSRect bottomSubviewFrame = [bottomSubview frame];
        if(subviewTop < dividerHeight - 1.0) {
            bottomSubviewFrame.origin.y -= subviewTop;
            bottomSubviewFrame.size.height += subviewTop;
        } else {
            bottomSubviewFrame.origin.y -= dividerHeight;
            bottomSubviewFrame.size.height += dividerHeight;
        }
        [bottomSubview setFrame: bottomSubviewFrame];
    }
    
    for(NSUInteger i = 0; i < nSubviews; i++) {
        NSView *subviewAbove = [contentSubviews objectAtIndex: i];
        NSView *subviewBelow = nil;
        if(i + 1 < nSubviews)
            subviewBelow = [contentSubviews objectAtIndex: i + 1];
        NSView *dividerSubview
            = [dividerSubviewsForVerticalContent objectAtIndex: i];
        
        NSRect dividerFrame = bounds;
        if(subviewBelow) {
            NSRect frameBelow = [subviewBelow frame];
            dividerFrame.origin.y
                = frameBelow.origin.y + frameBelow.size.height;
        } else {
            dividerFrame.origin.y = 0.0;
        }
        
        NSRect frameAbove = [subviewAbove frame];
        dividerFrame.size.height
            = frameAbove.origin.y - dividerFrame.origin.y;
        
        [dividerSubview setFrame: dividerFrame];
    }
}


- (void) adjustSubviewsToEqualSizes {
    for(NSView *subview in contentSubviews) {
        [subview setFrame: NSZeroRect];
    }
    
    [self adjustSubviews];
}


- (NSSize) minimumSize {
    CGFloat emWidth = [(AppDelegate *) [NSApp delegate] emWidth];
    CGFloat lineHeight = [(AppDelegate *) [NSApp delegate] lineHeight];
    CGFloat dividerHeight
        = [DocumentSplitView minimumDividerThicknessForAxis:
                              VerticalSplitAxis];
    CGFloat dividerWidth
        = [DocumentSplitView minimumDividerThicknessForAxis:
                              HorizontalSplitAxis];
    NSUInteger nDividersForHorizontalContent
        = [dividerSubviewsForHorizontalContent count];
    NSUInteger nDividersForVerticalContent
        = [dividerSubviewsForVerticalContent count];
    
    NSSize minimumSize = NSZeroSize;
    
    if(committedAxis == UncommittedSplitAxis) {
        id <SizeConstraintParticipant> subview
            = [contentSubviews objectAtIndex: 0];
        NSSize subviewMinimumSize = [subview minimumSize];
        minimumSize.width += subviewMinimumSize.width;
        minimumSize.height += subviewMinimumSize.height;
    } else if(committedAxis == HorizontalSplitAxis) {
        for(id <SizeConstraintParticipant> subview in contentSubviews) {
            NSSize subviewMinimumSize = [subview minimumSize];
            minimumSize.width += subviewMinimumSize.width;
            if(minimumSize.height < subviewMinimumSize.height)
                minimumSize.height = subviewMinimumSize.height;
        }
    } else if(committedAxis == VerticalSplitAxis) {
        for(id <SizeConstraintParticipant> subview in contentSubviews) {
            NSSize subviewMinimumSize = [subview minimumSize];
            minimumSize.height += subviewMinimumSize.height;
            if(minimumSize.width < subviewMinimumSize.width)
                minimumSize.width = subviewMinimumSize.width;
        }
    }
    
    minimumSize.width += dividerWidth * nDividersForHorizontalContent;
    minimumSize.height += dividerHeight * nDividersForVerticalContent;
    
    return minimumSize;
}


- (NSSize) desiredSize {
    if(committedAxis == UncommittedSplitAxis) {
        id <SizeConstraintParticipant> child
            = [contentSubviews objectAtIndex: 0];
        NSSize result = [child desiredSize];
        result.width += [self dividerThicknessForAxis: HorizontalSplitAxis];
        result.height += [self dividerThicknessForAxis: VerticalSplitAxis];
        return result;
    } else if(committedAxis == HorizontalSplitAxis) {
        NSSize result = NSZeroSize;
        for(id <SizeConstraintParticipant> child in contentSubviews) {
            NSSize childSize = [child desiredSize];
            result.width += childSize.width;
            if(result.height < childSize.height)
                result.height = childSize.height;
        }
        NSUInteger nChildren = [contentSubviews count];
        result.width
            += [self dividerThicknessForAxis: HorizontalSplitAxis] * nChildren;
        return result;
    } else if(committedAxis == VerticalSplitAxis) {
        NSSize result = NSZeroSize;
        for(id <SizeConstraintParticipant> child in contentSubviews) {
            NSSize childSize = [child desiredSize];
            result.height += childSize.height;
            if(result.width < childSize.width)
                result.width = childSize.width;
        }
        NSUInteger nChildren = [contentSubviews count];
        result.height
            += [self dividerThicknessForAxis: VerticalSplitAxis] * nChildren;
        return result;
    }
}


- (NSString *) caption {
    if([contentSubviews count] > 0) {
        id <SizeConstraintParticipant> subview = [contentSubviews lastObject];
        return [subview caption];
    } else {
        return @"";
    }
}


- (NSString *) sizeReport {
    if([contentSubviews count] > 0) {
        id <SizeConstraintParticipant> subview = [contentSubviews lastObject];
        return [subview sizeReport];
    } else {
        return @"";
    }
}


- (CGFloat) dividerThicknessForAxis: (enum SplitAxis) dividerAxis {
    if(dividerAxis == HorizontalSplitAxis) {
        CGFloat minimumThickness
            = [DocumentSplitView minimumDividerThicknessForAxis: dividerAxis];
        return minimumThickness;
    } else if(dividerAxis == VerticalSplitAxis) {
        CGFloat minimumThickness
            = [DocumentSplitView minimumDividerThicknessForAxis: dividerAxis];
        return minimumThickness;
    }
}


- (CGFloat) absoluteMinCoordinateOfDividerAt: (NSUInteger) dividerIndex
                                        axis: (enum SplitAxis) dividerAxis
{
    if(dividerAxis == HorizontalSplitAxis) {
        if(dividerIndex > 0) {
            NSView *subviewToLeft
                = [contentSubviews objectAtIndex: dividerIndex - 1];
            NSRect frameToLeft = [subviewToLeft frame];
            CGFloat frameToLeftLeft = frameToLeft.origin.x;
            return frameToLeftLeft;
        } else {
            return 0.0;
        }
    } else if(dividerAxis == VerticalSplitAxis) {
        if(dividerIndex + 1 < [contentSubviews count]) {
            NSView *subviewBelow
                = [contentSubviews objectAtIndex: dividerIndex + 1];
            NSRect frameBelow = [subviewBelow frame];
            CGFloat frameBelowBottom = frameBelow.origin.y;
            return frameBelowBottom;
        } else {
            return 0.0;
        }
    }
}


- (CGFloat) absoluteMaxCoordinateOfDividerAt: (NSUInteger) dividerIndex
                                        axis: (enum SplitAxis) dividerAxis
{
    if(dividerAxis == HorizontalSplitAxis) {
        NSView *subviewToRight = [contentSubviews objectAtIndex: dividerIndex];
        NSRect frameToRight = [subviewToRight frame];
        CGFloat dividerThickness = [self dividerThicknessForAxis: dividerAxis];
        CGFloat frameToRightRight
            = frameToRight.origin.x + frameToRight.size.width;
        return frameToRightRight - dividerThickness;
    } else if(dividerAxis == VerticalSplitAxis) {
        NSView *subviewAbove = [contentSubviews objectAtIndex: dividerIndex];
        NSRect frameAbove = [subviewAbove frame];
        CGFloat dividerThickness = [self dividerThicknessForAxis: dividerAxis];
        CGFloat frameAboveTop = frameAbove.origin.y + frameAbove.size.height;
        return frameAboveTop - dividerThickness;
    }
}


- (CGFloat) constrainMinCoordinate: (CGFloat) proposedPosition
                       ofDividerAt: (NSUInteger) dividerIndex
                              axis: (enum SplitAxis) dividerAxis
{
    if(dividerAxis == HorizontalSplitAxis) {
        CGFloat enforcedLeft = 0.0;
        if(dividerIndex > 0) {
            CGFloat absoluteLeft
                = [self absoluteMinCoordinateOfDividerAt: dividerIndex
                        axis: dividerAxis];
            CGFloat minimumWidth
                = [self subviewMinimumSizeForAxis: dividerAxis];
            enforcedLeft = ceil(absoluteLeft + minimumWidth);
        }
        
        if(proposedPosition < enforcedLeft)
            return [self constrainSplitPosition: enforcedLeft
                         ofDividerAt: dividerIndex
                         axis: dividerAxis];
        else
            return proposedPosition;
    } else if(dividerAxis == VerticalSplitAxis) {
        CGFloat enforcedBottom = 0.0;
        if(dividerIndex + 1 < [contentSubviews count]) {
            CGFloat absoluteBottom
                = [self absoluteMinCoordinateOfDividerAt: dividerIndex
                        axis: dividerAxis];
            CGFloat minimumHeight
                = [self subviewMinimumSizeForAxis: dividerAxis];
            enforcedBottom = ceil(absoluteBottom + minimumHeight);
        }
        
        if(proposedPosition < enforcedBottom)
            return [self constrainSplitPosition: enforcedBottom
                         ofDividerAt: dividerIndex
                         axis: dividerAxis];
        else
            return proposedPosition;
    }
}


- (CGFloat) constrainMaxCoordinate: (CGFloat) proposedPosition
                       ofDividerAt: (NSUInteger) dividerIndex
                              axis: (enum SplitAxis) dividerAxis
{
    if(dividerAxis == HorizontalSplitAxis) {
        CGFloat absoluteRight
            = [self absoluteMaxCoordinateOfDividerAt: dividerIndex
                    axis: dividerAxis];
        CGFloat minimumWidth = [self subviewMinimumSizeForAxis: dividerAxis];
        CGFloat enforcedRight = floor(absoluteRight - minimumWidth);
        
        if(proposedPosition > enforcedRight)
            return [self constrainSplitPosition: enforcedRight
                         ofDividerAt: dividerIndex
                         axis: dividerAxis];
        else
            return proposedPosition;
    } else if(dividerAxis == VerticalSplitAxis) {
        CGFloat absoluteTop
            = [self absoluteMaxCoordinateOfDividerAt: dividerIndex
                    axis: dividerAxis];
        CGFloat minimumHeight = [self subviewMinimumSizeForAxis: dividerAxis];
        CGFloat enforcedTop = floor(absoluteTop - minimumHeight);
        
        if(proposedPosition > enforcedTop)
            return [self constrainSplitPosition: enforcedTop
                         ofDividerAt: dividerIndex
                         axis: dividerAxis];
        else
            return proposedPosition;
    }
}


- (CGFloat) constrainSplitPosition: (CGFloat) proposedPosition
                       ofDividerAt: (NSUInteger) dividerIndex
                              axis: (enum SplitAxis) dividerAxis
{
    if(dividerAxis == HorizontalSplitAxis) {
        CGFloat absoluteLeft
            = [self absoluteMinCoordinateOfDividerAt: dividerIndex
                    axis: dividerAxis];
        
        CGFloat emWidth = [(AppDelegate *) [NSApp delegate] emWidth];
        CGFloat proposedWidth = proposedPosition - absoluteLeft;
        CGFloat constrainedWidth = round(proposedWidth / emWidth) * emWidth;
        CGFloat constrainedPosition = ceil(absoluteLeft + constrainedWidth);
        
        return constrainedPosition;
    } else if(dividerAxis == VerticalSplitAxis) {
        CGFloat absoluteTop
            = [self absoluteMaxCoordinateOfDividerAt: dividerIndex
                    axis: dividerAxis];
        
        CGFloat lineHeight = [(AppDelegate *) [NSApp delegate] lineHeight];
        CGFloat proposedHeight = absoluteTop - proposedPosition;
        CGFloat constrainedHeight
            = round(proposedHeight / lineHeight) * lineHeight;
        CGFloat constrainedPosition = floor(absoluteTop - constrainedHeight);
        
        return constrainedPosition;
    }
}


- (CGFloat) subviewMinimumSizeForAxis: (enum SplitAxis) dividerAxis {
    if(dividerAxis == HorizontalSplitAxis) {
        CGFloat emWidth = [(AppDelegate *) [NSApp delegate] emWidth];
        CGFloat width = emWidth * [DocumentContentView minimumColumns];
        width += [DocumentContentView leftMarginWidth];
        width += [DocumentContentView leftPaddingWidth];
        width += [DocumentContentView rightPaddingWidth];
        return width;
    } else if(dividerAxis == VerticalSplitAxis) {
        CGFloat lineHeight = [(AppDelegate *) [NSApp delegate] lineHeight];
        return lineHeight * [DocumentContentView minimumLines];
    }
}


- (CGFloat) subviewCollapseThresholdSizeForAxis: (enum SplitAxis) dividerAxis
{
    if(dividerAxis == HorizontalSplitAxis) {
        CGFloat emWidth = [(AppDelegate *) [NSApp delegate] emWidth];
        return emWidth * [DocumentContentView collapseColumns];
    } else if(dividerAxis == VerticalSplitAxis) {
        CGFloat lineHeight = [(AppDelegate *) [NSApp delegate] lineHeight];
        return lineHeight * [DocumentContentView collapseLines];
    }
}


- (BOOL) contentExistsBeforeDividerIndex: (NSUInteger) dividerIndex
                                    axis: (enum SplitAxis) dividerAxis
{
    if(dividerAxis == HorizontalSplitAxis) {
        return dividerIndex > 0;
    } else if(dividerAxis == VerticalSplitAxis) {
        return YES;
    }
}


- (BOOL) contentExistsAfterDividerIndex: (NSUInteger) dividerIndex
                                   axis: (enum SplitAxis) dividerAxis
{
    if(dividerAxis == HorizontalSplitAxis) {
        return YES;
    } else if(dividerAxis == VerticalSplitAxis) {
        return dividerIndex + 1 < [contentSubviews count];
    }
}


- (NSUInteger) contentIndexBeforeDividerIndex: (NSUInteger) dividerIndex
                                         axis: (enum SplitAxis) dividerAxis
{
    if(dividerAxis == HorizontalSplitAxis) {
        return dividerIndex - 1;
    } else if(dividerAxis == VerticalSplitAxis) {
        return dividerIndex;
    }
}


- (NSUInteger) contentIndexAfterDividerIndex: (NSUInteger) dividerIndex
                                        axis: (enum SplitAxis) dividerAxis
{
    if(dividerAxis == HorizontalSplitAxis) {
        return dividerIndex;
    } else if(dividerAxis == VerticalSplitAxis) {
        return dividerIndex + 1;
    }
}

@end
