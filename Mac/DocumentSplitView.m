#import "DocumentSplitView.h"
#import <HsFFI.h>
#import "Te/ForeignInterface_stub.h"
#import "AppDelegate.h"
#import "DocumentContentView.h"
#import "TransparentHelperWindow.h"
#import "Utilities.h"


@implementation DocumentSplitView
@synthesize contentSubviews;


+ (CGFloat) minimumDividerThicknessForAxis: (enum SplitAxis) dividerAxis {
    if(dividerAxis == HorizontalSplitAxis) {
        return 8.0;
    } else if(dividerAxis == VerticalSplitAxis) {
        return 22.0;
    }
}


- (id) initWithFrame: (NSRect) frame {
    self = [super initWithFrame: frame];
    if(self) {
        trackingDividerDrag = NO;
        contentSubviews = [NSMutableArray arrayWithCapacity: 16];
        dividerSubviewsForHorizontalContent
            = [NSMutableArray arrayWithCapacity: 16];
        dividerSubviewsForVerticalContent
            = [NSMutableArray arrayWithCapacity: 16];
        ghostWindow = nil;

        NSFont *captionFont = [(AppDelegate *) [NSApp delegate] baseFont];
        captionAttributes
            = [NSMutableDictionary dictionaryWithCapacity: 1];
        [captionAttributes setObject: captionFont forKey: NSFontAttributeName];
        
        NSFont *titleFont = [NSFont titleBarFontOfSize: 14.0];
        titleAttributes
            = [NSMutableDictionary dictionaryWithCapacity: 3];
        [titleAttributes setObject: titleFont forKey: NSFontAttributeName];
        NSMutableParagraphStyle *titleParagraphStyle
            = [[NSParagraphStyle defaultParagraphStyle] mutableCopyWithZone: nil];
        [titleParagraphStyle setAlignment: NSCenterTextAlignment];
        [titleAttributes setObject: titleParagraphStyle
                         forKey: NSParagraphStyleAttributeName];
        titleUnderprintAttributes
            = [titleAttributes mutableCopyWithZone: nil];
        [titleUnderprintAttributes
          setObject: [NSColor colorWithDeviceWhite: 0.81 alpha: 1.0]
          forKey: NSForegroundColorAttributeName];
        
        topBorderColor
            = [NSColor colorWithDeviceWhite: 0.89 alpha: 1.0];
        bottomBorderColor
            = [NSColor colorWithDeviceWhite: 0.32 alpha: 1.0];
        NSColor *captionGradientTopColor
            = [NSColor colorWithDeviceWhite: 0.91 alpha: 1.0];
        NSColor *captionGradientBottomColor
            = [NSColor colorWithDeviceWhite: 1.00 alpha: 1.0];
        NSColor *titleGradientTopColor
            = [NSColor colorWithDeviceWhite: 0.81 alpha: 1.0];
        NSColor *titleGradientBottomColor
            = [NSColor colorWithDeviceWhite: 0.66 alpha: 1.0];
        curvyBorderColor
            = [NSColor colorWithDeviceWhite: 0.0 alpha: 0.5];
        captionGradient
            = [[NSGradient alloc] initWithStartingColor: captionGradientTopColor
                                  endingColor: captionGradientBottomColor];
        titleGradient
            = [[NSGradient alloc] initWithStartingColor: titleGradientTopColor
                                  endingColor: titleGradientBottomColor];
        
        baselineOffset = 3.0;
        captionInset = [DocumentContentView leftMarginWidth] + 5.0;
        captionLineHeight = [(AppDelegate *) [NSApp delegate] lineHeight];
        titleLineHeight = [@"M" sizeWithAttributes: titleAttributes].height;
    }
    return self;
}


- (DocumentContentView *) newContentSubviewAtIndex: (NSUInteger) index
                          alongAxis: (enum SplitAxis) alongAxis
{
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
    
    DocumentContentView *newContentSubview
        = [[DocumentContentView alloc] initWithFrame: initialFrame];
    if(setToZeroRectAfterward)
        [newContentSubview setFrame: NSZeroRect];
    
    [contentSubviews insertObject: newContentSubview atIndex: index];
    [self addSubview: newContentSubview];
    
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
    
    return newContentSubview;
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
    }
}


- (void) drawRect: (NSRect) dirtyRect {    
    NSUInteger nDividersForHorizontalContent
        = [dividerSubviewsForHorizontalContent count];
    for(NSUInteger i = 0; i < nDividersForHorizontalContent; i++) {
        NSRect dividerFrame
            = [[dividerSubviewsForHorizontalContent objectAtIndex: i] frame];
        
        [self drawDividerForHorizontalContentInFrame: dividerFrame];
    }
    
    NSUInteger nDividersForVerticalContent
        = [dividerSubviewsForVerticalContent count];
    for(NSUInteger i = 0; i < nDividersForVerticalContent; i++) {
        NSRect dividerFrame
            = [[dividerSubviewsForVerticalContent objectAtIndex: i] frame];
        BOOL isBottom = i + 1 == nDividersForVerticalContent;
        
        [self drawDividerForVerticalContentInFrame: dividerFrame
                                          isBottom: isBottom
                                           caption: @"(12, 13) in 1980"
                                     documentTitle: @"Document Title"];
    }
}


- (void) drawGhostForHorizontalContent: (NSRect) frame {
    [self drawDividerForHorizontalContentInFrame: frame];
}


- (void) drawGhostForVerticalContent: (NSRect) frame {
    [self drawDividerForVerticalContentInFrame: frame
          isBottom: NO
          caption: nil
          documentTitle: nil];
}


- (void) drawDividerForHorizontalContentInFrame: (NSRect) dividerFrame {
}


- (void) drawDividerForVerticalContentInFrame: (NSRect) dividerFrame
                                     isBottom: (BOOL) isBottom
                                      caption: (NSString *) caption
                                documentTitle: (NSString *) documentTitle
{
    NSRect topBorderRect = dividerFrame;
    topBorderRect.origin.y += topBorderRect.size.height - 1.0;
    topBorderRect.size.height = 1.0;
    
    NSRect bottomBorderRect = dividerFrame;
    bottomBorderRect.size.height = 1.0;
    
    CGFloat leftX = dividerFrame.origin.x;
    CGFloat rightX = dividerFrame.origin.x + dividerFrame.size.width;
    
    CGFloat curveMiddleX
        = (dividerFrame.size.width - captionInset) / 4.0
          + dividerFrame.origin.x + captionInset;
    CGFloat curveLeftX = curveMiddleX - 50.0;
    CGFloat curveRightX = curveMiddleX + 50.0;
    CGFloat lowerControlPointX
        = (curveRightX - curveLeftX) * 0.75 + curveLeftX;
    CGFloat upperControlPointX
        = (curveRightX - curveLeftX) * 0.25 + curveLeftX;
    
    CGFloat topY = dividerFrame.origin.y + dividerFrame.size.height;
    CGFloat bottomY = dividerFrame.origin.y;
    
    NSBezierPath *topRegion = [NSBezierPath bezierPath];
    [topRegion moveToPoint: NSMakePoint(leftX, topY)];
    [topRegion lineToPoint: NSMakePoint(leftX, bottomY)];
    [topRegion lineToPoint: NSMakePoint(curveLeftX, bottomY)];
    [topRegion curveToPoint: NSMakePoint(curveRightX, topY)
               controlPoint1: NSMakePoint(lowerControlPointX, bottomY)
               controlPoint2: NSMakePoint(upperControlPointX, topY)];
    [topRegion closePath];
    
    NSBezierPath *bottomRegion = [NSBezierPath bezierPath];
    [bottomRegion moveToPoint: NSMakePoint(rightX, topY)];
    [bottomRegion lineToPoint: NSMakePoint(rightX, bottomY)];
    [bottomRegion lineToPoint: NSMakePoint(curveLeftX, bottomY)];
    [bottomRegion curveToPoint: NSMakePoint(curveRightX, topY)
                  controlPoint1: NSMakePoint(lowerControlPointX, bottomY)
                  controlPoint2: NSMakePoint(upperControlPointX, topY)];
    [bottomRegion closePath];
    
    NSBezierPath *curvyBorder = [NSBezierPath bezierPath];
    [curvyBorder moveToPoint: NSMakePoint(curveLeftX, bottomY)];
    [curvyBorder curveToPoint: NSMakePoint(curveRightX, topY)
                 controlPoint1: NSMakePoint(lowerControlPointX, bottomY)
                 controlPoint2: NSMakePoint(upperControlPointX, topY)];
    [curvyBorder lineToPoint: NSMakePoint(rightX, topY)];
    
    [titleGradient drawInBezierPath: bottomRegion angle: -90.0];
    
    [captionGradient drawInBezierPath: topRegion angle: -90.0];
    
    [topBorderColor set];
    [NSBezierPath fillRect: topBorderRect];
    
    [curvyBorderColor set];
    [curvyBorder stroke];
    
    if(!isBottom) {
        [bottomBorderColor set];
        [NSBezierPath fillRect: bottomBorderRect];
    }
    
    if(caption) {
        NSRect captionRect = dividerFrame;
        captionRect.origin.y += baselineOffset;
        captionRect.size.height = captionLineHeight;
        captionRect.origin.x += captionInset;
        captionRect.size.width -= captionInset;
        
        [caption drawInRect: captionRect
                 withAttributes: captionAttributes];
    }
    
    if(!isBottom && documentTitle) {
        NSRect titleRect = dividerFrame;
        titleRect.origin.y += baselineOffset;
        titleRect.size.height = titleLineHeight;
        
        NSRect titleUnderprintRect = titleRect;
        titleUnderprintRect.origin.y -= 1.0;
        
        [documentTitle drawInRect: titleUnderprintRect
                       withAttributes: titleUnderprintAttributes];
        
        [documentTitle drawInRect: titleRect
                       withAttributes: titleAttributes];
    }
}


- (NSView *) hitTest: (NSPoint) point {
    NSView *superResult = [super hitTest: point];
    if([dividerSubviewsForHorizontalContent containsObject: superResult]
       || [dividerSubviewsForVerticalContent containsObject: superResult])
    {
        return self;
    } else {
        return superResult;
    }
}


- (void) mouseDown: (NSEvent *) event {
    NSPoint location = [self convertPoint: [event locationInWindow]
                             fromView: nil];
    
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
        createdAbove = NO;
        createdBelow = NO;
        
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
    
    NSView *subviewAbove
        = [contentSubviews objectAtIndex: dividerIndexBeingTracked];
    NSView *subviewBelow = nil;
    if(dividerIndexBeingTracked + 1 < [contentSubviews count]) {
        subviewBelow
            = [contentSubviews objectAtIndex: dividerIndexBeingTracked + 1];
    }
    NSView *dividerSubview
        = [dividerSubviewsForVerticalContent
            objectAtIndex: dividerIndexBeingTracked];
    
    NSPoint location = [self convertPoint: [event locationInWindow]
                             fromView: nil];
    CGFloat verticalOffset = location.y - previousDragPoint.y;
    
    CGFloat oldPosition;
    NSRect frameAbove = [subviewAbove frame];
    NSRect frameBelow;
    if(subviewBelow) {
        frameBelow = [subviewBelow frame];
        oldPosition = frameBelow.origin.y + frameBelow.size.height;
    } else {
        frameBelow = NSMakeRect(0.0, 0.0, [self bounds].size.width, 0.0);
        oldPosition = 0.0;
    }
    
    CGFloat proposedNewPosition = oldPosition + verticalOffset;
    
    CGFloat constrainedNewPosition
        = [self constrainSplitPosition: proposedNewPosition
                ofDividerAt: dividerIndexBeingTracked
                axis: VerticalSplitAxis];
    
    CGFloat minConstrainedPosition
        = [self constrainMinCoordinate: constrainedNewPosition
                ofDividerAt: dividerIndexBeingTracked
                axis: VerticalSplitAxis];
    if(constrainedNewPosition < minConstrainedPosition)
        constrainedNewPosition = minConstrainedPosition;
    
    CGFloat maxConstrainedPosition
        = [self constrainMaxCoordinate: constrainedNewPosition
                ofDividerAt: dividerIndexBeingTracked
                axis: VerticalSplitAxis];
    if(constrainedNewPosition > maxConstrainedPosition)
        constrainedNewPosition = maxConstrainedPosition;
    
    CGFloat dividerThickness
        = [self dividerThicknessForAxis: VerticalSplitAxis];
    
    CGFloat absoluteMax
        = [self absoluteMaxCoordinateOfDividerAt: dividerIndexBeingTracked
                                            axis: VerticalSplitAxis];
    
    CGFloat proposedEdgeAbove = proposedNewPosition + dividerThickness;
    CGFloat proposedHeightAbove = absoluteMax - proposedEdgeAbove;
    
    CGFloat constrainedEdgeAbove = constrainedNewPosition + dividerThickness;
    CGFloat constrainedHeightAbove = absoluteMax - constrainedEdgeAbove;
    
    CGFloat absoluteMin
        = [self absoluteMinCoordinateOfDividerAt: dividerIndexBeingTracked
                                            axis: VerticalSplitAxis];
    
    CGFloat proposedEdgeBelow = proposedNewPosition;
    CGFloat proposedHeightBelow = proposedEdgeBelow - absoluteMin;
    
    CGFloat constrainedEdgeBelow = constrainedNewPosition;
    CGFloat constrainedHeightBelow = constrainedEdgeBelow - absoluteMin;
    
    collapsedAbove = NO;
    collapsedBelow = NO;
    
    if(!creatingNewDivider) {
        CGFloat actualNewPosition = constrainedNewPosition;
        
        CGFloat subviewCollapseThresholdSize
            = [self subviewCollapseThresholdSizeForAxis: VerticalSplitAxis];
        
        {
            if(proposedHeightAbove < subviewCollapseThresholdSize) {
                actualNewPosition = absoluteMax + dividerThickness;
                collapsedAbove = YES;
            }
        }
        
        if(!collapsedAbove) {
            if(proposedHeightBelow < subviewCollapseThresholdSize) {
                actualNewPosition = absoluteMin;
                collapsedBelow = YES;
            }
        }
        
        CGFloat effectiveDividerThickness;
        if(collapsedAbove || collapsedBelow) {
            effectiveDividerThickness = 0.0;
        } else {
            effectiveDividerThickness = dividerThickness;
        }
        
        CGFloat actualEdgeAbove = actualNewPosition + effectiveDividerThickness;
        frameAbove.size.height
            = frameAbove.size.height - (actualEdgeAbove - frameAbove.origin.y);
        frameAbove.origin.y = actualEdgeAbove;
        [subviewAbove setFrame: frameAbove];
        
        CGFloat actualEdgeBelow = actualNewPosition;
        frameBelow.size.height = actualEdgeBelow - frameBelow.origin.y;
        if(frameBelow.size.height < 0.0) frameBelow.size.height = 0.0;
        frameBelow.origin.y = actualEdgeBelow - frameBelow.size.height;
        if(frameBelow.origin.y < 0.0) frameBelow.origin.y = 0.0;
        if(subviewBelow)
            [subviewBelow setFrame: frameBelow];
        
        NSRect dividerFrame = [dividerSubview frame];
        dividerFrame.origin.y = actualNewPosition;
        dividerFrame.size.height = effectiveDividerThickness;
        [dividerSubview setFrame: dividerFrame];
        
        previousDragPoint.x = location.x;
        if(actualNewPosition != oldPosition) {
            previousDragPoint.y += actualNewPosition - oldPosition;
        }
    } else {
        CGFloat subviewMinimumSize
            = [self subviewMinimumSizeForAxis: VerticalSplitAxis];
        
        CGFloat semiConstrainedNewPosition
            = [self constrainSplitPosition: proposedNewPosition
                    ofDividerAt: dividerIndexBeingTracked
                    axis: VerticalSplitAxis];
        
        {
            CGFloat thresholdHeightAbove
                = frameAbove.size.height
                  - (subviewMinimumSize + dividerThickness);
            
            if(proposedHeightAbove <= thresholdHeightAbove) {                
                [self cleanupGhostWindow];
                
                CGFloat semiConstrainedEdgeAbove
                    = semiConstrainedNewPosition + dividerThickness;
                CGFloat semiConstrainedHeightAbove
                    = absoluteMax - semiConstrainedEdgeAbove;
                
                subviewBelow = subviewAbove;
                subviewAbove = [self newContentSubviewAtIndex:
                                      dividerIndexBeingTracked
                                     alongAxis: VerticalSplitAxis];
                
                frameBelow = frameAbove;
                frameBelow.size.height
                    -= semiConstrainedHeightAbove + dividerThickness;
                
                frameAbove.size.height = semiConstrainedHeightAbove;
                frameAbove.origin.y
                    += frameBelow.size.height + dividerThickness;
                
                NSRect newDividerFrame = [dividerSubview frame];
                newDividerFrame.origin.y
                    = frameAbove.origin.y - dividerThickness;
                
                dividerSubview
                    = [dividerSubviewsForVerticalContent
                        objectAtIndex: dividerIndexBeingTracked];
                
                [subviewAbove setFrame: frameAbove];
                [subviewBelow setFrame: frameBelow];
                [dividerSubview setFrame: newDividerFrame];
                
                previousDragPoint = location;
                createdAbove = YES;
                creatingNewDivider = NO;
                
                [self adjustSubviews];
            }
        }
        
        if(!createdAbove) {
            CGFloat thresholdHeightBelow
                = frameBelow.size.height - subviewMinimumSize;
            
            if(proposedHeightBelow <= thresholdHeightBelow) {
                [self cleanupGhostWindow];
                
                CGFloat semiConstrainedEdgeBelow
                    = semiConstrainedNewPosition;
                CGFloat semiConstrainedHeightBelow
                    = semiConstrainedEdgeBelow - absoluteMin;
                
                subviewAbove = [self newContentSubviewAtIndex:
                                      dividerIndexBeingTracked + 1
                                     alongAxis: VerticalSplitAxis];
                
                frameAbove = frameBelow;
                frameAbove.origin.y += constrainedHeightBelow;
                frameAbove.size.height -= constrainedHeightBelow;
                
                frameBelow.size.height = constrainedHeightBelow;
                
                NSRect newDividerFrame = [dividerSubview frame];
                newDividerFrame.origin.y
                    = frameAbove.origin.y - dividerThickness;
                
                dividerSubview
                    = [dividerSubviewsForVerticalContent
                        objectAtIndex: dividerIndexBeingTracked + 1];
                
                [subviewAbove setFrame: frameAbove];
                if(subviewBelow)
                    [subviewBelow setFrame: frameBelow];
                [dividerSubview setFrame: newDividerFrame];
                
                previousDragPoint = location;
                dividerIndexBeingTracked++;
                createdBelow = YES;
                creatingNewDivider = NO;
                
                [self adjustSubviews];
            }
        }
    }
}


- (void) mouseUp: (NSEvent *) event {
    if(!trackingDividerDrag)
        return;
    
    if(ghostWindow) {
        [self cleanupGhostWindow];
    }
    
    if(!creatingNewDivider) {
        if(collapsedAbove) {
            [self removeContentSubviewAtIndex: dividerIndexBeingTracked];
        } else if(collapsedBelow) {
            [self removeContentSubviewAtIndex: dividerIndexBeingTracked + 1];
        }
        
        if(collapsedAbove || collapsedBelow || createdAbove || createdBelow) {
            [self adjustSubviews];
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
        [[dividerSubviewsForVerticalContent objectAtIndex: 0]
          setFrame: verticalDividerFrame];
        
        NSRect horizontalDividerFrame = [self bounds];
        horizontalDividerFrame.size.width = dividerWidth;
        [[dividerSubviewsForHorizontalContent objectAtIndex: 0]
          setFrame: horizontalDividerFrame];
    }
}


- (void) adjustSubviewsHorizontalAxis {
    NSUInteger nSubviews = [contentSubviews count];
    
    if(nSubviews < 2)
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
    
    CGFloat dividerThickness
        = [self dividerThicknessForAxis: HorizontalSplitAxis];
    NSRect bounds = [self bounds];
    CGFloat totalSubviewWidthAfter
        = bounds.size.width - nSubviews * dividerThickness;
    
    CGFloat subviewRight = 0.0;
    for(NSUInteger i = nSubviews - 1; i >= 0; i--) {
        NSView *subview = [contentSubviews objectAtIndex: i];
        CGFloat subviewWidth = floor(proportions[i] * totalSubviewWidthAfter);
        CGFloat subviewLeft = subviewRight - subviewWidth;
        
        NSRect subviewFrame = bounds;
        subviewFrame.origin.x = subviewLeft;
        subviewFrame.size.width = subviewWidth;
        [subview setFrame: subviewFrame];
        
        CGFloat dividerLeft = subviewLeft - dividerThickness;
        
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
        
        subviewLeft = dividerLeft + dividerThickness;
        subviewWidth = subviewRight - subviewLeft;
        
        subviewFrame.origin.x = subviewLeft;
        subviewFrame.size.width = subviewWidth;
        [subview setFrame: subviewFrame];
        
        subviewRight -= subviewWidth;
        subviewRight -= dividerThickness;
    }
    
    free(proportions);
    
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
    
    if(nSubviews < 2)
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
    
    CGFloat dividerThickness
        = [self dividerThicknessForAxis: VerticalSplitAxis];
    NSRect bounds = [self bounds];
    CGFloat totalSubviewHeightAfter
        = bounds.size.height - nSubviews * dividerThickness;
    
    CGFloat subviewTop = bounds.size.height;
    for(NSUInteger i = 0; i < nSubviews; i++) {
        NSView *subview = [contentSubviews objectAtIndex: i];
        CGFloat subviewHeight = floor(proportions[i] * totalSubviewHeightAfter);
        CGFloat subviewBottom = subviewTop - subviewHeight;
        
        NSRect subviewFrame = bounds;
        subviewFrame.origin.y = subviewBottom;
        subviewFrame.size.height = subviewHeight;
        [subview setFrame: subviewFrame];
        
        CGFloat dividerBottom = subviewBottom - dividerThickness;
        
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
        
        subviewBottom = dividerBottom + dividerThickness;
        subviewHeight = subviewTop - subviewBottom;
        
        subviewFrame.origin.y = subviewBottom;
        subviewFrame.size.height = subviewHeight;
        [subview setFrame: subviewFrame];
        
        subviewTop -= subviewHeight;
        subviewTop -= dividerThickness;
    }
    
    free(proportions);
    
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


- (CGFloat) dividerThicknessForAxis: (enum SplitAxis) dividerAxis {
    if(dividerAxis == HorizontalSplitAxis) {
        CGFloat minimumThickness
            = [DocumentSplitView minimumDividerThicknessForAxis: dividerAxis];
        CGFloat emWidth = [(AppDelegate *) [NSApp delegate] emWidth];
        CGFloat totalWidth = [self bounds].size.width;
        NSUInteger nDividers = [contentSubviews count];
        CGFloat minimumNonContentSpace = nDividers * minimumThickness;
        NSUInteger nContentColumns
            = floor((totalWidth - minimumNonContentSpace) / emWidth);
        CGFloat contentSpace = nContentColumns * emWidth;
        CGFloat nonContentSpace = totalWidth - contentSpace;
        return ceil(nonContentSpace / nDividers);
    } else if(dividerAxis == VerticalSplitAxis) {
        CGFloat minimumThickness
            = [DocumentSplitView minimumDividerThicknessForAxis: dividerAxis];
        CGFloat lineHeight = [(AppDelegate *) [NSApp delegate] lineHeight];
        CGFloat totalHeight = [self bounds].size.height;
        NSUInteger nDividers = [contentSubviews count];
        CGFloat minimumNonContentSpace = nDividers * minimumThickness;
        NSUInteger nContentLines
            = floor((totalHeight - minimumNonContentSpace) / lineHeight);
        CGFloat contentSpace = nContentLines * lineHeight;
        CGFloat nonContentSpace = totalHeight - contentSpace;
        return ceil(nonContentSpace / nDividers);
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
            enforcedLeft = absoluteLeft + minimumWidth;
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
            enforcedBottom = absoluteBottom + minimumHeight;
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
        CGFloat enforcedRight = absoluteRight - minimumWidth;
        
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
        CGFloat enforcedTop = absoluteTop - minimumHeight;
        
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
        CGFloat constrainedPosition = absoluteLeft + constrainedWidth;
        
        return constrainedPosition;
    } else if(dividerAxis == VerticalSplitAxis) {
        CGFloat absoluteTop
            = [self absoluteMaxCoordinateOfDividerAt: dividerIndex
                    axis: dividerAxis];
        
        CGFloat lineHeight = [(AppDelegate *) [NSApp delegate] lineHeight];
        CGFloat proposedHeight = absoluteTop - proposedPosition;
        CGFloat constrainedHeight
            = round(proposedHeight / lineHeight) * lineHeight;
        CGFloat constrainedPosition = absoluteTop - constrainedHeight;
        
        return constrainedPosition;
    }
}


- (CGFloat) subviewMinimumSizeForAxis: (enum SplitAxis) dividerAxis {
    if(dividerAxis == HorizontalSplitAxis) {
        CGFloat emWidth = [(AppDelegate *) [NSApp delegate] emWidth];
        return emWidth * 16.0;
    } else if(dividerAxis == VerticalSplitAxis) {
        CGFloat lineHeight = [(AppDelegate *) [NSApp delegate] lineHeight];
        return lineHeight * 5.0;
    }
}


- (CGFloat) subviewCollapseThresholdSizeForAxis: (enum SplitAxis) dividerAxis
{
    if(dividerAxis == HorizontalSplitAxis) {
        CGFloat emWidth = [(AppDelegate *) [NSApp delegate] emWidth];
        return emWidth * 8.0;
    } else if(dividerAxis == VerticalSplitAxis) {
        CGFloat lineHeight = [(AppDelegate *) [NSApp delegate] lineHeight];
        return lineHeight * 2.5;
    }
}

@end
