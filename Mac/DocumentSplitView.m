#import "DocumentSplitView.h"
#import <HsFFI.h>
#import "Te/ForeignInterface_stub.h"
#import "AppDelegate.h"
#import "DocumentContentView.h"
#import "Utilities.h"


@implementation DocumentSplitView
@synthesize contentSubviews;


+ (CGFloat) minimumDividerThickness {
    return 22.0;
}


- (id) initWithFrame: (NSRect) frame {
    self = [super initWithFrame: frame];
    if(self) {
        trackingDividerDrag = NO;
        contentSubviews = [NSMutableArray arrayWithCapacity: 16];
        dividerSubviews = [NSMutableArray arrayWithCapacity: 16];
    }
    return self;
}


- (DocumentContentView *) newContentSubviewAtIndex: (NSUInteger) index {
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
    
    NSView *newDividerSubview = [[NSView alloc] initWithFrame: NSZeroRect];
    [contentSubviews insertObject: newContentSubview atIndex: index];
    [dividerSubviews insertObject: newDividerSubview atIndex: index];
    [self addSubview: newContentSubview];
    [self addSubview: newDividerSubview];
    return newContentSubview;
}


- (void) removeContentSubviewAtIndex: (NSUInteger) index {
    [contentSubviews removeObjectAtIndex: index];
    [dividerSubviews removeObjectAtIndex: index];
}


- (NSView *) dividerSubviewAtIndex: (NSUInteger) dividerIndex {
    return [dividerSubviews objectAtIndex: dividerIndex];
}


- (void) drawRect: (NSRect) dirtyRect {
    NSFont *captionFont = [(AppDelegate *) [NSApp delegate] baseFont];
    NSMutableDictionary *captionAttributes
        = [NSMutableDictionary dictionaryWithCapacity: 1];
    [captionAttributes setObject: captionFont forKey: NSFontAttributeName];
    
    NSFont *titleFont = [NSFont titleBarFontOfSize: 14.0];
    NSMutableDictionary *titleAttributes
        = [NSMutableDictionary dictionaryWithCapacity: 3];
    [titleAttributes setObject: titleFont forKey: NSFontAttributeName];
    NSMutableParagraphStyle *titleParagraphStyle
        = [[NSParagraphStyle defaultParagraphStyle] mutableCopyWithZone: nil];
    [titleParagraphStyle setAlignment: NSCenterTextAlignment];
    [titleAttributes setObject: titleParagraphStyle
                     forKey: NSParagraphStyleAttributeName];
    NSMutableDictionary *titleUnderprintAttributes
        = [titleAttributes mutableCopyWithZone: nil];
    [titleUnderprintAttributes
      setObject: [NSColor colorWithDeviceWhite: 0.81 alpha: 1.0]
      forKey: NSForegroundColorAttributeName];
    
    NSColor *topBorderColor
        = [NSColor colorWithDeviceWhite: 0.89 alpha: 1.0];
    NSColor *bottomBorderColor
        = [NSColor colorWithDeviceWhite: 0.32 alpha: 1.0];
    NSColor *captionGradientTopColor
        = [NSColor colorWithDeviceWhite: 0.91 alpha: 1.0];
    NSColor *captionGradientBottomColor
        = [NSColor colorWithDeviceWhite: 1.00 alpha: 1.0];
    NSColor *titleGradientTopColor
        = [NSColor colorWithDeviceWhite: 0.81 alpha: 1.0];
    NSColor *titleGradientBottomColor
        = [NSColor colorWithDeviceWhite: 0.66 alpha: 1.0];
    NSGradient *captionGradient
        = [[NSGradient alloc] initWithStartingColor: captionGradientTopColor
                              endingColor: captionGradientBottomColor];
    NSGradient *titleGradient
        = [[NSGradient alloc] initWithStartingColor: titleGradientTopColor
                              endingColor: titleGradientBottomColor];
    
    CGFloat baselineOffset = 3.0;
    CGFloat captionInset = [DocumentContentView leftMarginWidth];
    CGFloat captionLineHeight = [(AppDelegate *) [NSApp delegate] lineHeight];
    CGFloat titleLineHeight
        = [@"M" sizeWithAttributes: titleAttributes].height;
    
    NSUInteger nDividers = [dividerSubviews count];
    for(NSUInteger i = 0; i < nDividers; i++) {
        NSRect dividerFrame = [[dividerSubviews objectAtIndex: i] frame];
        BOOL isBottom = i + 1 == nDividers;
        
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
        
        NSBezierPath *topBorderRightPart = [NSBezierPath bezierPath];
        [topBorderRightPart moveToPoint: NSMakePoint(curveRightX, topY)];
        [topBorderRightPart lineToPoint: NSMakePoint(rightX, topY)];
        
        [titleGradient drawInBezierPath: bottomRegion angle: -90.0];
        
        [captionGradient drawInBezierPath: topRegion angle: -90.0];
        
        [topBorderColor set];
        [NSBezierPath fillRect: topBorderRect];
        
        [[NSColor colorWithDeviceWhite: 0.0 alpha: 0.5] set];
        [curvyBorder stroke];
        [topBorderRightPart stroke];
        
        if(!isBottom) {
            [bottomBorderColor set];
            [NSBezierPath fillRect: bottomBorderRect];
        }
        
        NSString *caption = @"(12, 13) in 1980";
        
        NSRect captionRect = dividerFrame;
        captionRect.origin.y += baselineOffset;
        captionRect.size.height = captionLineHeight;
        captionRect.origin.x += captionInset;
        captionRect.size.width -= captionInset;
        
        [caption drawInRect: captionRect
                 withAttributes: captionAttributes];
        
        if(!isBottom) {
            NSString *documentTitle = @"Document Title";
            
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
}


- (NSView *) hitTest: (NSPoint) point {
    NSView *superResult = [super hitTest: point];
    if([dividerSubviews containsObject: superResult]) {
        return self;
    } else {
        return superResult;
    }
}


- (void) mouseDown: (NSEvent *) event {
    NSPoint location = [self convertPoint: [event locationInWindow]
                             fromView: nil];
    
    NSUInteger nDividers = [dividerSubviews count];
    
    BOOL found = NO;
    NSUInteger dividerIndex;
    for(NSUInteger i = 0; i < nDividers; i++) {
        NSView *dividerSubview = [dividerSubviews objectAtIndex: i];
        NSRect dividerFrame = [dividerSubview frame];
        if(NSPointInRect(location, dividerFrame)) {
            found = YES;
            dividerIndex = i;
            break;
        }
    }
    
    if(found) {
        trackingDividerDrag = YES;
        dividerBeingTracked = dividerIndex;
        previousDragPoint = location;
        createdAbove = NO;
        createdBelow = NO;
                
        BOOL optionDown;
        if([event modifierFlags] & NSAlternateKeyMask)
            optionDown = YES;
        else
            optionDown = NO;
        
        if((dividerIndex + 1 == nDividers) || optionDown) {
            creatingNewDivider = YES;
        } else {
            creatingNewDivider = NO;
        }
    }
}


- (void) mouseDragged: (NSEvent *) event {
    if(!trackingDividerDrag)
        return;
    
    NSView *subviewAbove
        = [contentSubviews objectAtIndex: dividerBeingTracked];
    NSView *subviewBelow = nil;
    if(dividerBeingTracked + 1 < [contentSubviews count]) {
        subviewBelow
            = [contentSubviews objectAtIndex: dividerBeingTracked + 1];
    }
    NSView *dividerSubview
        = [dividerSubviews objectAtIndex: dividerBeingTracked];
    
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
                ofDividerAt: dividerBeingTracked];
    
    CGFloat minConstrainedPosition
        = [self constrainMinCoordinate: constrainedNewPosition
                ofDividerAt: dividerBeingTracked];
    if(constrainedNewPosition < minConstrainedPosition)
        constrainedNewPosition = minConstrainedPosition;
    
    CGFloat maxConstrainedPosition
        = [self constrainMaxCoordinate: constrainedNewPosition
                ofDividerAt: dividerBeingTracked];
    if(constrainedNewPosition > maxConstrainedPosition)
        constrainedNewPosition = maxConstrainedPosition;
    
    CGFloat dividerThickness = [self dividerThickness];
    
    CGFloat absoluteMax
        = [self absoluteMaxCoordinateOfDividerAt: dividerBeingTracked];
    
    CGFloat proposedEdgeAbove = proposedNewPosition + dividerThickness;
    CGFloat proposedHeightAbove = absoluteMax - proposedEdgeAbove;
    
    CGFloat constrainedEdgeAbove = constrainedNewPosition + dividerThickness;
    CGFloat constrainedHeightAbove = absoluteMax - constrainedEdgeAbove;
    
    CGFloat absoluteMin
        = [self absoluteMinCoordinateOfDividerAt: dividerBeingTracked];
    
    CGFloat proposedEdgeBelow = proposedNewPosition;
    CGFloat proposedHeightBelow = proposedEdgeBelow - absoluteMin;
    
    CGFloat constrainedEdgeBelow = constrainedNewPosition;
    CGFloat constrainedHeightBelow = constrainedEdgeBelow - absoluteMin;
    
    collapsedAbove = NO;
    collapsedBelow = NO;
    
    if(!creatingNewDivider) {
        CGFloat actualNewPosition = constrainedNewPosition;
        
        CGFloat subviewCollapseThresholdSize
            = [self subviewCollapseThresholdSize];
        
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
        CGFloat subviewMinimumSize = [self subviewMinimumSize];
        
        {
            CGFloat thresholdHeightAbove
                = frameAbove.size.height - subviewMinimumSize;
            
            if(constrainedHeightAbove <= thresholdHeightAbove) {
                subviewBelow = subviewAbove;
                subviewAbove
                    = [self newContentSubviewAtIndex: dividerBeingTracked];
                
                frameBelow = frameAbove;
                frameBelow.size.height
                    -= constrainedHeightAbove + dividerThickness;
                
                frameAbove.size.height = constrainedHeightAbove;
                frameAbove.origin.y
                    += frameBelow.size.height + dividerThickness;
                
                NSRect newDividerFrame = [dividerSubview frame];
                newDividerFrame.origin.y
                    = frameAbove.origin.y - dividerThickness;
                
                dividerSubview
                    = [dividerSubviews objectAtIndex: dividerBeingTracked];
                
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
            
            if(constrainedHeightBelow <= thresholdHeightBelow) {
                subviewAbove
                    = [self newContentSubviewAtIndex: dividerBeingTracked + 1];
                
                frameAbove = frameBelow;
                frameAbove.origin.y += constrainedHeightBelow;
                frameAbove.size.height -= constrainedHeightBelow;
                
                frameBelow.size.height = constrainedHeightBelow;
                
                NSRect newDividerFrame = [dividerSubview frame];
                newDividerFrame.origin.y
                    = frameAbove.origin.y - dividerThickness;
                
                dividerSubview
                    = [dividerSubviews objectAtIndex: dividerBeingTracked + 1];
                
                [subviewAbove setFrame: frameAbove];
                if(subviewBelow)
                    [subviewBelow setFrame: frameBelow];
                [dividerSubview setFrame: newDividerFrame];
                
                previousDragPoint = location;
                dividerBeingTracked++;
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
    
    if(!creatingNewDivider) {
        if(collapsedAbove) {
            [self removeContentSubviewAtIndex: dividerBeingTracked];
        } else if(collapsedBelow) {
            [self removeContentSubviewAtIndex: dividerBeingTracked + 1];
        }
        
        if(collapsedAbove || collapsedBelow || createdAbove || createdBelow) {
            [self adjustSubviews];
        }
    }
    trackingDividerDrag = NO;
}


- (void) adjustSubviews {
    NSUInteger nSubviews = [contentSubviews count];
    
    if(nSubviews == 0) {
        return;
    } else if(nSubviews == 1) {
        CGFloat dividerThickness = [self dividerThickness];
        
        NSRect contentFrame = [self bounds];
        contentFrame.origin.y += dividerThickness;
        contentFrame.size.height -= dividerThickness;
        [[contentSubviews objectAtIndex: 0] setFrame: contentFrame];
        
        NSRect dividerFrame = [self bounds];
        dividerFrame.size.height = dividerThickness;
        [[dividerSubviews objectAtIndex: 0] setFrame: dividerFrame];
        
        return;
    }
    
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
    
    CGFloat dividerThickness = [self dividerThickness];
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
                              ofDividerAt: i];
        
        CGFloat minDividerBottom = [self constrainMinCoordinate: dividerBottom
                                         ofDividerAt: i];
        if(dividerBottom < minDividerBottom)
            dividerBottom = minDividerBottom;
        
        CGFloat maxDividerBottom = [self constrainMaxCoordinate: dividerBottom
                                         ofDividerAt: i];
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
        NSView *dividerSubview = [dividerSubviews objectAtIndex: i];
        
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
    
    [self setNeedsDisplay: YES];
}


- (void) adjustSubviewsToEqualSizes {
    for(NSView *subview in contentSubviews) {
        [subview setFrame: NSZeroRect];
    }
    
    [self adjustSubviews];
}


- (CGFloat) dividerThickness {
    CGFloat minimumThickness = [DocumentSplitView minimumDividerThickness];
    CGFloat lineHeight = [(AppDelegate *) [NSApp delegate] lineHeight];
    CGFloat totalHeight = [self frame].size.height;
    NSUInteger nDividers = [contentSubviews count];
    CGFloat minimumNonContentSpace
        = totalHeight - nDividers * minimumThickness;
    NSUInteger nContentLines
        = floor((totalHeight - minimumNonContentSpace) / lineHeight);
    CGFloat contentSpace = nContentLines * lineHeight;
    CGFloat nonContentSpace = totalHeight - contentSpace;
    CGFloat extraNonContentSpace = nonContentSpace - minimumNonContentSpace;
    CGFloat extraNonContentSpacePerDivider
        = ceil(extraNonContentSpace / nDividers);
    return extraNonContentSpacePerDivider + minimumThickness;
}


- (CGFloat) absoluteMinCoordinateOfDividerAt: (NSUInteger) dividerIndex {
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


- (CGFloat) absoluteMaxCoordinateOfDividerAt: (NSUInteger) dividerIndex {
    NSView *subviewAbove = [contentSubviews objectAtIndex: dividerIndex];
    NSRect frameAbove = [subviewAbove frame];
    CGFloat dividerThickness = [self dividerThickness];
    CGFloat frameAboveTop = frameAbove.origin.y + frameAbove.size.height;
    return frameAboveTop - dividerThickness;
}


- (CGFloat) constrainMinCoordinate: (CGFloat) proposedPosition
                       ofDividerAt: (NSUInteger) dividerIndex
{
    CGFloat enforcedBottom = 0.0;
    if(dividerIndex + 1 < [contentSubviews count]) {
        CGFloat absoluteBottom
            = [self absoluteMinCoordinateOfDividerAt: dividerIndex];
        CGFloat minimumHeight = [self subviewMinimumSize];
        enforcedBottom = absoluteBottom + minimumHeight;
    }
    
    if(proposedPosition < enforcedBottom)
        return [self constrainSplitPosition: enforcedBottom
                     ofDividerAt: dividerIndex];
    else
        return proposedPosition;
}


- (CGFloat) constrainMaxCoordinate: (CGFloat) proposedPosition
                       ofDividerAt: (NSUInteger) dividerIndex;
{
    CGFloat absoluteTop
        = [self absoluteMaxCoordinateOfDividerAt: dividerIndex];
    CGFloat minimumHeight = [self subviewMinimumSize];
    CGFloat enforcedTop = absoluteTop - minimumHeight;
    
    if(proposedPosition > enforcedTop)
        return [self constrainSplitPosition: enforcedTop
                     ofDividerAt: dividerIndex];
    else
        return proposedPosition;
}


- (CGFloat) constrainSplitPosition: (CGFloat) proposedPosition
                       ofDividerAt: (NSUInteger) dividerIndex;
{
    NSView *subviewAbove = [contentSubviews objectAtIndex: dividerIndex];
    NSRect frameAbove = [subviewAbove frame];
    CGFloat dividerThickness = [self dividerThickness];
    CGFloat frameAboveTop = frameAbove.origin.y + frameAbove.size.height;
    CGFloat absoluteTop = frameAboveTop - dividerThickness;
    
    CGFloat lineHeight = [(AppDelegate *) [NSApp delegate] lineHeight];
    CGFloat proposedHeight = absoluteTop - proposedPosition;
    CGFloat constrainedHeight
        = round(proposedHeight / lineHeight) * lineHeight;
    CGFloat constrainedPosition = absoluteTop - constrainedHeight;
    
    return constrainedPosition;
}


- (CGFloat) subviewMinimumSize {
    CGFloat lineHeight = [(AppDelegate *) [NSApp delegate] lineHeight];
    return lineHeight * 5.0;
}


- (CGFloat) subviewCollapseThresholdSize {
    CGFloat lineHeight = [(AppDelegate *) [NSApp delegate] lineHeight];
    return lineHeight * 2.5;
}

@end
