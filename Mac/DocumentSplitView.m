#import "DocumentSplitView.h"
#import <HsFFI.h>
#import "Te/ForeignInterface_stub.h"
#import "AppDelegate.h"
#import "Utilities.h"


@implementation DocumentSplitView

- (id) initWithFrame: (NSRect) frame {
    self = [super initWithFrame: frame];
    if(self) {
    }
    return self;
}


- (void) drawRect: (NSRect) dirtyRect {
    [[NSColor redColor] set];
    [self enumerateDividerRectangles:
           ^(NSRect dividerRect) {
               [NSBezierPath fillRect: dividerRect];
           }];
}


- (void) enumerateDividerRectangles: (void (^)(NSRect rect)) block {
    CGFloat left = 0.0;
    CGFloat width = [self bounds].size.width;
    
    NSArray *subviews = [self subviews];
    for(NSUInteger i = 0; i + 1 < [subviews count]; i++) {
        NSView *subviewAbove = [subviews objectAtIndex: i];
        NSView *subviewBelow = [subviews objectAtIndex: i + 1];
        
        NSRect frameAbove = [subviewAbove frame];
        NSRect frameBelow = [subviewBelow frame];
        
        CGFloat dividerTop = frameAbove.origin.y;
        CGFloat dividerBottom = frameBelow.origin.y + frameBelow.size.height;
        CGFloat dividerHeight = dividerTop - dividerBottom + 32.0;
        
        NSRect divider = NSMakeRect(left, dividerBottom, width, dividerHeight);
        
        block(divider);
    }
}


- (void) adjustSubviews {
    NSArray *subviews = [self subviews];
    NSUInteger nSubviews = [subviews count];
    
    if(nSubviews == 0) {
        return;
    } else if(nSubviews == 1) {
        [[subviews objectAtIndex: 0] setFrame: [self bounds]];
        
        return;
    }
    
    CGFloat totalSubviewHeightBefore = 0.0;
    for(NSView *subview in subviews) {
        totalSubviewHeightBefore += [subview frame].size.height;
    }
    
    CGFloat *proportions = malloc(nSubviews * sizeof(CGFloat));
    if(totalSubviewHeightBefore == 0.0) {
        for(NSUInteger i = 0; i < nSubviews; i++) {
            proportions[i] = 1.0 / nSubviews;
        }
    } else {
        for(NSUInteger i = 0; i < nSubviews; i++) {
            NSView *subview = [subviews objectAtIndex: i];
            proportions[i]
                = [subview frame].size.height / totalSubviewHeightBefore;
        }
    }
    
    CGFloat dividerThickness = [self dividerThickness];
    NSUInteger nDividers = nSubviews - 1;
    NSRect bounds = [self bounds];
    CGFloat totalSubviewHeightAfter
        = bounds.size.height - nDividers * dividerThickness;
    
    CGFloat subviewTop = bounds.size.height;
    for(NSUInteger i = 0; i < nSubviews; i++) {
        NSView *subview = [subviews objectAtIndex: i];
        CGFloat subviewHeight = floor(proportions[i] * totalSubviewHeightAfter);
        CGFloat subviewBottom = subviewTop - subviewHeight;
        
        if(i + 1 == nSubviews && subviewBottom > 0.0) {
            subviewHeight += subviewBottom;
            subviewBottom = 0.0;
        }
        
        NSRect subviewFrame = bounds;
        subviewFrame.origin.y = subviewBottom;
        subviewFrame.size.height = subviewHeight;
        [subview setFrame: subviewFrame];
        
        subviewTop -= subviewHeight;
        subviewTop -= dividerThickness;
    }
    
    free(proportions);
    
    [self setNeedsDisplay: YES];
}


- (CGFloat) dividerThickness {
    CGFloat lineHeight = [(AppDelegate *) [NSApp delegate] lineHeight];
    CGFloat totalHeight = [self frame].size.height;
    NSUInteger nDividers = [[self subviews] count] - 1;
    NSUInteger nBorderAreas = nDividers + 1;
    CGFloat minimumNonContentSpace = totalHeight - nBorderAreas * 32.0;
    NSUInteger nContentLines
        = floor((totalHeight - minimumNonContentSpace) / lineHeight);
    CGFloat contentSpace = nContentLines * lineHeight;
    CGFloat nonContentSpace = totalHeight - contentSpace;
    CGFloat extraNonContentSpace = nonContentSpace - minimumNonContentSpace;
    CGFloat extraNonContentSpacePerDivider
        = ceil(extraNonContentSpace / nDividers);
    return extraNonContentSpacePerDivider;
}


- (CGFloat) constrainMinCoordinate: (CGFloat) proposedMin
                       ofDividerAt: (NSUInteger) dividerIndex
{
    CGFloat lineHeight = [(AppDelegate *) [NSApp delegate] lineHeight];
    return lineHeight * 5.0 + 32.0;
}


- (CGFloat) constrainMaxCoordinate: (CGFloat) proposedMax
                       ofDividerAt: (NSUInteger) dividerIndex;
{
    CGFloat lineHeight = [(AppDelegate *) [NSApp delegate] lineHeight];
    CGFloat totalHeight = [self frame].size.height;
    return totalHeight - 32.0 - lineHeight * 5.0 + [self dividerThickness];
}


- (CGFloat) constrainSplitPosition: (CGFloat) proposedPosition
                       ofDividerAt: (NSUInteger) dividerIndex;
{
    CGFloat lineHeight = [(AppDelegate *) [NSApp delegate] lineHeight];
    return round(proposedPosition / lineHeight) * lineHeight;
}

@end
