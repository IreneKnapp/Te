#import "Window/Document/HorizontalDividerManager.h"
#import <HsFFI.h>
#import "Te/LowLevel/ForeignInterface_stub.h"
#import "AppDelegate.h"
#import "Window/Document.h"

static WindowDocumentHorizontalDividerManager *sharedManager = nil;

@implementation WindowDocumentHorizontalDividerManager
@synthesize captionAttributes;
@synthesize titleAttributes;
@synthesize titleUnderprintAttributes;
@synthesize topBorderColor;
@synthesize bottomBorderColor;
@synthesize curvyBorderColor;
@synthesize captionGradient;
@synthesize titleGradient;
@synthesize resizeIndicatorDarkColor;
@synthesize resizeIndicatorLightColor;
@synthesize baselineOffset;
@synthesize captionInset;
@synthesize captionLineHeight;
@synthesize titleLineHeight;
@synthesize inactiveCaptionAttributes;
@synthesize inactiveTitleAttributes;
@synthesize inactiveTopBorderColor;
@synthesize inactiveBottomBorderColor;
@synthesize inactiveCurvyBorderColor;
@synthesize inactiveTitleGradient;
@synthesize inactiveResizeIndicatorDarkColor;
@synthesize inactiveResizeIndicatorLightColor;

+ (id) sharedManager {
    if(sharedManager == nil) {
        sharedManager = [[super allocWithZone: nil] init];
        
        NSFont *captionFont = [(AppDelegate *) [NSApp delegate] captionFont];
        sharedManager.captionAttributes
            = [NSMutableDictionary dictionaryWithCapacity: 2];
        [sharedManager.captionAttributes
          setObject: captionFont forKey: NSFontAttributeName];
        sharedManager.inactiveCaptionAttributes
            = [sharedManager.captionAttributes mutableCopyWithZone: nil];
        [sharedManager.inactiveCaptionAttributes
          setObject: [NSColor colorWithDeviceWhite: 0.61 alpha: 1.0]
          forKey: NSForegroundColorAttributeName];
        
        NSFont *titleFont = [NSFont titleBarFontOfSize: 13.0];
        sharedManager.titleAttributes
            = [NSMutableDictionary dictionaryWithCapacity: 3];
        [sharedManager.titleAttributes
          setObject: titleFont forKey: NSFontAttributeName];
        sharedManager.titleUnderprintAttributes
            = [sharedManager.titleAttributes mutableCopyWithZone: nil];
        [sharedManager.titleUnderprintAttributes
          setObject: [NSColor colorWithDeviceWhite: 0.81 alpha: 1.0]
          forKey: NSForegroundColorAttributeName];
        sharedManager.inactiveTitleAttributes
            = [sharedManager.titleAttributes mutableCopyWithZone: nil];
        [sharedManager.inactiveTitleAttributes
          setObject: [NSColor colorWithDeviceWhite: 0.61 alpha: 1.0]
          forKey: NSForegroundColorAttributeName];
        
        sharedManager.topBorderColor
            = [NSColor colorWithDeviceWhite: 0.89 alpha: 1.0];
        sharedManager.bottomBorderColor
            = [NSColor colorWithDeviceWhite: 0.32 alpha: 1.0];
        NSColor *captionGradientTopColor
            = [NSColor colorWithDeviceWhite: 0.91 alpha: 1.0];
        NSColor *captionGradientBottomColor
            = [NSColor colorWithDeviceWhite: 1.00 alpha: 1.0];
        NSColor *titleGradientTopColor
            = [NSColor colorWithDeviceWhite: 0.81 alpha: 1.0];
        NSColor *titleGradientBottomColor
            = [NSColor colorWithDeviceWhite: 0.66 alpha: 1.0];
        sharedManager.curvyBorderColor
            = [NSColor colorWithDeviceWhite: 0.0 alpha: 0.5];
        sharedManager.captionGradient
            = [[NSGradient alloc] initWithStartingColor: captionGradientTopColor
                                  endingColor: captionGradientBottomColor];
        sharedManager.titleGradient
            = [[NSGradient alloc] initWithStartingColor: titleGradientTopColor
                                  endingColor: titleGradientBottomColor];
        sharedManager.resizeIndicatorDarkColor
            = [NSColor colorWithDeviceWhite: 0.25 alpha: 1.0];
        sharedManager.resizeIndicatorLightColor
            = [NSColor colorWithDeviceWhite: 0.81 alpha: 1.0];
        
        sharedManager.inactiveTopBorderColor
            = [NSColor colorWithDeviceWhite: 0.95 alpha: 1.0];
        sharedManager.inactiveBottomBorderColor
            = [NSColor colorWithDeviceWhite: 0.65 alpha: 1.0];
        NSColor *inactiveTitleGradientTopColor
            = [NSColor colorWithDeviceWhite: 0.97 alpha: 1.0];
        NSColor *inactiveTitleGradientBottomColor
            = [NSColor colorWithDeviceWhite: 0.89 alpha: 1.0];
        sharedManager.inactiveCurvyBorderColor
            = [NSColor colorWithDeviceWhite: 0.5 alpha: 0.5];
        sharedManager.inactiveTitleGradient
            = [[NSGradient alloc] initWithStartingColor:
                                   inactiveTitleGradientTopColor
                                  endingColor:
                                   inactiveTitleGradientBottomColor];
        sharedManager.inactiveResizeIndicatorDarkColor
            = [NSColor colorWithDeviceWhite: 0.65 alpha: 1.0];
        sharedManager.inactiveResizeIndicatorLightColor
            = [NSColor colorWithDeviceWhite: 0.81 alpha: 1.0];
        
        sharedManager.baselineOffset = 3.0;
        sharedManager.captionInset = 5.0;
        sharedManager.captionLineHeight
            = [(AppDelegate *) [NSApp delegate] captionLineHeight];
        sharedManager.titleLineHeight
            = [@"M" sizeWithAttributes: sharedManager.titleAttributes].height;
    }
    return sharedManager;
}


+ (id) allocWithZone: (NSZone *) zone {
    return [self sharedManager];
}


- (id) copyWithZone: (NSZone *) zone {
    return self;
}


- (void) drawRect: (NSRect) dirtyRect
         ofWindow: (NSWindow *) window
{
    void *applicationState
        = [(AppDelegate *) [NSApp delegate] applicationState];
    if(!applicationState) return;
    
    NSWindowController *controller = [window windowController];
    if([controller isKindOfClass: [WindowDocument class]]) {
        WindowDocument *windowDocument = (WindowDocument *) controller;
        
        uuid_t *documentWindowID = [windowDocument windowID];
        
        void *horizontalDividers
            = teDocumentWindowHorizontalDividers
                (applicationState, documentWindowID);
        if(!horizontalDividers) return;
        
        uint64_t count
            = teDocumentHorizontalDividerListCount(horizontalDividers);
        for(uint64_t i = 0; i < count; i++) {
            uuid_t horizontalDividerID;
            teDocumentHorizontalDividerListItem
                (horizontalDividers, i, &horizontalDividerID);
            if(uuidIsNull(&horizontalDividerID)) continue;
            
            int64_t left, top, width, height;
            teDocumentHorizontalDividerFrame
                (applicationState,
                 &horizontalDividerID,
                 &left, &top, &width, &height);
            
            NSRect dividerFrame;
            dividerFrame.origin.x = left;
            dividerFrame.origin.y = top;
            dividerFrame.size.width = width;
            dividerFrame.size.height = height;
            
            BOOL isBottom = YES;
            BOOL activeState= YES;
            NSString *caption = @"above";
            NSString *documentTitle = @"Below";
            
            [self drawInFrame: dividerFrame
                  isBottom: isBottom
                  activeState: activeState
                  caption: caption
                  documentTitle: documentTitle];
        }
        
        teDocumentHorizontalDividerListFree(horizontalDividers);
    }
    
}


- (void) drawGhost: (NSRect) frame {
    [self drawInFrame: frame
          isBottom: NO
          activeState: YES
          caption: nil
          documentTitle: nil];
}


- (void) drawInFrame: (NSRect) dividerFrame
            isBottom: (BOOL) isBottom
         activeState: (BOOL) activeState
             caption: (NSString *) caption
       documentTitle: (NSString *) documentTitle
{
    NSRect topBorderRect = dividerFrame;
    topBorderRect.size.height = 1.0;
    
    NSRect bottomBorderRect = dividerFrame;
    bottomBorderRect.origin.y -= 1.0;
    bottomBorderRect.size.height = 1.0;
    
    CGFloat leftX = dividerFrame.origin.x;
    CGFloat rightX = dividerFrame.origin.x + dividerFrame.size.width;
    
    CGFloat captionEmWidth = [(AppDelegate *) [NSApp delegate] captionEmWidth];
    
    CGFloat curveLeftX = ceil(leftX + captionInset + captionEmWidth * 18.0);
    CGFloat curveRightX = curveLeftX + 50.0;
    CGFloat lowerControlPointX
        = (curveRightX - curveLeftX) * 0.75 + curveLeftX;
    CGFloat upperControlPointX
        = (curveRightX - curveLeftX) * 0.25 + curveLeftX;
    
    CGFloat topY = dividerFrame.origin.y;
    CGFloat bottomY = dividerFrame.origin.y + dividerFrame.size.height;
    
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
    
    [NSGraphicsContext saveGraphicsState];
    
    [NSBezierPath clipRect: dividerFrame];
    
    if(activeState) {
        [titleGradient drawInBezierPath: bottomRegion angle: 90.0];
    } else {
        [inactiveTitleGradient drawInBezierPath: bottomRegion angle: 90.0];
    }
    
    [captionGradient drawInBezierPath: topRegion angle: 90.0];
    
    if(activeState) {
        [topBorderColor set];
    } else {
        [inactiveTopBorderColor set];
    }
    [NSBezierPath fillRect: topBorderRect];
    
    if(activeState) {
        [curvyBorderColor set];
    } else {
        [inactiveCurvyBorderColor set];
    }
    [curvyBorder stroke];
    
    if(!isBottom) {
        if(activeState) {
            [bottomBorderColor set];
        } else {
            [inactiveBottomBorderColor set];
        }
        [NSBezierPath fillRect: bottomBorderRect];
    }
    
    if(caption) {
        NSRect captionRect = dividerFrame;
        captionRect.origin.y += captionRect.size.height;
        captionRect.origin.y -= baselineOffset + captionLineHeight;
        captionRect.size.height = captionLineHeight;
        captionRect.size.width = curveLeftX;
        captionRect.origin.x += captionInset;
        captionRect.size.width -= captionInset;
        
        CGFloat newWidth
            = [caption sizeWithAttributes: captionAttributes].width;
        captionRect.origin.x = (captionRect.size.width - newWidth) / 2.0;
        captionRect.size.width = newWidth;
        
        [NSGraphicsContext saveGraphicsState];
        
        [topRegion addClip];
        
        if(activeState) {
            [caption drawInRect: captionRect
                     withAttributes: captionAttributes];
        } else {
            [caption drawInRect: captionRect
                     withAttributes: inactiveCaptionAttributes];
        }
        
        [NSGraphicsContext restoreGraphicsState];
    }
    
    if(!isBottom && documentTitle) {
        NSRect titleRect = dividerFrame;
        titleRect.origin.y += titleRect.size.height;
        titleRect.origin.y -= baselineOffset + titleLineHeight;
        titleRect.size.height = titleLineHeight;
        
        CGFloat newWidth
            = [documentTitle sizeWithAttributes: titleAttributes].width;
        titleRect.origin.x = (titleRect.size.width - newWidth) / 2.0;
        titleRect.size.width = newWidth;
        
        if(titleRect.origin.x < curveRightX)
            titleRect.origin.x = curveRightX;
        
        NSRect titleUnderprintRect = titleRect;
        titleUnderprintRect.origin.y += 1.0;
        
        [NSGraphicsContext saveGraphicsState];
        
        [bottomRegion addClip];
        
        if(activeState) {
            [documentTitle drawInRect: titleUnderprintRect
                           withAttributes: titleUnderprintAttributes];
            
            [documentTitle drawInRect: titleRect
                           withAttributes: titleAttributes];
        } else {
            [documentTitle drawInRect: titleRect
                           withAttributes: inactiveTitleAttributes];
        }
        
        [NSGraphicsContext restoreGraphicsState];
    }
    
    if(isBottom) {
        [self drawBottomRightCornerResizeIndicatorInFrame: dividerFrame
              activeState: activeState];
    } else {
        NSRect indicatorFrame = dividerFrame;
        indicatorFrame.origin.x += indicatorFrame.size.width - 1.0;
        indicatorFrame.size.width = 30.0;
        indicatorFrame.origin.x -= indicatorFrame.size.width;
        indicatorFrame.origin.y += 2.0;
        indicatorFrame.size.height -= 4.0;
        [self drawVerticalResizeIndicatorInFrame: indicatorFrame
              activeState: activeState];
    }
    
    [NSGraphicsContext restoreGraphicsState];
}


- (void) drawBottomRightCornerResizeIndicatorInFrame: (NSRect) frame
                                         activeState: (BOOL) activeState
{
    void *applicationState
        = [(AppDelegate *) [NSApp delegate] applicationState];
    if(!applicationState) {
        return;
    }
    
    CGFloat bottom = frame.origin.y + frame.size.height - 1.0;
    CGFloat right = frame.size.width - 1.0;
    
    CGFloat dividerHeight = frame.size.height;
    
    NSBezierPath *indicatorPath = [NSBezierPath bezierPath];
    NSBezierPath *indicatorUnderprintPath = [NSBezierPath bezierPath];
    for(CGFloat i = 4.0; i < dividerHeight - 2.0; i += 4.0) {
        [indicatorPath moveToPoint: NSMakePoint(right - i, bottom)];
        [indicatorPath lineToPoint: NSMakePoint(right, bottom - i)];
        
        [indicatorUnderprintPath moveToPoint:
                                  NSMakePoint(right - i + 1, bottom)];
        [indicatorUnderprintPath lineToPoint:
                                  NSMakePoint(right, bottom - i + 1)];
    }
    
    if(activeState) {
        [resizeIndicatorDarkColor set];
    } else {
        [inactiveResizeIndicatorDarkColor set];
    }
    [indicatorPath stroke];
    if(activeState) {
        [resizeIndicatorLightColor set];
    } else {
        [inactiveResizeIndicatorLightColor set];
    }
    [indicatorUnderprintPath stroke];
}


- (void) drawVerticalResizeIndicatorInFrame: (NSRect) frame
                                activeState: (BOOL) activeState
{
    CGFloat originalHeight = frame.size.height;
    CGFloat newHeight = 3.0 * 3;
    frame.origin.y
        = floor(frame.origin.y + (frame.size.height - newHeight) / 2.0);
    frame.size.height = newHeight;
    
    frame.size.width -= (originalHeight - newHeight) / 2.0;
    
    for(CGFloat i = 0.0; i < frame.size.height; i += 3.0) {
        NSRect line
            = NSMakeRect(frame.origin.x, frame.origin.y + i,
                         frame.size.width - 1.0, 1.0);
        
        if(activeState) {
            [resizeIndicatorDarkColor set];
        } else {
            [inactiveResizeIndicatorDarkColor set];
        }
        
        [NSBezierPath fillRect: line];
        
        NSRect underprintLine = line;
        underprintLine.origin.x += 1.0;
        underprintLine.origin.y += 1.0;
        
        if(activeState) {
            [resizeIndicatorLightColor set];
        } else {
            [inactiveResizeIndicatorLightColor set];
        }
        
        [NSBezierPath fillRect: underprintLine];
    }
}

@end