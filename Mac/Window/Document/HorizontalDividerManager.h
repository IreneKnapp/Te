#import <Cocoa/Cocoa.h>
#import "Utilities.h"

@interface WindowDocumentHorizontalDividerManager : NSObject
{
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
    
    NSMutableDictionary *inactiveCaptionAttributes;
    NSMutableDictionary *inactiveTitleAttributes;
    NSColor *inactiveTopBorderColor;
    NSColor *inactiveBottomBorderColor;
    NSColor *inactiveCurvyBorderColor;
    NSGradient *inactiveTitleGradient;
    NSColor *inactiveResizeIndicatorDarkColor;
    NSColor *inactiveResizeIndicatorLightColor;
}
@property (assign) NSMutableDictionary *captionAttributes;
@property (assign) NSMutableDictionary *titleAttributes;
@property (assign) NSMutableDictionary *titleUnderprintAttributes;
@property (assign) NSColor *topBorderColor;
@property (assign) NSColor *bottomBorderColor;
@property (assign) NSColor *curvyBorderColor;
@property (assign) NSGradient *captionGradient;
@property (assign) NSGradient *titleGradient;
@property (assign) NSColor *resizeIndicatorDarkColor;
@property (assign) NSColor *resizeIndicatorLightColor;
@property (assign) CGFloat baselineOffset;
@property (assign) CGFloat captionInset;
@property (assign) CGFloat captionLineHeight;
@property (assign) CGFloat titleLineHeight;
@property (assign) NSMutableDictionary *inactiveCaptionAttributes;
@property (assign) NSMutableDictionary *inactiveTitleAttributes;
@property (assign) NSColor *inactiveTopBorderColor;
@property (assign) NSColor *inactiveBottomBorderColor;
@property (assign) NSColor *inactiveCurvyBorderColor;
@property (assign) NSGradient *inactiveTitleGradient;
@property (assign) NSColor *inactiveResizeIndicatorDarkColor;
@property (assign) NSColor *inactiveResizeIndicatorLightColor;

+ (id) sharedManager;
+ (id) allocWithZone: (NSZone *) zone;
- (id) copyWithZone: (NSZone *) zone;
- (void) drawRect: (NSRect) dirtyRect
         ofWindow: (NSWindow *) window;
- (void) drawGhost: (NSRect) frame;
- (void) drawInFrame: (NSRect) dividerFrame
            isBottom: (BOOL) isBottom
         activeState: (BOOL) activeState
             caption: (NSString *) caption
       documentTitle: (NSString *) documentTitle;
- (void) drawBottomRightCornerResizeIndicatorInFrame: (NSRect) frame
                                         activeState: (BOOL) activeState;
- (void) drawVerticalResizeIndicatorInFrame: (NSRect) frame
                                activeState: (BOOL) activeState;
@end
