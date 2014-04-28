#import <Cocoa/Cocoa.h>
#import "Utilities.h"

@interface WindowDocumentHorizontalDividerManager : NSObject
@property (retain) NSMutableDictionary *captionAttributes;
@property (retain) NSMutableDictionary *titleAttributes;
@property (retain) NSMutableDictionary *titleUnderprintAttributes;
@property (retain) NSColor *topBorderColor;
@property (retain) NSColor *bottomBorderColor;
@property (retain) NSColor *curvyBorderColor;
@property (retain) NSGradient *captionGradient;
@property (retain) NSGradient *titleGradient;
@property (retain) NSColor *resizeIndicatorDarkColor;
@property (retain) NSColor *resizeIndicatorLightColor;
@property (assign) CGFloat baselineOffset;
@property (assign) CGFloat captionInset;
@property (assign) CGFloat captionLineHeight;
@property (assign) CGFloat titleLineHeight;
@property (retain) NSMutableDictionary *inactiveCaptionAttributes;
@property (retain) NSMutableDictionary *inactiveTitleAttributes;
@property (retain) NSColor *inactiveTopBorderColor;
@property (retain) NSColor *inactiveBottomBorderColor;
@property (retain) NSColor *inactiveCurvyBorderColor;
@property (retain) NSGradient *inactiveTitleGradient;
@property (retain) NSColor *inactiveResizeIndicatorDarkColor;
@property (retain) NSColor *inactiveResizeIndicatorLightColor;

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
