#import "Window/Document/View.h"


@implementation WindowDocumentView

- (id) initWithFrame: (NSRect) frame {
    self = [super initWithFrame: frame];
    if(self) {
        [self setAutoresizingMask: NSViewWidthSizable | NSViewHeightSizable];
    }
    return self;
}

@end
