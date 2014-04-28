#import <Cocoa/Cocoa.h>

@interface FileNameCell : NSCell
{
    NSMutableDictionary *textAttributes;
}
@property (retain) NSImage *icon;
@property (retain) NSString *text;

- (void) initHelper;
- (id) initWithCoder: (NSCoder *) coder;
- (id) initTextCell: (NSString *) newText;
- (id) initImageCell: (NSImage *) newImage;
- (void) drawWithFrame: (NSRect) cellFrame inView: (NSView *) controlView;
- (NSTextView *) fieldEditorForView: (NSView *) controlView;
- (NSText *) setUpFieldEditorAttributes: (NSText *) fieldEditor;
- (void) editWithFrame: (NSRect) cellFrame
                inView: (NSView *) controlView
                editor: (NSText *) fieldEditor
              delegate: (id) delegate
                 event: (NSEvent *) event;
- (void) selectWithFrame: (NSRect) cellFrame
                  inView: (NSView *) controlView
                  editor: (NSText *) fieldEditor
                delegate: (id) delegate
                   start: (NSInteger) selectionStart
                  length: (NSInteger) selectionLength;
- (void) endEditing: (NSText *) fieldEditor;
@end
