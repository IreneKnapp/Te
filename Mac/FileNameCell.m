#import "FileNameCell.h"
#import "FileNameCellFieldEditor.h"

@implementation FileNameCell
@synthesize icon;
@synthesize text;


- (void) initHelper {
    windowFieldEditors = [NSMapTable mapTableWithWeakToStrongObjects];
    
    textAttributes = [NSMutableDictionary dictionaryWithCapacity: 2];
    [textAttributes setObject: [NSFont controlContentFontOfSize: 0.0]
                    forKey: NSFontAttributeName];
}


- (id) initWithCoder: (NSCoder *) coder {
    self = [super initWithCoder: coder];
    if(self) {
        icon = nil;
        text = @"";
        [self initHelper];
    }
    return self;
}


- (id) initTextCell: (NSString *) newText {
    self = [super initTextCell: newText];
    if(self) {
        icon = nil;
        text = newText;
        [self initHelper];
    }
    return self;
}


- (id) initImageCell: (NSImage *) newImage {
    self = [super initImageCell: nil];
    if(self) {
        icon = newImage;
        text = @"";
        [self initHelper];
    }
    return self;
}


- (void) drawWithFrame: (NSRect) cellFrame inView: (NSView *) controlView {
    if(icon) {
        NSRect iconFrame = cellFrame;
        iconFrame.size.width = iconFrame.size.height;
        
        [icon drawInRect: iconFrame
              fromRect: NSZeroRect
              operation: NSCompositeSourceOver
              fraction: 1.0
              respectFlipped: YES
              hints: nil];
    }
    
    if(text) {
        NSRect textFrame = cellFrame;
        textFrame.size.width -= textFrame.size.height;
        textFrame.origin.x += textFrame.size.height;
        
        NSColor *foregroundColor = nil;
        if([self backgroundStyle] == NSBackgroundStyleDark)
            foregroundColor = [NSColor whiteColor];
        else
            foregroundColor = [NSColor blackColor];
        [textAttributes setObject: foregroundColor
                        forKey: NSForegroundColorAttributeName];
        
        [text drawInRect: textFrame withAttributes: textAttributes];
    }
}


- (NSTextView *) fieldEditorForView: (NSView *) controlView {
    NSWindow *window = [controlView window];
    if(!window)
        return nil;
    
    FileNameCellFieldEditor *fieldEditor
        = [windowFieldEditors objectForKey: window];
    if(!fieldEditor) {
        fieldEditor = [[FileNameCellFieldEditor alloc] init];
        [windowFieldEditors setObject: fieldEditor forKey: window];
    }
    return fieldEditor;
}


- (NSText *) setUpFieldEditorAttributes: (NSText *) fieldEditor {
    [fieldEditor setBackgroundColor: [NSColor whiteColor]];
    [fieldEditor setDrawsBackground: YES];
    [fieldEditor setEditable: YES];
    [fieldEditor setSelectable: YES];
    [fieldEditor setRichText: NO];
    [fieldEditor setImportsGraphics: NO];
    [fieldEditor setUsesFontPanel: NO];
    [fieldEditor setFont: [NSFont controlContentFontOfSize: 0.0]];
    [fieldEditor setAlignment: NSLeftTextAlignment];
    [fieldEditor setTextColor: [NSColor blackColor]];
    [fieldEditor setBaseWritingDirection: NSWritingDirectionNatural];
    [fieldEditor setVerticallyResizable: YES];
    [fieldEditor setHorizontallyResizable: YES];
    
    if([fieldEditor isKindOfClass: [NSTextView class]]) {
        NSTextView *textView = (NSTextView *) fieldEditor;
        
        [textView setAllowsDocumentBackgroundColorChange: NO];
        [textView setAllowedInputSourceLocales: nil];
        [textView setAllowsUndo: YES];
        [textView setFieldEditor: YES];        
        [textView setAllowsImageEditing: NO];
        [textView setAutomaticQuoteSubstitutionEnabled: NO];
        [textView setAutomaticLinkDetectionEnabled: NO];
        [textView setUsesRuler: NO];
        [textView setRulerVisible: NO];
        [textView setInsertionPointColor: [NSColor blackColor]];        
        [textView setContinuousSpellCheckingEnabled: NO];
        [textView setGrammarCheckingEnabled: NO];
        [textView setAutomaticDashSubstitutionEnabled: NO];
        [textView setAutomaticDataDetectionEnabled: NO];
        [textView setAutomaticTextReplacementEnabled: NO];
        
        NSMutableParagraphStyle *paragraphStyle
            = [[NSParagraphStyle defaultParagraphStyle]
               mutableCopyWithZone: nil];
        [paragraphStyle setAlignment: NSLeftTextAlignment];
        [paragraphStyle setFirstLineHeadIndent: 0.0];
        [paragraphStyle setHeadIndent: 0.0];
        [paragraphStyle setTailIndent: 0.0];
        [textView setDefaultParagraphStyle: paragraphStyle];
    }
}


- (void) editWithFrame: (NSRect) cellFrame
                inView: (NSView *) controlView
                editor: (NSText *) fieldEditor
              delegate: (id) delegate
                 event: (NSEvent *) event;
{
    [self selectWithFrame: cellFrame
          inView: controlView
          editor: fieldEditor
          delegate: delegate
          start: 0
          length: [[self stringValue] length]];
}


- (void) selectWithFrame: (NSRect) cellFrame
                  inView: (NSView *) controlView
                  editor: (NSText *) fieldEditor
                delegate: (id) delegate
                   start: (NSInteger) selectionStart
                  length: (NSInteger) selectionLength
{
    cellFrame.origin.x += cellFrame.size.height;
    cellFrame.size.width -= cellFrame.size.height;
    
    [fieldEditor setString: text];
    
    NSSize maxSize = cellFrame.size;
    NSSize textContainerInset = NSMakeSize(0.0, 0.0);
    
    if([fieldEditor isKindOfClass: [NSTextView class]]) {
        NSTextView *textView = (NSTextView *) fieldEditor;
        NSLayoutManager *layoutManager = [textView layoutManager];
        NSTextContainer *textContainer = [textView textContainer];
        
        textContainerInset = NSMakeSize(2.0, 2.0);
        [textView setTextContainerInset: textContainerInset];
        [textContainer setWidthTracksTextView: NO];
        [textContainer setHeightTracksTextView: NO];
        [textContainer setContainerSize: NSMakeSize(maxSize.width, INFINITY)];
        [textContainer setLineFragmentPadding: 0.0];
        
        NSUInteger numberOfGlyphs = [layoutManager numberOfGlyphs];
        if(numberOfGlyphs > 0) {
            NSRange allGlyphs = NSMakeRange(0, numberOfGlyphs);
            NSRect textRect
                = [layoutManager boundingRectForGlyphRange: allGlyphs
                                 inTextContainer: textContainer];
            
            CGFloat textWidth = textRect.origin.x + textRect.size.width;
            
            if(cellFrame.size.width > textWidth)
                cellFrame.size.width = textWidth;
        }
    }
    
    CGFloat horizontalOffset = textContainerInset.width;
    cellFrame.origin.x -= horizontalOffset;
    cellFrame.size.width += horizontalOffset * 2.0;
    maxSize.width += horizontalOffset * 2.0;
    
    CGFloat verticalOffset = 1.0 - textContainerInset.height;
    if([controlView isFlipped]) {
        verticalOffset *= -1.0;
    }
    cellFrame.origin.y -= verticalOffset;
    cellFrame.size.height += verticalOffset * 2.0;
    maxSize.height += verticalOffset * 2.0;
    
    maxSize.height = INFINITY;
    
    [self setUpFieldEditorAttributes: fieldEditor];
    [fieldEditor setMinSize: cellFrame.size];
    [fieldEditor setMaxSize: maxSize];
    
    NSRange selectedRange = NSMakeRange(selectionStart, selectionLength);
    [fieldEditor setSelectedRange: selectedRange];
    
    [controlView addSubview: fieldEditor];
    [fieldEditor setFrame: cellFrame];
    [fieldEditor setDelegate: delegate];
    [[controlView window] makeFirstResponder: fieldEditor];
}


- (void) endEditing: (NSText *) fieldEditor {
    [fieldEditor removeFromSuperview];
}

@end
