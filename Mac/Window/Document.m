#import "Window/Document.h"
#import <HsFFI.h>
#import "Te/LowLevel/ForeignInterface_stub.h"
#import "AppDelegate.h"
#import "Window/Document/View.h"
#import "Window/Document/PaneManager.h"
#import "Window/Document/HorizontalDividerManager.h"
#import "Window/Document/VerticalDividerManager.h"
#import "Utilities.h"


@implementation WindowDocument
@synthesize adjustingSize;

+ (NSUInteger) windowStyleMask {
    return NSTitledWindowMask
           | NSClosableWindowMask
           | NSMiniaturizableWindowMask
           | NSResizableWindowMask;
}


- (id) initWithWindowID: (uuid_t *) newWindowID
       contentRectangle: (NSRect) contentRectangle
{
    void *applicationState = getApplicationState();
    if(!applicationState)
        return nil;
    
    NSWindow *window
        = [[NSWindow alloc] initWithContentRect: contentRectangle
                            styleMask: [WindowDocument windowStyleMask]
                            backing: NSBackingStoreBuffered
                            defer: YES];
    
    self = [super initWithWindowID: newWindowID window: window];
    if(self) {
        [window setDelegate: self];
        
        adjustingSize = NO;
        stillLoading = YES;
        
        NSRect contentFrame = [[window contentView] frame];
        
        documentView
            = [[WindowDocumentView alloc] initWithFrame: contentFrame];
        
        [window setContentView: documentView];
        
        teDocumentWindowAdjustPanes(applicationState, windowID);
        
        [self setConstraints];
        
        stillLoading = NO;
        
        manuallyAdjustedSize = contentRectangle.size;
        [self adjustSize: manuallyAdjustedSize withAnimation: NO];
        
        NSNotificationCenter *notificationCenter
            = [NSNotificationCenter defaultCenter];
        
        [notificationCenter
          addObserver: self
          selector: @selector(preferredScrollerStyleDidChange:)
          name: NSPreferredScrollerStyleDidChangeNotification
          object: nil];
        
        [window orderFront: self];
    }
    return self;
}


- (void) setConstraints {
    void *applicationState = getApplicationState();
    if(!applicationState)
        return;
    
    NSWindow *window = [self window];
    NSSize minimumSize;
    teDocumentWindowMinimumSize
        (applicationState, windowID, &minimumSize.width, &minimumSize.height);
    CGFloat emWidth = [(AppDelegate *) [NSApp delegate] emWidth];
    emWidth = ceil(emWidth);
    CGFloat lineHeight = [(AppDelegate *) [NSApp delegate] lineHeight];
    lineHeight = ceil(lineHeight);
    
    [window setContentMinSize: minimumSize];
    [window setContentMaxSize: NSMakeSize(INFINITY, INFINITY)];
}


- (void) adjustSize: (NSSize) newSize withAnimation: (BOOL) withAnimation {
    BOOL oldAdjustingSize = adjustingSize;
    adjustingSize = YES;
    
    NSWindow *window = [self window];
    NSRect frame = [window frame];
    NSSize contentSize = [window contentRectForFrameRect: frame].size;
    NSSize decorationSize = NSMakeSize(frame.size.width - contentSize.width,
                                       frame.size.height - contentSize.height);
    
    CGFloat heightDifference = newSize.height - contentSize.height;
    frame.origin.y -= heightDifference;
    
    frame.size.width = newSize.width + decorationSize.width;
    frame.size.height = newSize.height + decorationSize.height;
    
    if(withAnimation)
        [[window animator] setFrame: frame display: NO];
    else
        [window setFrame: frame display: NO];
    
    adjustingSize = oldAdjustingSize;
}


- (void) adjustSizePerContentConstraints {
    if(!adjustingSize && !stillLoading) {
        void *applicationState = getApplicationState();
        if(!applicationState)
            return;
        
        NSSize desiredSize;
        teDocumentWindowDesiredSize
            (applicationState, windowID,
             &desiredSize.width, &desiredSize.height);
        
        CGFloat emWidth = [(AppDelegate *) [NSApp delegate] emWidth];
        CGFloat lineHeight = [(AppDelegate *) [NSApp delegate] lineHeight];
        
        NSUInteger linesToAdd = 0;
        if(manuallyAdjustedSize.height > desiredSize.height)
            linesToAdd
                = floor((manuallyAdjustedSize.height - desiredSize.height)
                        / lineHeight);
        desiredSize.height += linesToAdd * lineHeight;
        desiredSize.height = ceil(desiredSize.height);
        
        NSUInteger columnsToAdd = 0;
        if(manuallyAdjustedSize.width > desiredSize.width)
            columnsToAdd
                = floor((manuallyAdjustedSize.width - desiredSize.width)
                        / emWidth);
        desiredSize.width += columnsToAdd * emWidth;
        desiredSize.width = ceil(desiredSize.width);
        
        [self adjustSize: desiredSize withAnimation: YES];
        
        [self setConstraints];
    }
}


- (void) windowWillStartLiveResize: (NSNotification *) notification {
    if(!stillLoading) {
        [documentView setNeedsDisplay: YES];
        adjustingSize = YES;
    }
}


- (void) windowDidEndLiveResize: (NSNotification *) notification {
    if(!stillLoading) {
        [documentView setNeedsDisplay: YES];
        [self adjustSizePerContentConstraints];
        manuallyAdjustedSize = [[self window] frame].size;
    }
    adjustingSize = NO;
}


- (void) windowDidBecomeMain: (NSNotification *) notification {
    if(!stillLoading) {
        [documentView setNeedsDisplay: YES];
    }
}


- (void) windowDidResignMain: (NSNotification *) notification {
    if(!stillLoading) {
        [documentView setNeedsDisplay: YES];
    }
}


- (void) preferredScrollerStyleDidChange: (NSNotification *) notification {
    [self adjustSizePerContentConstraints];
}


- (BOOL) getCurrentFolderInodeID: (uuid_t *) result {
    void *applicationState = getApplicationState();
    if(!applicationState)
        return NO;
    
    // TODO
    return NO;
}


- (void) mouseDown: (NSEvent *) event {
    void *applicationState = getApplicationState();
    if(!applicationState) return;
    
    NSPoint location = [documentView convertPoint: [event locationInWindow]
                                     fromView: nil];
    
    int64_t x = location.x;
    int64_t y = location.y;
    
    BOOL optionDown;
    if([event modifierFlags] & NSAlternateKeyMask)
        optionDown = YES;
    else
        optionDown = NO;
    
    teDocumentWindowMouseDown(applicationState, &windowID, x, y, optionDown);
    
    /*
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
    */
}


- (void) mouseDragged: (NSEvent *) event {
    /*
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
        
        [documentView setNeedsDisplay: YES];
        
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
                
                teDocumentWindowAdjustPanes(applicationState, windowID);
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
                
                teDocumentWindowAdjustPanes(applicationState, windowID);
            }
        }
    }
    */
}


- (void) mouseUp: (NSEvent *) event {
    /*
    if(!trackingDividerDrag)
        return;
    
    [documentView setNeedsDisplay: YES];
    
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
    */
}

@end
