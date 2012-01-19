#import <Cocoa/Cocoa.h>

typedef uint8_t uuid_t[16];

enum CursorType {
    ArrowCursorType = 0,
    ResizeUpCursorType,
    ResizeDownCursorType,
    ResizeLeftCursorType,
    ResizeRightCursorType,
    ResizeUpDownCursorType,
    ResizeLeftRightCursorType,
};


extern void *getApplicationState();
extern void setUUIDNull(uuid_t *destination);
extern void copyUUID(uuid_t *destination, uuid_t *source);
extern int uuidIsNull(uuid_t *uuid);
extern NSUInteger uuidHashPointerFunction(const void *item,
                                          NSUInteger (*size)(const void *item));
extern BOOL uuidIsEqualPointerFunction(const void *item1,
                                       const void *item2,
                                       NSUInteger (*size)(const void *item));
extern NSUInteger uuidSizePointerFunction(const void *item);
extern NSString *uuidDescriptionPointerFunction(const void *item);
extern void *opaqueAcquirePointerFunction(const void *src,
                                          NSUInteger (*size)(const void *item),
                                          BOOL shouldCopy);
extern void opaqueRelinquishPointerFunction(const void *item,
                                            NSUInteger
                                             (*size)(const void *item));
extern uint64_t dragOperationMaskToOperations(NSDragOperation operationMask);
extern NSDragOperation dragOperationsToOperationMask(uint64_t operations);
extern void appendWord64(NSMutableData *data, uint64_t value);
extern void extractWord64(NSData *data, NSInteger offset, uint64_t *value);
extern void *extractInodesDragInformation(NSData *data,
                                          uint64_t allowedDragOperations);
