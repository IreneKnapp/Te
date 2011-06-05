#import "Utilities.h"
#import <HsFFI.h>
#import "Te/ForeignInterface_stub.h"
#import "AppDelegate.h"


void *getApplicationState() {
    return [(AppDelegate *) [NSApp delegate] applicationState];
}


void setUUIDNull(uuid_t *destination) {
    for(int i = 0; i < 16; i++) {
        ((uint8_t *) destination)[i] = 0x00;
    }
}


void copyUUID(uuid_t *destination, uuid_t *source) {
    for(int i = 0; i < 16; i++) {
        ((uint8_t *) destination)[i] = ((uint8_t *) source)[i];
    }
}


int uuidIsNull(uuid_t *uuid) {
    for(int i = 0; i < 16; i++) {
        uint8_t byte = ((uint8_t *) uuid)[i];
        if(byte != 0x00)
            return 0;
    }
    return 1;
}


NSUInteger uuidHashPointerFunction(const void *item,
                                   NSUInteger (*size)(const void *item))
{
    uint64_t result = teUUIDHash((void *) item);
    return result;
}

                                    
BOOL uuidIsEqualPointerFunction(const void *item1,
                                const void *item2,
                                NSUInteger (*size)(const void *item))
{
    return teUUIDEqual((void *) item1, (void *) item2);
}


NSUInteger uuidSizePointerFunction(const void *item)
{
    return 16;
}


NSString *uuidDescriptionPointerFunction(const void *item)
{
    char *cString = teUUIDShow((void *) item);
    NSString *string = [NSString stringWithUTF8String: cString];
    teStringFree(cString);
    return string;
}


void *opaqueAcquirePointerFunction(const void *src,
                                   NSUInteger (*size)(const void *item),
                                   BOOL shouldCopy)
{
    NSUInteger objectSize = size(src);
    void *result = malloc(objectSize);
    memcpy(result, src, objectSize);
    return result;
}


void opaqueRelinquishPointerFunction(const void *item,
                                     NSUInteger (*size)(const void *item))
{
    free((void *) item);
}


uint64_t dragOperationMaskToOperations(NSDragOperation operationMask) {
    uint64_t operations = 0;
    if(operationMask & NSDragOperationCopy)
        operations |= teDragOperationCopy();
    if(operationMask & NSDragOperationLink)
        operations |= teDragOperationLink();
    if(operationMask & NSDragOperationGeneric)
        operations |= teDragOperationGeneric();
    if(operationMask & NSDragOperationMove)
        operations |= teDragOperationMove();
    if(operationMask & NSDragOperationDelete)
        operations |= teDragOperationDelete();
    return operations;
}


NSDragOperation dragOperationsToOperationMask(uint64_t operations) {
    NSDragOperation operationMask = 0;
    if(operations & teDragOperationCopy())
        operationMask |= NSDragOperationCopy;
    if(operations & teDragOperationLink())
        operationMask |= NSDragOperationLink;
    if(operations & teDragOperationGeneric())
        operationMask |= NSDragOperationGeneric;
    if(operations & teDragOperationMove())
        operationMask |= NSDragOperationMove;
    if(operations & teDragOperationDelete())
        operationMask |= NSDragOperationDelete;
    return operationMask;
}
