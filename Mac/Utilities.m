#import "Utilities.h"
#import <HsFFI.h>
#import "Te/LowLevel/ForeignInterface_stub.h"
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
    teTextFree(cString);
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


void appendWord64(NSMutableData *data, uint64_t value) {
    uint8_t buffer[8];
    buffer[0] = (value >> 0) & 0xFF;
    buffer[1] = (value >> 8) & 0xFF;
    buffer[2] = (value >> 16) & 0xFF;
    buffer[3] = (value >> 24) & 0xFF;
    buffer[4] = (value >> 32) & 0xFF;
    buffer[5] = (value >> 40) & 0xFF;
    buffer[6] = (value >> 48) & 0xFF;
    buffer[7] = (value >> 56) & 0xFF;
    [data appendBytes: &buffer length: 8];
}


void extractWord64(NSData *data, NSInteger offset, uint64_t *value) {
    uint8_t buffer[8];
    [data getBytes: &buffer range: NSMakeRange(offset, 8)];
    *value = 0;
    *value |= (((uint64_t) buffer[0]) << 0);
    *value |= (((uint64_t) buffer[1]) << 8);
    *value |= (((uint64_t) buffer[2]) << 16);
    *value |= (((uint64_t) buffer[3]) << 24);
    *value |= (((uint64_t) buffer[4]) << 32);
    *value |= (((uint64_t) buffer[5]) << 40);
    *value |= (((uint64_t) buffer[6]) << 48);
    *value |= (((uint64_t) buffer[7]) << 56);
}


void *extractInodesDragInformation(NSData *data,
                                   uint64_t allowedDragOperations)
{
    void *applicationState = getApplicationState();
    if(!applicationState)
        return NULL;
    
    if(data && ([data length] >= 24)) {
        uuid_t draggedBrowserWindowID;
        [data getBytes: &draggedBrowserWindowID
              range: NSMakeRange(0, 16)];
        
        uint64_t inodeIDCount;
        extractWord64(data, 16, &inodeIDCount);
        
        if([data length] == 16 + 8 + 16 * inodeIDCount) {
            uuid_t *inodeIDs = malloc(inodeIDCount * sizeof(uuid_t));
            [data getBytes: inodeIDs
                  range: NSMakeRange(24, inodeIDCount * 16)];
            
            void *inodes = teInodeListNew(applicationState,
                                          &draggedBrowserWindowID,
                                          inodeIDCount,
                                          inodeIDs);
            
            free(inodeIDs);
            
            void *dragInformation
                = teInodeDragInformationNew(applicationState,
                                            allowedDragOperations,
                                            &draggedBrowserWindowID,
                                            inodes);
            
            teInodeListFree(inodes);
            
            return dragInformation;
        } else {
            return NULL;
        }
    } else {
        return NULL;
    }
}
