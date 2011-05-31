#import <Foundation/Foundation.h>

typedef uint8_t uuid_t[16];


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
