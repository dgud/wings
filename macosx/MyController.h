/* MyController */

#import <Cocoa/Cocoa.h>

@interface MyController : NSObject
{
    NSFileHandle *readHandle;
    NSPipe *outPipe;
    NSPipe *inPipe;
    NSTask *aTask;
    NSString *wingsFile;
}
@end
