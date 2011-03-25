#import "MyController.h"
// Err, documentation..
// This starts up erlang via the erlexec program after setting the
//   environment to wherever the app bundle has been put.
// It links up to the stdin and stdout of the emulator and logs emulator output to the OS X console
// If erlang dies we detect stdout closing and die ourselves.
// If we are closed then we rely on erlang detecting its stdin dying thereby killing itself..

@implementation MyController

// Called when the MyController object is kind of unloaded from the NIB file
- (id) init
{
    [super init];
    aTask = [[NSTask alloc] init];
    outPipe = [[NSPipe alloc] init];
    inPipe = [[NSPipe alloc] init];
    readHandle = [inPipe fileHandleForReading];
    wingsFile = nil;
    return self;
}

- (BOOL)application:(NSApplication *)theApplication openFile:(NSString *)filename
{
    wingsFile = filename;
    return YES;
}

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification
{
    NSMutableDictionary *env = [[NSMutableDictionary alloc] init];
    NSString *appDir = [[NSBundle mainBundle] bundlePath];
    NSString *erlangDir = [appDir stringByAppendingString:@"/Contents/Resources"];
    NSString *execFile = [erlangDir stringByAppendingString:@"/bin/erlexec"];
    NSString *binDir = [erlangDir stringByAppendingString:@"/bin"];
    NSArray *args;
    UInt32 version;

    Gestalt(gestaltSystemVersion, (SInt32 *) &version);
    if (version < 0x1040) {
      NSRunCriticalAlertPanel(@"Wings requires Mac OS 10.4 or later",
			      @"Sorry for the inconvenience.", @"OK", nil, nil);
      exit(0);
    }

    [env setObject:erlangDir forKey:@"ROOTDIR"];
    [env setObject:binDir forKey:@"BINDIR"];
    [env setObject:@"beam" forKey:@"EMU"];
    [env setObject:@"Wings3d" forKey:@"PROGNAME"];
    [env setObject:NSHomeDirectory() forKey:@"HOME"];

    args = [NSArray arrayWithObjects: @"-smp", @"-run", @"wings_start", @"start_halt", wingsFile, nil];
    [aTask setStandardOutput: inPipe];
    [aTask setStandardInput: outPipe];
    [aTask setArguments: args];
    [aTask setEnvironment: env];
    [aTask setLaunchPath: execFile];

    // Set up async monitoring for activity on stdout
    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(textFromErlang:)
                                                 name:NSFileHandleReadCompletionNotification
                                               object:readHandle];
    [readHandle readInBackgroundAndNotify];
    [aTask launch];
    [env release];
}

// Callback from the neat async file descriptor monitor/notifier of Cocoa..
// This is almost Erlang Like :-)
- (void) textFromErlang:(NSNotification *)notification
{
    NSDictionary *userinfo = [notification userInfo];
    NSData *data = [userinfo objectForKey:NSFileHandleNotificationDataItem];
    if ([data length]) {
        NSString *string = [[NSString alloc] initWithData:data
                                                 encoding:NSUTF8StringEncoding];
        NSLog(string);
        [string release];
        [readHandle readInBackgroundAndNotify]; // This isn't persistent - re-trigger
    } else {
        [NSApp terminate:self]; //Bye Bye, erlang has closed
    }
}

-(void) dealloc
{
    [[NSNotificationCenter defaultCenter] removeObserver:self];
    
    [inPipe release];
    [outPipe release];
    [aTask release];
    [super dealloc];
}

@end
