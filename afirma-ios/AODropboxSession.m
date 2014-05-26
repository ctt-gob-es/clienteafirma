//
//  AODropboxSession.m
//  SignSample02
//
//

#import "AODropboxSession.h"

@implementation AODropboxSession

@synthesize session;
static AODropboxSession *instance =nil;
+(AODropboxSession *)getInstance
{
    @synchronized(self)
    {
        if(instance==nil)
        {
            
            instance= [AODropboxSession  new];
        }
    }
    return instance;
}
@end
