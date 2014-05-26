//
//  AOUrlDropbox.m
//  SignSample02
//
//

#import "AOUrlDropbox.h"


@implementation AOUrlDropbox
@synthesize str;
static AOUrlDropbox *instance =nil;
+(AOUrlDropbox *)getInstance
{
    @synchronized(self)
    {
        if(instance==nil)
        {
            
            instance= [AOUrlDropbox new];
        }
    }
    return instance;
}
@end