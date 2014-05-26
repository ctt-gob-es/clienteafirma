//
//  AODropboxSession.h
//  SignSample02
//
//

#import <Foundation/Foundation.h>

@interface AODropboxSession : NSObject{
    NSString *session;

}
@property(nonatomic,retain)NSString *session;
+(AODropboxSession*)getInstance;

@end
