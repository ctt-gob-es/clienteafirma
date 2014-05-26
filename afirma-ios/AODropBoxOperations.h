//
//  AODropBoxOperations.h
//  SignSample02
//
//

#import <Foundation/Foundation.h>
#import <DropboxSDK/DropboxSDK.h>

@class DBRestClient;

@interface AODropBoxOperations : NSObject <DBRestClientDelegate,UIAlertViewDelegate> {

        DBRestClient *restClient;

}

@property (nonatomic, readonly) DBRestClient* restClient;

-(BOOL *) saveFile:(NSString*)signature filename:(NSString *)filename controller:(UIViewController *) controller;

-(void) alertView:(UIAlertView *)alertView clickedButtonAtIndex:(NSInteger)buttonIndex;

@end
