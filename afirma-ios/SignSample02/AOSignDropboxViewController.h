//
//  AOSignDropboxViewController.h
//  SignSample02
//
//

#import <UIKit/UIKit.h>
#import <DropboxSDK/DropboxSDK.h>
#import "GAITrackedViewController.h"
@class DBRestClient;

@interface AOSignDropboxViewController : GAITrackedViewController<UITableViewDelegate, DBRestClientDelegate> {
    IBOutlet UITableView *dbxTblView;
    
    NSArray* storePaths;
    NSString* storeHash;
    NSString* currentStorePath;
    DBRestClient* restClient;
    IBOutlet UIButton *backButton;
    IBOutlet UIButton *signButton;
    IBOutlet UIButton *logout;
}



@property (retain, nonatomic) IBOutlet UITableView *dbxTblView;

-(IBAction)backButtonPressed:(id)sender;
-(IBAction)logOutButtonPressed:(id)sender;
-(IBAction)signButtonPressed:(id)sender;


@end
