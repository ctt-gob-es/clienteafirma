//
//  AODropBoxCloudViewController.h
//  SignSample02
//
//

#import <UIKit/UIKit.h>
#import "GAITrackedViewController.h"

@class DBRestClient;

@interface AODropBoxCloudViewController : GAITrackedViewController <UITableViewDelegate> {
    IBOutlet UITableView *dbxTblView;
    
    NSArray* storePaths;
    NSString* storeHash;
    NSString* currentStorePath;
    DBRestClient* restClient;
    IBOutlet UIButton *LogOffDropbox;
    IBOutlet UIButton *saveStore;
}

@property (retain, nonatomic) IBOutlet UITableView *dbxTblView;

-(IBAction)buttonPressed:(id)sender;

-(IBAction)saveStorebuttonPressed:(id)sender;

@end
