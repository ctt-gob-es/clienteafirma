//
//  AOManagerStoreScreenViewController.h
//  SignSample02
//
//

#import <UIKit/UIKit.h>
#import "GAITrackedViewController.h"

@interface AOManagerStoreScreenViewController : GAITrackedViewController <UITableViewDelegate>{	
	NSString *relinkUserId;
    IBOutlet UITableView *tblViewManager;
    IBOutlet UIButton *deleteButton;
    
}
@property (retain, nonatomic) IBOutlet UITableView *tblViewManager;
@property (retain, nonatomic) NSMutableArray *tableDataManager;
-(IBAction)deleteButtonPressed:(id)sender;

@end

