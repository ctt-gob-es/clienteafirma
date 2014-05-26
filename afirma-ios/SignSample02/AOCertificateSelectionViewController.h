//
//  AOCertificateSelectionViewController.h
//  SignSample02
//
//

#import <UIKit/UIKit.h>
#import "GAITrackedViewController.h"

@interface AOCertificateSelectionViewController : GAITrackedViewController <UITableViewDelegate>{
        IBOutlet UITableView *tblView;
        IBOutlet UIButton *selectionButton;
}

@property (retain, nonatomic) IBOutlet UITableView *tblView;
@property (nonatomic, retain) UIButton *selectionButton;
@property (nonatomic, retain) NSString *startUrl;
@property (nonatomic, retain) NSDictionary *parameters;

@end
