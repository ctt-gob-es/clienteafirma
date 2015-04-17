//
//  AOAcercadeViewController.h
//  SignSample02
//
//

#import <UIKit/UIKit.h>
#import "GAITrackedViewController.h"

@interface AOAcercadeViewController : GAITrackedViewController

@property (retain, nonatomic) IBOutlet UIButton *masInfoButton;
-(IBAction)buttonPressed:(id)sender;
@end
