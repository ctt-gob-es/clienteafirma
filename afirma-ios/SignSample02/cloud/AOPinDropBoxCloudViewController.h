//
//  AOPinDropBoxCloudViewController.h
//  SignSample02
//
//

#import <UIKit/UIKit.h>
#import "GAITrackedViewController.h"

@interface AOPinDropBoxCloudViewController : GAITrackedViewController
@property (retain, nonatomic) IBOutlet UITextView *nameCertDropBoxCloud;
@property (retain, nonatomic) IBOutlet UITextField *pinCertDropBoxCloud;
@property (retain, nonatomic) IBOutlet UIButton *buttonCertDropBoxCloud;

-(IBAction)pinButtonPressed:(id)sender;

@property (retain, nonatomic) NSString *paramNameCertDropBox;
@property (retain, nonatomic) NSString *paramUrlCertDropBox;

@end
