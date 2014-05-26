//
//  AOPinViewController.h
//  SignSample02
//
//

#import <UIKit/UIKit.h>
#import "GAITrackedViewController.h"

@interface AOPinViewController : GAITrackedViewController

@property (retain, nonatomic) IBOutlet UITextView *nombreCert;

@property (retain, nonatomic) IBOutlet UITextField *pinTextField;

@property (retain, nonatomic) IBOutlet UIButton *pinButton;
-(IBAction)pinButtonPressed:(id)sender;

@property(nonatomic, retain) NSString *nombreCertInUse;

@property(nonatomic, retain) NSString *pkcs12Path;

@property(nonatomic, retain) NSString *base64UrlSafeCertificateData;

@property(nonatomic, retain) NSString *certificateName;

@property (nonatomic, retain) NSDictionary *parameters;


@end
