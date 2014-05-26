//
//  AOSignViewController.h
//  SignSample02
//
//

#import <UIKit/UIKit.h>
#import "GAITrackedViewController.h"

@interface AOSignViewController : GAITrackedViewController
@property (retain, nonatomic) IBOutlet UILabel *nombreCert;
@property (retain, nonatomic) IBOutlet UIButton *signButton;
-(IBAction)buttonPressed:(id)sender;

@property (nonatomic, retain) NSDictionary *parameters;
@property(nonatomic, retain) NSString *certificateName;
@property(nonatomic, retain) NSString *base64UrlSafeCertificateData;

-(void)setPrivateKey:(SecKeyRef *) privateKey;

@end
