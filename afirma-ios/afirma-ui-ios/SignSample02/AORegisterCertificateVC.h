//
//  AORegisterCertificateVC.h
//  SignSample02
//
//  Created by Rocio Tovar on 25/3/15.
//  Copyright (c) 2015 Atos. All rights reserved.
//

#import <UIKit/UIKit.h>

@protocol AORegisterCertificateVCDelegate <NSObject>

@required
- (void)certificateAdded;

@end

@interface AORegisterCertificateVC : UIViewController <UITextFieldDelegate>

@property (strong, nonatomic) id<AORegisterCertificateVCDelegate> delegate;
@property (strong, nonatomic) NSString *selectedCertificate;
@property (strong, nonatomic) IBOutlet UILabel *selectedCertificateLabel;
@property (strong, nonatomic) IBOutlet UILabel *messageLabel;
@property (strong, nonatomic) IBOutlet UITextField *passwordTextField;

@end
