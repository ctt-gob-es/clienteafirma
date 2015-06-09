//
//  ModalCertificatesController.h
//  PortaFirmasUniv
//
//  Created by Antonio Fiñana on 10/12/12.
//  Copyright (c) 2012 Atos. All rights reserved.
//

#import <UIKit/UIKit.h>

@protocol ModalCertificatesControllerDelegate <NSObject>

@required
- (void)certificateAdded;

@end

@interface ModalCertificatesController : PFBaseVC
{
    NSString *_selectedCertificate;
    NSString *_password;
    NSString *_infoLabel;
}

@property NSString *selectedCertificate;
@property (weak, nonatomic) IBOutlet UILabel *_messageView;
@property (weak, nonatomic) IBOutlet UITextField *passwordText;
@property (retain) IBOutlet UIButton *registrarBtn;
@property (weak, nonatomic) id<ModalCertificatesControllerDelegate> delegate;

- (IBAction)clickCancel:(id)sender;
- (IBAction)clickImport:(id)sender;
- (void)registerWithCertificate;

@end
