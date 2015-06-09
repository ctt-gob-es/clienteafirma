//
//  DocumentCertificatesViewController.h
//  PortaFirmasUniv
//
//  Created by Antonio Fiñana on 19/11/12.
//  Copyright (c) 2012 Atos. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "CertificateUtils.h"
#include "ModalCertificatesController.h"

@interface DocumentCertificatesViewController : PFBaseTVC <ModalCertificatesControllerDelegate> {

    NSString *_infoLabel;
    NSArray *files;
    NSString *_selectedCertificate;
    NSString *_password;
    BOOL waitingForDelete;
    BOOL watingForRegister;
}
@property (weak, nonatomic) IBOutlet UITextView *messageView;

// Find files in Document directory
- (NSArray *)findFiles:(NSArray *)extensions;

@end
