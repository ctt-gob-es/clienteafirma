//
//  CertificateCell.h
//  PortaFirmasUniv
//
//  Created by Rocio Tovar on 18/3/15.
//  Copyright (c) 2015 Atos. All rights reserved.
//

#import "PFCertificateInfo.h"
#import <UIKit/UIKit.h>

@interface CertificateCell : UITableViewCell

- (void)setCertificateInfo:(PFCertificateInfo *)certificateInfo forEditingCell:(BOOL)isEditing;

@end
