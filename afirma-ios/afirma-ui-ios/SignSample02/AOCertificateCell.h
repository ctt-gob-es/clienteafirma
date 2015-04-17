//
//  AOCertificateCell.h
//  SignSample02
//
//  Created by Rocio Tovar on 24/3/15.
//  Copyright (c) 2015 Atos. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "AOCertificateInfo.h"

@interface AOCertificateCell : UITableViewCell

- (void)setCertificateInfo:(AOCertificateInfo *)certificateInfo forEditingCell:(BOOL)isEditing;

@end
