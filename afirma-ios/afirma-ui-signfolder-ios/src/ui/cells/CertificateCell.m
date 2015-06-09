//
//  CertificateCell.m
//  PortaFirmasUniv
//
//  Created by Rocio Tovar on 18/3/15.
//  Copyright (c) 2015 Atos. All rights reserved.
//

#import "DateHelper.h"
#import "CertificateCell.h"

static NSString *const kCertificateCellDateFormatString = @"Válido desde %@ hasta %@";
static const NSTimeInterval kCertificateCellDate15DaysTimeInterval = -(15*24*60*60);

@interface CertificateCell ()
{
    CALayer *_expirationIconLayer;
}

@property (nonatomic, weak) IBOutlet UILabel *subjectLabel;
@property (nonatomic, weak) IBOutlet UILabel *issuerLabel;
@property (nonatomic, weak) IBOutlet UILabel *purposeLabel;
@property (nonatomic, weak) IBOutlet UILabel *dateLabel;
@property (nonatomic, weak) IBOutlet UILabel *expirationLabel;

@end

@implementation CertificateCell

- (void)setCertificateInfo:(PFCertificateInfo *)certificateInfo forEditingCell:(BOOL)isEditing
{
    [_subjectLabel setText:certificateInfo.subject];
    [_issuerLabel setText:certificateInfo.issuer];
    [_purposeLabel setText:[certificateInfo getPurposeString]];
    [_dateLabel setText:[NSString stringWithFormat:kCertificateCellDateFormatString,[certificateInfo getCreationDateString], [certificateInfo getExpirationDateString]]];
    if (isEditing) {
        [_expirationLabel setHidden:YES];
    } else {
        [self setupBackgroundForExpirationDate:certificateInfo.expirationDate];
    }
}

- (void)setupBackgroundForExpirationDate:(NSDate *)expirationDate
{
    NSTimeInterval secondsToExpirationDate = [[NSDate date] timeIntervalSinceDate:expirationDate];
    UIColor *color;
    
    if (secondsToExpirationDate > 0) {
        color = COLOR_PRIORITY_RED;
    } else if (secondsToExpirationDate > kCertificateCellDate15DaysTimeInterval) {
        color = COLOR_PRIORITY_YELLOW;
    } else {
        color = nil;
    }
    
    if (color) {
        _expirationIconLayer = [QuartzUtils circleWithColor:color andRect:_expirationLabel.frame];
        [self.contentView.layer insertSublayer:_expirationIconLayer atIndex:0];
        [_expirationLabel setHidden:NO];
    } else {
        [_expirationLabel setHidden:YES];
    }
}

#pragma mark - Reuse

- (void)prepareForReuse
{
    [_expirationIconLayer removeFromSuperlayer];
}

@end
