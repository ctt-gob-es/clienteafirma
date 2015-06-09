//
//  PFCertificateInfo.m
//  PortaFirmasUniv
//
//  Created by Rocio Tovar on 18/3/15.
//  Copyright (c) 2015 Atos. All rights reserved.
//

#import "DateHelper.h"
#import "PFCertificateInfo.h"

static NSString *const PFCertificateInfoPurposeStringAuthentication = @"Autenticación";
static NSString *const PFCertificateInfoPurposeStringSignature = @"Firma";
static NSString *const PFCertificateInfoPurposeStringEncryption = @"Cifrado";

@implementation PFCertificateInfo

- (NSString *)getPurposeString
{
    NSMutableArray *purposesStringArray = [@[]mutableCopy];
    
    if (_purpose & PFCertificateInfoPurposeAuthentication) {
        [purposesStringArray addObject:PFCertificateInfoPurposeStringAuthentication];
    }
    if (_purpose & PFCertificateInfoPurposeSignature) {
        [purposesStringArray addObject:PFCertificateInfoPurposeStringSignature];
    }
    if (_purpose & PFCertificateInfoPurposeEncryption) {
        [purposesStringArray addObject:PFCertificateInfoPurposeStringEncryption];
    }
    
    return [purposesStringArray componentsJoinedByString:@", "];
}

- (NSString *)getCreationDateString
{
    return [DateHelper getStringFromDate:_creationDate];
}

- (NSString *)getExpirationDateString
{
    return [DateHelper getStringFromDate:_expirationDate];
}

@end
