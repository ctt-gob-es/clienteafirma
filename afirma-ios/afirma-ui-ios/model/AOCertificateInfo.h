//
//  AOCertificateInfo.h
//  SignSample02
//
//  Created by Rocio Tovar on 24/3/15.
//  Copyright (c) 2015 Atos. All rights reserved.
//

#import <Foundation/Foundation.h>

typedef NS_OPTIONS(NSUInteger, PFCertificateInfoPurpose) {
    PFCertificateInfoPurposeAuthentication = (1 << 0),// => 00000001
    PFCertificateInfoPurposeSignature      = (1 << 1),// => 00000010
    PFCertificateInfoPurposeEncryption     = (1 << 3)// => 00001000
};

@interface AOCertificateInfo : NSObject

@property (strong, nonatomic) NSString *issuer;
@property (strong, nonatomic) NSString *subject;
@property (strong, nonatomic) NSDate *expirationDate;
@property (strong, nonatomic) NSDate *creationDate;
@property (assign, nonatomic) PFCertificateInfoPurpose purpose;
@property (assign, nonatomic) SecCertificateRef certificateRef;

- (NSString *)getPurposeString;
- (NSString *)getCreationDateString;
- (NSString *)getExpirationDateString;

@end
