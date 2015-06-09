//
//  OpenSSLCertificateHelper.h
//  PortaFirmasUniv
//
//  Created by Rocio Tovar on 18/3/15.
//  Copyright (c) 2015 Atos. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "PFCertificateInfo.h"

@interface OpenSSLCertificateHelper : NSObject

+ (NSArray *)getAddedCertificatesInfo;
+ (OSStatus)deleteCertificate:(PFCertificateInfo *)certificateInfo;

@end
