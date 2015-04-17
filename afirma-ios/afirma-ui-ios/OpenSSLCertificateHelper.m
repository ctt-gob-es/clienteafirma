//
//  OpenSSLCertificateHelper.m
//  PortaFirmasUniv
//
//  Created by Rocio Tovar on 18/3/15.
//  Copyright (c) 2015 Atos. All rights reserved.
//

#import "OpenSSLCertificateHelper.h"
#import "CertificateUtils.h"
#import <Security/Security.h>
#import <openssl/x509.h>

@interface OpenSSLCertificateHelper ()
{
    SecKeyRef _currentPrivateKey;
    SecCertificateRef _currentCertificate;
}

@end

@implementation OpenSSLCertificateHelper

#pragma mark - Certificates List Methods

+ (NSArray *)getAddedCertificatesInfo
{
    OSStatus status = noErr;
    NSMutableDictionary *queryDict = [self certificatesQueryDict];
    CFArrayRef resultsArray = nil;
    
    status = SecItemCopyMatching((__bridge CFDictionaryRef)queryDict, (CFTypeRef *)&resultsArray);
    
    if (status != noErr) {
        return nil;
    }
    
    NSMutableArray *certificatesInfoArray = nil;
    CFIndex resultCount = CFArrayGetCount(resultsArray);
    if (resultCount > 0) {
        assert(CFGetTypeID(resultsArray) == CFArrayGetTypeID() );
        certificatesInfoArray = [@[] mutableCopy];
    }
    
    for (CFIndex resultIndex = 0; resultIndex < resultCount; resultIndex++) {
        SecCertificateRef certificateRef = (SecCertificateRef)CFArrayGetValueAtIndex(resultsArray, resultIndex);
        AOCertificateInfo *certificateInfoDict = [self getPFCertificateInfoFromCertificate:certificateRef];
        if (certificateInfoDict) {
            [certificatesInfoArray addObject:certificateInfoDict];
        }
    }
    
    return certificatesInfoArray;
}

+ (NSMutableDictionary *)certificatesQueryDict
{
    NSMutableDictionary *queryDict = [[NSMutableDictionary alloc]init];
    queryDict[(__bridge __strong id)(kSecClass)] = (__bridge id)(kSecClassCertificate);
    queryDict[(__bridge __strong id)(kSecReturnRef)] = (__bridge id)(kCFBooleanTrue);
    queryDict[(__bridge __strong id)(kSecMatchLimit)] = (__bridge id)(kSecMatchLimitAll);
    
    return queryDict;
}

#pragma mark - Certificate Info Methods

#pragma mark - Public


+ (OSStatus)deleteCertificate:(AOCertificateInfo *)certificateInfo
{
    NSMutableDictionary *queryDict = [[NSMutableDictionary alloc]init];
    queryDict[(__bridge __strong id)(kSecClass)] = (__bridge id)(kSecClassCertificate);
    queryDict[(__bridge __strong id)(kSecValueRef)] = (__bridge id)(certificateInfo.certificateRef);
    
    return SecItemDelete((__bridge CFDictionaryRef)queryDict);
}

#pragma mark - Private

+ (AOCertificateInfo *)getPFCertificateInfoFromCertificate:(SecCertificateRef)certificateRef
{
    if (certificateRef != NULL) {
        NSData *certificateData = (__bridge NSData *) SecCertificateCopyData(certificateRef);
        
        const unsigned char *certificateDataBytes = (const unsigned char *)[certificateData bytes];
        X509 *certificateX509 = d2i_X509(NULL, &certificateDataBytes, [certificateData length]);
        
        AOCertificateInfo *certificateInfo = [AOCertificateInfo new];
        [certificateInfo setCertificateRef:certificateRef];
        [certificateInfo setIssuer:[self getIssuerNameForCertificate:certificateX509]];
        [certificateInfo setSubject:[self getSubjectNameForCertificate:certificateX509]];
        [certificateInfo setCreationDate:[self getCreationDateFromCertificate:certificateX509]];
        [certificateInfo setExpirationDate:[self getExpiryDateFromCertificate:certificateX509]];
        [certificateInfo setPurpose:[self getPurposeFromCertificate:certificateX509]];
        
        return certificateInfo;
    }
    
    return nil;
}

+ (NSString *)getIssuerNameForCertificate:(X509 *)certificateX509
{
    NSString *issuer = nil;
    
    if (certificateX509 != NULL) {
        X509_NAME *issuerX509Name = X509_get_issuer_name(certificateX509);
        issuer = [self getStringFromX509Name:issuerX509Name];
    }
    
    return issuer;
}

+ (NSString *)getSubjectNameForCertificate:(X509 *)certificateX509
{
    NSString *subject = nil;
    
    if (certificateX509 != NULL) {
        X509_NAME *subjectX509Name = X509_get_subject_name(certificateX509);
        subject = [self getStringFromX509Name:subjectX509Name];
    }
    
    return subject;
}

+ (NSDate *)getExpiryDateFromCertificate:(X509 *)certificateX509
{
    NSDate *expiryDate = nil;
    
    if (certificateX509 != NULL) {
        ASN1_TIME *certificateExpiryASN1 = X509_get_notAfter(certificateX509);
        expiryDate = [self getDateFromASN1Time:certificateExpiryASN1];
    }
    
    return expiryDate;
}

+ (NSDate *)getCreationDateFromCertificate:(X509 *)certificateX509
{
    NSDate *creationDate = nil;
    
    if (certificateX509 != NULL) {
        ASN1_TIME *certificateCreationASN1 = X509_get_notBefore(certificateX509);
        creationDate = [self getDateFromASN1Time:certificateCreationASN1];
    }
    
    return creationDate;
}

+ (PFCertificateInfoPurpose)getPurposeFromCertificate:(X509 *)certificateX509
{
    PFCertificateInfoPurpose purpose = nil;
    X509_get_ext_d2i(certificateX509, NID_key_usage, NULL, (int *)&purpose);
    
    return purpose;
}

#pragma mark - Generic Convertion Methods

+ (NSString *)getStringFromX509Name:(X509_NAME *)nameX509
{
    NSString *nameString = nil;
    
    if (nameX509 != NULL) {
        X509_NAME_ENTRY *nameEntry = X509_NAME_get_entry(nameX509, X509_NAME_get_index_by_NID(nameX509, NID_commonName, -1));
        
        if (nameEntry) {
            ASN1_STRING *nameASN1 = X509_NAME_ENTRY_get_data(nameEntry);
            
            if (nameASN1 != NULL) {
                unsigned char *name = ASN1_STRING_data(nameASN1);
                nameString = [NSString stringWithUTF8String:(char *)name];
            }
        }
    }
    
    return nameString;
}

+ (NSDate *)getDateFromASN1Time:(ASN1_TIME *)timeASN1
{
    NSDate *date = nil;
    
    if (timeASN1 != NULL) {
        ASN1_GENERALIZEDTIME *timeASN1Generalized = ASN1_TIME_to_generalizedtime(timeASN1, NULL);
        if (timeASN1Generalized != NULL) {
            unsigned char *stringASN1Data = ASN1_STRING_data(timeASN1Generalized);
            
            // ASN1 generalized times look like this: "20131114230046Z"
            //                                format:  YYYYMMDDHHMMSS
            //                               indices:  01234567890123
            //                                                   1111
            // There are other formats (e.g. specifying partial seconds or
            // time zones) but this is good enough for our purposes since
            // we only use the date and not the time.
            //
            // (Source: http://www.obj-sys.com/asn1tutorial/node14.html)
            
            NSString *timeStr = [NSString stringWithUTF8String:(char *)stringASN1Data];
            NSDateComponents *dateComponents = [[NSDateComponents alloc] init];
            
            dateComponents.year   = [[timeStr substringWithRange:NSMakeRange(0, 4)] intValue];
            dateComponents.month  = [[timeStr substringWithRange:NSMakeRange(4, 2)] intValue];
            dateComponents.day    = [[timeStr substringWithRange:NSMakeRange(6, 2)] intValue];
            dateComponents.hour   = [[timeStr substringWithRange:NSMakeRange(8, 2)] intValue];
            dateComponents.minute = [[timeStr substringWithRange:NSMakeRange(10, 2)] intValue];
            dateComponents.second = [[timeStr substringWithRange:NSMakeRange(12, 2)] intValue];
            
            NSCalendar *calendar = [NSCalendar currentCalendar];
            date = [calendar dateFromComponents:dateComponents];
        }
    }
    
    return date;
}

@end
