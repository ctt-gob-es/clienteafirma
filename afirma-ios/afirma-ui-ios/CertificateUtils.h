//
//  CertificateUtils.h
//  FirmaDigital
//
//

#import <Foundation/Foundation.h>
#import <Security/Security.h>
#import <UIKit/UIKit.h>
#import <CommonCrypto/CommonDigest.h>

#define kChosenDigestLength     CC_SHA1_DIGEST_LENGTH
#define kTypeOfWrapPadding      kSecPaddingPKCS1
#define kTypeOfSigPadding       kSecPaddingPKCS1SHA1

@interface CertificateUtils : NSObject
{
    SecCertificateRef _certificateRef;
}

@property (nonatomic, assign) SecKeyRef publicKey;
@property (nonatomic, assign) SecKeyRef privateKey;
@property (nonatomic, assign) SecIdentityRef myIdentity;
@property (retain, nonatomic) NSString *summaryString;
@property (retain, nonatomic) NSData *publicKeyBits;
@property (nonatomic, strong) NSString *selectedCertificateName;
@property (nonatomic, retain) NSString *base64UrlSafeCertificateData;


+ (CertificateUtils *)sharedWrapper;
- (OSStatus)loadCertKeyWithName:(NSString *)certName password:(NSString *)pass fromDocument:(BOOL)saveInDocument;
- (OSStatus)loadCertKeyChainWithName:(NSString *)certName password:(NSString *)pass fromDocument:(BOOL)saveInDocument;
- (BOOL)searchIdentityByName:(NSString *)certificateName;
- (NSData *)getHashBytesSHA1:(NSData *)plainText;
- (NSData *)getHashBytesSHA256:(NSData *)plainText;
- (NSData *)getHashBytesSHA384:(NSData *)plainText;
- (NSData *)getHashBytesSHA512:(NSData *)plainText;
- (NSData *)getSignatureBytesSHA1:(NSData *)plainText;
- (NSData *)getSignatureBytesSHA512:(NSData *)plainText;
- (NSData *)getSignatureBytesSHA256:(NSData *)plainText;
- (NSData *)getSignatureBytesSHA384:(NSData *)plainText;

@end
