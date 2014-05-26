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







@interface CertificateUtils : NSObject {
   
	SecKeyRef _privateKey;
   
   }

@property (nonatomic) SecKeyRef privateKey;

// Search in KeyChain literals
- (NSData *)getHashBytesSHA1:(NSData *)plainText;
- (NSData *)getHashBytesSHA256:(NSData *)plainText;
- (NSData *)getHashBytesSHA384:(NSData *)plainText;
- (NSData *)getHashBytesSHA512:(NSData *)plainText;
- (NSData *)getSignatureBytesSHA1:(NSData *)plainText;
- (NSData *)getSignatureBytesSHA512:(NSData *)plainText;
- (NSData *)getSignatureBytesSHA256:(NSData *)plainText;
- (NSData *)getSignatureBytesSHA384:(NSData *)plainText;

@end
