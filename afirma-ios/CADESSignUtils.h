//
//  CADESSignUtils.h
//  SignSample02
//
//

#import <Foundation/Foundation.h>

@interface CADESSignUtils : NSObject

+(NSString*) dictionary2JavaProperties: (NSDictionary*) dict;
+(NSDictionary*) javaProperties2Dictionary:(NSString*) urlString;
+(bool*) isValidAlgorithm:(NSString*)algorithm;
+(NSDictionary*) parseUrl:(NSString*) urlString;

+(NSData*) signPkcs1:(NSString*)algorithm privateKey:(SecKeyRef*)privateKey data:(NSData*)dataPreSign;
+(NSData*) hashData:(NSString*) algorithm data:(NSData*)dataPreSign;

+(OSStatus) extractIdentityAndTrust:(CFDataRef) inPKCS12Data :(NSString *)pass :(SecIdentityRef *)outIdentity :(SecTrustRef *)outTrust;

+(char*)getAlgorithmOID:(NSString*)algorithm;
+(char*)getHashAlgorithmOID:(NSString*)algorithm;

@end
