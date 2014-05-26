//
//  CADESSignUtils.h
//  SignSample02
//
//

#import <Foundation/Foundation.h>

@interface CADESSignUtils : NSObject

+(NSString*) encodeBase64:(NSData*)dataToEncode;

+(NSString*) urlSafeEncode: (NSString*) string;
+(NSString*) urlSafeDecode: (NSString*) string;

+(NSString*) dictionary2JavaProperties: (NSDictionary*) dict;
+(NSDictionary*) javaProperties2Dictionary:(NSString*) urlString;
+(bool*) isValidAlgorithm:(NSString*)algorithm;
+(NSDictionary*) parseUrl:(NSString*) urlString;

+(NSData*) signPkcs1:(NSString*)algorithm privateKey:(SecKeyRef*)privateKey data:(NSData*)dataPreSign;
+(NSData*) hashData:(NSString*) algorithm data:(NSData*)dataPreSign;

+(OSStatus) extractIdentityAndTrust:(CFDataRef) inPKCS12Data :(NSString *)pass :(SecIdentityRef *)outIdentity :(SecTrustRef *)outTrust;

+(NSData*)DesEncrypt:(NSString*)key :(NSData*)data;
+(NSData*)DesDecrypt:(NSString*)key :(NSData*)data;

+(char*)getAlgorithmOID:(NSString*)algorithm;
+(char*)getHashAlgorithmOID:(NSString*)algorithm;

+ (NSString *) base64EncodeString: (NSString *) strData;
+ (NSString *) base64EncodeData: (NSData *) objData;
+ (NSData *) base64DecodeString: (NSString *) strBase64;

@end
