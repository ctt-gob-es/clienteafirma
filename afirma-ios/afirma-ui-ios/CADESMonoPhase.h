//
//  CADESMonoPhase.h
//  SignSample02
//
//

#import <Foundation/Foundation.h>

@interface CADESMonoPhase : NSObject

+(NSData*) getCadesMonoPhase:(NSString*) base64UrlSafeCertificateData
                        mode:(NSString*)mode
                 contentData:(NSData*)contentData
                  privateKey:(SecKeyRef*)privateKeyPkcs12
               signAlgoInUse:(NSString *) signAlgoInUse
          contentDescription:(NSString*)contentDescription
                   policyOID:(NSString*)policyOID
                  policyHash:(NSString*)policyHash
               policiHashAlg:(char*)policyHashAlg
                   policyUri:(NSString*)policyUri
               signAlgorithm:(char*)signAlgorithm
        signingCertificateV2:(int)signingCertificateV2
  precalculatedHashAlgorithm:(char*)precalculatedHashAlgorithm
           precalculatedHash:(NSData*)precalculatedHash;

@end
