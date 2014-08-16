//
//  CertificateUtils.m
//  FirmaDigital
//
//  Modified by Carlos Pérez on 25/02/11.
//

#import "CertificateUtils.h"
#import <Foundation/Foundation.h>
#import <Security/Security.h>
#import <UIKit/UIKit.h>

#define SHA1_DIGESTINFO_HEADER_LENGTH 15
// 15
static unsigned char SHA1_DIGESTINFO_HEADER[] =
{
    0x30,  0x21,  0x30, 0x09,  0x06,  0x05,  0x2B,  0x0E,  0x03, 0x02,
    0x1A,  0x05,  0x00,  0x04,  0x14
};
// 19
#define SHA256_DIGESTINFO_HEADER_LENGTH 19
static unsigned char SHA256_DIGESTINFO_HEADER[] =
{
    0x30, 0x31, 0x30,  0x0D,  0x06,  0x09,  0x60,  0x86, 0x48,  0x01,
    0x65, 0x03, 0x04,  0x02,  0x01,  0x05, 0x00,  0x04, 0x20
};
// 19
#define SHA384_DIGESTINFO_HEADER_LENGTH 19
static unsigned char SHA384_DIGESTINFO_HEADER[]=
{
    0x30,  0x41,  0x30,  0x0D,  0x06,  0x09,  0x60,  0x86,  0x48,  0x01,
    0x65,  0x03,  0x04, 0x02, 0x02, 0x05, 0x00,  0x04,  0x30
};
#define SHA512_DIGESTINFO_HEADER_LENGTH 19

// 19
static unsigned char  SHA512_DIGESTINFO_HEADER[] =
{
    0x30,  0x51,   0x30,   0x0D,   0x06,   0x09,   0x60,   0x86,   0x48,   0x01,
    0x65,   0x03,   0x04,   0x02,   0x03,   0x05,   0x00,   0x04,   0x40
};

@implementation CertificateUtils

@synthesize privateKey=_privateKey;


- (NSData *)getHashBytesSHA1:(NSData *)plainText {
    CC_SHA1_CTX ctx;	
    uint8_t * hashBytes = NULL;
    NSData * hash = nil;
    
	// Malloc a buffer to hold hash.
    hashBytes = malloc( kChosenDigestLength * sizeof(uint8_t) );
    memset((void *)hashBytes, 0x0, kChosenDigestLength);
	
    // Initialize the context.
    CC_SHA1_Init(&ctx);
	
    // Perform the hash.
    CC_SHA1_Update (&ctx, (void *)[plainText bytes], [plainText length]);
	
    // Finalize the output.
    CC_SHA1_Final(hashBytes, &ctx);
	
    // Build up the SHA1 blob.
    hash = [NSData dataWithBytes:(const void *)hashBytes length:(NSUInteger)kChosenDigestLength];
    
    if (hashBytes) free(hashBytes);
	
    return hash;
}

- (NSData *)getHashBytesSHA512:(NSData *)plainText {
    
    CC_SHA512_CTX ctx;
    uint8_t * hashBytes = NULL;
    NSData * hash = nil;
    
	// Malloc a buffer to hold hash.
    hashBytes = malloc( CC_SHA512_DIGEST_LENGTH * sizeof(uint8_t) );
    memset((void *)hashBytes, 0x0, CC_SHA512_DIGEST_LENGTH);
	
    // Initialize the context.
    CC_SHA512_Init(&ctx);
	
    // Perform the hash.
    CC_SHA512_Update (&ctx, (void *)[plainText bytes], [plainText length]);
	
    // Finalize the output.
    CC_SHA512_Final(hashBytes, &ctx);
	
    // Build up the SHA1 blob.
    hash = [NSData dataWithBytes:(const void *)hashBytes length:(NSUInteger)CC_SHA512_DIGEST_LENGTH];
    
    if (hashBytes) free(hashBytes);
	
    return hash;
}

- (NSData *)getHashBytesSHA256:(NSData *)plainText {
     
    CC_SHA256_CTX ctx;
    uint8_t * hashBytes = NULL;
    NSData * hash = nil;
    
	// Malloc a buffer to hold hash.
    hashBytes = malloc( CC_SHA256_DIGEST_LENGTH * sizeof(uint8_t) );
    memset((void *)hashBytes, 0x0, CC_SHA256_DIGEST_LENGTH);
	
    // Initialize the context.
    CC_SHA256_Init(&ctx);
	
    // Perform the hash.
    CC_SHA256_Update (&ctx, (void *)[plainText bytes], [plainText length]);
	
    // Finalize the output.
    CC_SHA256_Final(hashBytes, &ctx);
	
    // Build up the SHA1 blob.
    hash = [NSData dataWithBytes:(const void *)hashBytes length:(NSUInteger)CC_SHA256_DIGEST_LENGTH];
    
    if (hashBytes) free(hashBytes);
	
    return hash;
}

- (NSData *)getHashBytesSHA384:(NSData *)plainText {
    
    CC_SHA512_CTX ctx;
    uint8_t * hashBytes = NULL;
    NSData * hash = nil;
    
	// Malloc a buffer to hold hash.
    hashBytes = malloc( CC_SHA384_DIGEST_LENGTH * sizeof(uint8_t) );
    memset((void *)hashBytes, 0x0, CC_SHA384_DIGEST_LENGTH);
	
    // Initialize the context.
    CC_SHA384_Init(&ctx);
	
    // Perform the hash.
    CC_SHA384_Update (&ctx, (void *)[plainText bytes], [plainText length]);
	
    // Finalize the output.
    CC_SHA384_Final(hashBytes, &ctx);
	
    // Build up the SHA1 blob.
    hash = [NSData dataWithBytes:(const void *)hashBytes length:(NSUInteger)CC_SHA384_DIGEST_LENGTH];
    
    if (hashBytes) free(hashBytes);
	
    return hash;
}

- (NSData *)getSignatureBytesSHA1:(NSData *)plainText{
	OSStatus sanityCheck = noErr;
	NSData * signedHash = nil;
	
	uint8_t * signedHashBytes = NULL;
	size_t signedHashBytesSize = 0;
	
	signedHashBytesSize = SecKeyGetBlockSize(_privateKey);
	
	// Malloc a buffer to hold signature.
	signedHashBytes = malloc( signedHashBytesSize * sizeof(uint8_t) );
	memset((void *)signedHashBytes, 0x0, signedHashBytesSize);
	
    const uint8_t *hashMessage=[[self getHashBytesSHA1:plainText] bytes];

    uint8_t * digestInfo = malloc((CC_SHA1_DIGEST_LENGTH + SHA1_DIGESTINFO_HEADER_LENGTH)* sizeof(uint8_t));
    
    memcpy(digestInfo, SHA1_DIGESTINFO_HEADER, SHA1_DIGESTINFO_HEADER_LENGTH);
    
    for (int i=SHA1_DIGESTINFO_HEADER_LENGTH; i<(CC_SHA1_DIGEST_LENGTH + SHA1_DIGESTINFO_HEADER_LENGTH); i++)
    {
        digestInfo[i] = (uint8_t) hashMessage[i-SHA1_DIGESTINFO_HEADER_LENGTH];
    }
    
    
	
    // Sign the SHA1 hash.
	sanityCheck = SecKeyRawSign(_privateKey,
								kSecPaddingPKCS1,
								digestInfo,
								CC_SHA1_DIGEST_LENGTH + SHA1_DIGESTINFO_HEADER_LENGTH,
                                (uint8_t *)signedHashBytes,
								&signedHashBytesSize
								);
	
	NSLog(@"sanityCheck::Return code=%ld",sanityCheck);
    
	// Build up signed SHA1 blob.
	signedHash = [NSData dataWithBytes:(const void *)signedHashBytes length:(NSUInteger)signedHashBytesSize];
	
    if (signedHashBytes) free(signedHashBytes);
	if (digestInfo) free(digestInfo);
	
	return signedHash;
}


- (NSData *)getSignatureBytesSHA256:(NSData *)plainText {
    OSStatus sanityCheck = noErr;
	NSData * signedHash = nil;
	
	uint8_t * signedHashBytes = NULL;
	size_t signedHashBytesSize = 0;
	
	signedHashBytesSize = SecKeyGetBlockSize(_privateKey);
	
	// Malloc a buffer to hold signature.
	signedHashBytes = malloc( signedHashBytesSize * sizeof(uint8_t) );
	memset((void *)signedHashBytes, 0x0, signedHashBytesSize);
	
    const uint8_t *hashMessage=[[self getHashBytesSHA256:plainText] bytes];
    
    uint8_t * digestInfo = malloc((CC_SHA256_DIGEST_LENGTH + SHA256_DIGESTINFO_HEADER_LENGTH)* sizeof(uint8_t));
    memcpy(digestInfo, SHA256_DIGESTINFO_HEADER, SHA256_DIGESTINFO_HEADER_LENGTH);
    
    for (int i=SHA256_DIGESTINFO_HEADER_LENGTH; i<(CC_SHA256_DIGEST_LENGTH + SHA256_DIGESTINFO_HEADER_LENGTH); i++)
    {
        digestInfo[i] = (uint8_t) hashMessage[i-SHA256_DIGESTINFO_HEADER_LENGTH];
    }
    
    
	
    // Sign the SHA1 hash.
	sanityCheck = SecKeyRawSign(_privateKey,
								kSecPaddingPKCS1,
								digestInfo,
								CC_SHA256_DIGEST_LENGTH + SHA256_DIGESTINFO_HEADER_LENGTH,
                                (uint8_t *)signedHashBytes,
								&signedHashBytesSize
								);
	
	NSLog(@"sanityCheck::Return code=%ld",sanityCheck);
    
	// Build up signed SHA256 blob.
	signedHash = [NSData dataWithBytes:(const void *)signedHashBytes length:(NSUInteger)signedHashBytesSize];
	
    if (signedHashBytes) free(signedHashBytes);
	if (digestInfo) free(digestInfo);
	
	return signedHash;
}


- (NSData *)getSignatureBytesSHA384:(NSData *)plainText {
    OSStatus sanityCheck = noErr;
	NSData * signedHash = nil;
	
	uint8_t * signedHashBytes = NULL;
	size_t signedHashBytesSize = 0;
	
	signedHashBytesSize = SecKeyGetBlockSize(_privateKey);
	
	// Malloc a buffer to hold signature.
	signedHashBytes = malloc( signedHashBytesSize * sizeof(uint8_t) );
	memset((void *)signedHashBytes, 0x0, signedHashBytesSize);
	
    const uint8_t *hashMessage=[[self getHashBytesSHA384:plainText] bytes];
    
    uint8_t * digestInfo = malloc((CC_SHA384_DIGEST_LENGTH + SHA384_DIGESTINFO_HEADER_LENGTH)* sizeof(uint8_t));
    memcpy(digestInfo, SHA384_DIGESTINFO_HEADER, SHA384_DIGESTINFO_HEADER_LENGTH);
    
    for (int i=SHA384_DIGESTINFO_HEADER_LENGTH; i<(CC_SHA384_DIGEST_LENGTH + SHA384_DIGESTINFO_HEADER_LENGTH); i++)
    {
        digestInfo[i] = (uint8_t) hashMessage[i-SHA384_DIGESTINFO_HEADER_LENGTH];
    }
    
    
	
    // Sign the SHA1 hash.
	sanityCheck = SecKeyRawSign(_privateKey,
								kSecPaddingPKCS1,
								digestInfo,
								CC_SHA384_DIGEST_LENGTH + SHA384_DIGESTINFO_HEADER_LENGTH,
                                (uint8_t *)signedHashBytes,
								&signedHashBytesSize
								);
	
	NSLog(@"sanityCheck::Return code=%ld",sanityCheck);
    
	// Build up signed SHA256 blob.
	signedHash = [NSData dataWithBytes:(const void *)signedHashBytes length:(NSUInteger)signedHashBytesSize];
	
    if (signedHashBytes) free(signedHashBytes);
	if (digestInfo) free(digestInfo);
	
	return signedHash;

}



- (NSData *)getSignatureBytesSHA512:(NSData *)plainText
{
	OSStatus sanityCheck = noErr;
	NSData * signedHash = nil;
	
	uint8_t * signedHashBytes = NULL;
	size_t signedHashBytesSize = 0;
	
	signedHashBytesSize = SecKeyGetBlockSize(_privateKey);
	
	// Malloc a buffer to hold signature.
	signedHashBytes = malloc( signedHashBytesSize * sizeof(uint8_t) );
	memset((void *)signedHashBytes, 0x0, signedHashBytesSize);
	
    const uint8_t *hashMessage=[[self getHashBytesSHA512:plainText] bytes];
    // Concatenamos SHA512
    //SHA512_DIGESTINFO_HEADER+hashMessage
     
    
    uint8_t * digestInfo = malloc((CC_SHA512_DIGEST_LENGTH + SHA512_DIGESTINFO_HEADER_LENGTH)* sizeof(uint8_t));
       
    memcpy(digestInfo, SHA512_DIGESTINFO_HEADER, SHA512_DIGESTINFO_HEADER_LENGTH);
   
    for (int i=SHA512_DIGESTINFO_HEADER_LENGTH; i<(CC_SHA512_DIGEST_LENGTH + SHA512_DIGESTINFO_HEADER_LENGTH); i++)
    {
        digestInfo[i] = (uint8_t) hashMessage[i-SHA512_DIGESTINFO_HEADER_LENGTH];
    }
        
        
	
    // Sign the SHA1 hash.
	sanityCheck = SecKeyRawSign(_privateKey,
								kSecPaddingPKCS1,
								digestInfo,
								CC_SHA512_DIGEST_LENGTH + SHA512_DIGESTINFO_HEADER_LENGTH,
                                (uint8_t *)signedHashBytes,
								&signedHashBytesSize
								);
	
	NSLog(@"sanityCheck::Return code=%ld",sanityCheck);
    
	// Build up signed SHA1 blob.
	signedHash = [NSData dataWithBytes:(const void *)signedHashBytes length:(NSUInteger)signedHashBytesSize];
	
    if (signedHashBytes) free(signedHashBytes);
	if (digestInfo) free(digestInfo);
	
	return signedHash;
}


- (void)dealloc {
	[super dealloc];
   
    //[_summaryString dealloc];
    //[_publicKeyBits dealloc];
    
    
}
@end
