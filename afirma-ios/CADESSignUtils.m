//
//  CADESSignUtils.m
//  SignSample02
//
//

#import "CADESSignUtils.h"
#import <resolv.h>
#import <CommonCrypto/CommonCryptor.h>
#import <CommonCrypto/CommonDigest.h>
#import "CertificateUtils.h"
#import "CADESConstants.h"
#import "CADESOID.h"

@implementation CADESSignUtils


/**
Método que codifica un array de bytes a base64.
 
parámetros:
-----------
dataToEncode: datos a codificar.

 */
+(NSString*) encodeBase64:(NSData*)dataToEncode {
    
    NSData *encodedData = nil;
    
    NSUInteger dataToEncodeLength = dataToEncode.length;
      
    NSUInteger encodedBufferLength = ((dataToEncodeLength + 2) / 3) * 4 + 1;
    
    char *encodedBuffer = malloc(encodedBufferLength);
    
    int encodedRealLength = b64_ntop(dataToEncode.bytes, dataToEncodeLength,
                                     encodedBuffer, encodedBufferLength);
    
    if(encodedRealLength >= 0) {
        encodedData = [NSData dataWithBytesNoCopy:encodedBuffer
                                           length:encodedRealLength + 1
                                     freeWhenDone:YES];
    }
    else {
        free(encodedBuffer);
    }
    
    return [[NSString alloc] initWithData:encodedData encoding:NSUTF8StringEncoding];
    
}

/**
 Método que codifica una cadena base64 a URLSAFE.
 
 parámetros:
 -----------
 string: datos a codificar.
 
 */
+ (NSString*) urlSafeEncode: (NSString*) string {
    return [[string stringByReplacingOccurrencesOfString:@"+" withString:@"-"] stringByReplacingOccurrencesOfString:@"/" withString:@"_"];
}

/**
 Método que decodifica una cadena base64 a URLSAFE.
 
 parámetros:
 -----------
 string: datos a decodificar.
 
 */
+ (NSString*) urlSafeDecode: (NSString*) string {
    return [[string stringByReplacingOccurrencesOfString:@"-" withString:@"+"] stringByReplacingOccurrencesOfString:@"_" withString:@"/"];
}

/**
 Método que convierte un "NSDictionary" en formato "properties" de java.
 Es necesario para que el servidor lo pueda interpretar.
 
 parámetros:
 -----------
 dict: Diccionario a convertir.
 
 */
+ (NSString*) dictionary2JavaProperties: (NSDictionary*) dict
{
    
    NSString *dataTransport = @"";
    
    for (id key in dict) {
        dataTransport = [dataTransport stringByAppendingString:key];
        dataTransport = [dataTransport stringByAppendingString:@"="];
        dataTransport = [dataTransport stringByAppendingString:[dict objectForKey:key]];
        dataTransport = [dataTransport stringByAppendingString:@"\n"];
    }
    dataTransport = [dataTransport stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
    NSData* data = [dataTransport dataUsingEncoding:NSUTF8StringEncoding];
    
    return [self encodeBase64:data];
}

/**
 Método que convierte un "properties" de java en "NSDictionary".
 Es necesario para que el cliente lo pueda interpretar.
 
 parámetros:
 -----------
 urlString: Cadena recibida del servidor y que contiene un properties de java.
 
 */
+(NSDictionary*) javaProperties2Dictionary:(NSString*) urlString {
    urlString = [urlString stringByReplacingOccurrencesOfString:@"\r" withString:@""] ;
    NSArray *listItems = [urlString componentsSeparatedByString:@"\n"];
    NSMutableDictionary *keyValues = [NSMutableDictionary dictionaryWithCapacity:listItems.count];
    for (NSString *item in listItems) {
        //NSLog(@"Item %@",item);
        @try
        {
            NSRange range = [item rangeOfString:@"="];
            if(range.length>0){
                NSString *parte1 = [item substringToIndex:range.location];
                NSString *parte2 = [item substringFromIndex:range.location+1];//le sumamos 1 para que no coja el "="
                [keyValues setObject:parte2 forKey:parte1];
            }
        }
        @catch(NSException * exception) {
            continue;
        }
                
    }
    return keyValues;
}

/**
 Método que comprueba si un algoritmo es válido.
 parámetros:
 -----------
 algorithm: Algoritmo utilizado para el cifrado.
 */
+(bool*) isValidAlgorithm:(NSString*)algorithm{
    bool isValid=false;
    if ([[algorithm uppercaseString] isEqualToString:@"SHA1WITHRSA"] ) {
        isValid=true;
    }
    else if ([[algorithm uppercaseString] isEqualToString:@"SHA256WITHRSA"] ) {
        isValid=true;
    }
    else if ([[algorithm uppercaseString] isEqualToString:@"SHA384WITHRSA"] ) {
        isValid=true;
    }
    else if ([[algorithm uppercaseString] isEqualToString:@"SHA512WITHRSA"] ) {
        isValid=true;
    }
    
    return isValid;
}

/**
 Método que transforma la url introducida en su correspondiente diccionario usando el par clave valor.  
 parámetros:
 -----------
 urlString: Url recibida y que necesita ser transformada a un diccionario.
 
 */
+(NSDictionary*) parseUrl:(NSString*) urlString {
    urlString =  [self decodeFromPercentEscapeString:urlString];
    NSString* webStringURL = [urlString stringByAddingPercentEscapesUsingEncoding:NSUTF8StringEncoding];
    NSURL * url = [NSURL URLWithString:webStringURL];
    NSArray *listItems = [[url query] componentsSeparatedByString:@"&"];
    NSMutableDictionary *keyValues = [NSMutableDictionary dictionaryWithCapacity:listItems.count];
    for (NSString *item in listItems) {
        NSRange range = [item rangeOfString:@"="];
        NSString *parte1 = [item substringToIndex:range.location];
        NSString *parte2 = [item substringFromIndex:range.location+1];//le sumamos 1 para que no coja el "="
        //omitimos el parámetro "op" ya no se va a usar
        if(![parte1 isEqualToString:PARAMETER_NAME_OPERATION])
            [keyValues setObject:parte2 forKey:parte1];
        
    }
    //if([keyValues objectForKey:PARAMETER_NAME_OPERATION]!=NULL)
        if([url host]!=NULL)
            [keyValues setObject:[url host] forKey:PARAMETER_NAME_OPERATION];
    return keyValues;
}

// Decode a percent escape encoded string.
+ (NSString*) decodeFromPercentEscapeString:(NSString *) string {
    return (NSString *) CFURLCreateStringByReplacingPercentEscapesUsingEncoding(NULL,(CFStringRef) string, CFSTR(""),kCFStringEncodingUTF8);
}

/**
 Método que crea una firma de tipo pkcs1 a partir del algoritmo especificado.
 parámetros:
 -----------
 algorithm: Algoritmo utilizado para el cifrado.
 privateKey: Clave privada necesaria para realizar la firma.
 dataPreSign: datos a firmar.
 */

+(NSData*) signPkcs1:(NSString*) algorithm privateKey:(SecKeyRef*)privateKey data:(NSData*)dataPreSign{
    //Con los datos de la prefirma decodificados, se pasa a realizar la firma pkcs1.
    CertificateUtils *certUtils = [[CertificateUtils alloc] init];
    [certUtils setPrivateKey:*privateKey];
    NSArray *listItems = [algorithm componentsSeparatedByString:@"with"];
    if([listItems count] >0){
        NSString *alg = [listItems objectAtIndex:0];
        if ([[alg uppercaseString] isEqualToString:@"SHA1"] ) {
            return [certUtils getSignatureBytesSHA1:dataPreSign];
        }
        else if ([[alg uppercaseString] isEqualToString:@"SHA256"]){
            return [certUtils getSignatureBytesSHA256:dataPreSign];
        }
        else if ([[alg uppercaseString] isEqualToString:@"SHA384"]){
            return [certUtils getSignatureBytesSHA384:dataPreSign];
        }
        else if ([[alg uppercaseString] isEqualToString:@"SHA512"]){
            return [certUtils getSignatureBytesSHA512:dataPreSign];
        }
        else{
            NSLog(@"Algoritmo no soportado: %@",alg);
            return NULL;
        }
    }
    else{
        NSLog(@"Algoritmo no reconocido: %@",algorithm);
        return NULL;
    }
}

/**
 Método que crea un hash a partir del algoritmo especificado.
 parámetros:
 -----------
 algorithm: Algoritmo utilizado para el cifrado.
 dataPreSign: datos a calcular el hash.
 */
+(NSData*) hashData:(NSString*) algorithm data:(NSData*)dataPreSign{
    //Con los datos de la prefirma decodificados, se pasa a realizar la firma pkcs1.
    CertificateUtils *certUtils = [[CertificateUtils alloc] init];
    NSArray *listItems = [algorithm componentsSeparatedByString:@"with"];
    if([listItems count] >0){
        NSString *alg = [listItems objectAtIndex:0];
        if ([[alg uppercaseString] isEqualToString:@"SHA1"] ) {
            return [certUtils getHashBytesSHA1:dataPreSign];
        }
        else if ([[alg uppercaseString] isEqualToString:@"SHA256"]){
            return [certUtils getHashBytesSHA256:dataPreSign];
        }
        else if ([[alg uppercaseString] isEqualToString:@"SHA384"]){
            return [certUtils getHashBytesSHA384:dataPreSign];
        }
        else if ([[alg uppercaseString] isEqualToString:@"SHA512"]){
            return [certUtils getHashBytesSHA512:dataPreSign];
        }
        else{
            NSLog(@"Algoritmo no soportado: %@",alg);
            return NULL;
        }
    }
    else{
        NSLog(@"Algoritmo no reconocido: %@",algorithm);
        return NULL;
    }
}


/**
 Método que obtiene los datos de un almacen de claves pkcs12.
 
 parámetros:
 -----------
 inPKCS12Data: Almacen de claves
 pass:         Contraseña del almacen de claves
 outIdentity:  Identidad
 outTrust:     Identidad
 
 resultado:
 ----------
 OSStatus : codigo de error.
 
 */
+ (OSStatus) extractIdentityAndTrust:(CFDataRef) inPKCS12Data :(NSString *)pass :(SecIdentityRef *)outIdentity :(SecTrustRef *)outTrust {
	OSStatus securityError = errSecSuccess;
	
	CFStringRef password = (CFStringRef)pass;
	const void *keys[] = {kSecImportExportPassphrase};
	const void *values[] = {password};
	
	CFDictionaryRef optionsDictionary = CFDictionaryCreate(NULL, keys, values, 1, NULL, NULL);
	
	CFArrayRef items = CFArrayCreate(NULL, 0, 0, NULL);
	securityError = SecPKCS12Import(inPKCS12Data, optionsDictionary, &items);
	
	if (securityError == 0) {
		CFDictionaryRef myIdentityAndTrust = CFArrayGetValueAtIndex(items, 0);
		const void *tempIdentity = NULL;
		tempIdentity = CFDictionaryGetValue (myIdentityAndTrust, kSecImportItemIdentity);
		*outIdentity = (SecIdentityRef)tempIdentity;
		const void *tempTrust = NULL;
		tempTrust = CFDictionaryGetValue(myIdentityAndTrust, kSecImportItemTrust);
		*outTrust = (SecTrustRef)tempTrust;
	}
	
	if (optionsDictionary) {
		CFRelease(optionsDictionary);
    }
	
	return securityError;
}

/**
 Método que cifra los datos usando el algoritmo de cifrado simétrico DES.
 
 parámetros:
 -----------
 data: Datos a cifrar.
 
 resultado:
 ----------
 NSData : Datos cifrados.
 
 */
+(NSData*)DesEncrypt:(NSString*)key :(NSData*)data {
    const void *vkey = (const void *) [key UTF8String];
    size_t dataMoved;
    NSMutableData *encryptedData = [NSMutableData dataWithLength:data.length + kCCBlockSizeDES];
    CCCryptorStatus result = CCCrypt(
                                     kCCEncrypt,                 // Op
                                     kCCAlgorithmDES,            // Alg
                                     kCCOptionECBMode,           // Options
                                     vkey,                       // Key
                                     kCCKeySizeDES,              // LeyLength
                                     NULL,                       // IV
                                     data.bytes,                 // DataIn
                                     data.length,                // DataInLength
                                     encryptedData.mutableBytes, // DataOut
                                     encryptedData.length,       // DataOutAvailable
                                     &dataMoved                  // DataOutMoved
                                     );
    if (result == kCCSuccess) {
        encryptedData.length = dataMoved;
        return encryptedData;
    }
    return nil;
}

/**
 Método que cifra los datos usando el algoritmo de cifrado simétrico DES.
 
 parámetros:
 -----------
 data: Datos a cifrar.
 
 resultado:
 ----------
 NSData : Datos cifrados.
 
 */
+(NSData*)DesDecrypt:(NSString*)key :(NSData*)data {
    const void *vkey = (const void *) [key UTF8String];
    size_t dataMoved;
    NSMutableData *encryptedData = [NSMutableData dataWithLength:data.length + kCCBlockSizeDES];
    CCCryptorStatus result = CCCrypt(
                                     kCCDecrypt,                 // Op
                                     kCCAlgorithmDES,            // Alg
                                     kCCOptionECBMode,           // Options
                                     vkey,                       // Key
                                     kCCKeySizeDES,              // LeyLength
                                     NULL,                       // IV
                                     data.bytes,                 // DataIn
                                     data.length,                // DataInLength
                                     encryptedData.mutableBytes, // DataOut
                                     encryptedData.length,       // DataOutAvailable
                                     &dataMoved                  // DataOutMoved
                                     );
    if (result == kCCSuccess) {
        encryptedData.length = dataMoved;
        return encryptedData;
    }
    return nil;
}

/**
 Método que obtiene el OID del algoritmo de firma.
 
 parámetros:
 -----------
 algorithm: algoritmo introducido por el usuario
 
 resultado:
 ----------
 char* : OID del algoritmo de firma.
 
 */
+(char*)getAlgorithmOID:(NSString*)algorithm {
    NSArray *listItems = [algorithm componentsSeparatedByString:@"with"];
    if([listItems count] >0){
        NSString *alg = [listItems objectAtIndex:0];
        if ([[alg uppercaseString] isEqualToString:@"SHA1"] ) {
            return SHA1_OID;
        }
        else if ([[alg uppercaseString] isEqualToString:@"SHA256"]){
            return SHA256_OID;
        }
        else if ([[alg uppercaseString] isEqualToString:@"SHA384"]){
            return SHA384_OID;
        }
        else if ([[alg uppercaseString] isEqualToString:@"SHA512"]){
            return SHA512_OID;
        }
        else{
            NSLog(@"Algoritmo no soportado: %@",alg);
            return NULL;
        }
    }
    else{
        NSLog(@"Algoritmo no reconocido: %@",algorithm);
        return NULL;
    }

}

/**
 Método que obtiene el OID del hash del algoritmo de firma.
 
 parámetros:
 -----------
 algorithm: algoritmo introducido por el usuario
 
 resultado:
 ----------
 char* : OID del hash del algoritmo de firma.
 
 */
+(char*)getHashAlgorithmOID:(NSString*)algorithm {
      
    if ([[algorithm uppercaseString] isEqualToString:@"SHA1"] ) {
        return SHA1_OID;
    }
    else if ([[algorithm uppercaseString] isEqualToString:@"SHA256"]){
        return SHA256_OID;
    }
    else if ([[algorithm uppercaseString] isEqualToString:@"SHA384"]){
        return SHA384_OID;
    }
    else if ([[algorithm uppercaseString] isEqualToString:@"SHA512"]){
        return SHA512_OID;
    }
    else{
        NSLog(@"Algoritmo no soportado: %@",algorithm);
        return NULL;
    }
        
}


/*
    Encoding y decoding b64
 */


static const char _base64EncodingTable[64] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
static const short _base64DecodingTable[256] = {
	-2, -2, -2, -2, -2, -2, -2, -2, -2, -1, -1, -2, -1, -1, -2, -2,
	-2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2,
	-1, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, 62, -2, -2, -2, 63,
	52, 53, 54, 55, 56, 57, 58, 59, 60, 61, -2, -2, -2, -2, -2, -2,
	-2,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14,
	15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, -2, -2, -2, -2, -2,
	-2, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
	41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, -2, -2, -2, -2, -2,
	-2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2,
	-2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2,
	-2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2,
	-2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2,
	-2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2,
	-2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2,
	-2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2,
	-2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2
};

+ (NSString *) base64EncodeString: (NSString *) strData {
	return [self base64EncodeData: [strData dataUsingEncoding: NSUTF8StringEncoding] ];
}

+ (NSString *) base64EncodeData: (NSData *) objData {
	const unsigned char * objRawData = [objData bytes];
	char * objPointer;
	char * strResult;
    
	// Get the Raw Data length and ensure we actually have data
	int intLength = [objData length];
	if (intLength == 0) return nil;
    
	// Setup the String-based Result placeholder and pointer within that placeholder
	strResult = (char *)calloc(((intLength + 2) / 3) * 4, sizeof(char));
	objPointer = strResult;
    
	// Iterate through everything
	while (intLength > 2) { // keep going until we have less than 24 bits
		*objPointer++ = _base64EncodingTable[objRawData[0] >> 2];
		*objPointer++ = _base64EncodingTable[((objRawData[0] & 0x03) << 4) + (objRawData[1] >> 4)];
		*objPointer++ = _base64EncodingTable[((objRawData[1] & 0x0f) << 2) + (objRawData[2] >> 6)];
		*objPointer++ = _base64EncodingTable[objRawData[2] & 0x3f];
        
		// we just handled 3 octets (24 bits) of data
		objRawData += 3;
		intLength -= 3;
	}
    
	// now deal with the tail end of things
	if (intLength != 0) {
		*objPointer++ = _base64EncodingTable[objRawData[0] >> 2];
		if (intLength > 1) {
			*objPointer++ = _base64EncodingTable[((objRawData[0] & 0x03) << 4) + (objRawData[1] >> 4)];
			*objPointer++ = _base64EncodingTable[(objRawData[1] & 0x0f) << 2];
			*objPointer++ = '=';
		} else {
			*objPointer++ = _base64EncodingTable[(objRawData[0] & 0x03) << 4];
			*objPointer++ = '=';
			*objPointer++ = '=';
		}
	}
    
	// Terminate the string-based result
	*objPointer = '\0';
    
	// Return the results as an NSString object
	return [NSString stringWithCString:strResult encoding:NSASCIIStringEncoding];
}

+ (NSData *) base64DecodeString: (NSString *) strBase64 {
	const char * objPointer = [strBase64 cStringUsingEncoding:NSASCIIStringEncoding];
	int intLength = strlen(objPointer);
	int intCurrent;
	int i = 0, j = 0, k;
    
	unsigned char * objResult;
	objResult = calloc(intLength, sizeof(char));
    
	// Run through the whole string, converting as we go
	while ( ((intCurrent = *objPointer++) != '\0') && (intLength-- > 0) ) {
		if (intCurrent == '=') {
			if (*objPointer != '=' && ((i % 4) == 1)) {// || (intLength > 0)) {
				// the padding character is invalid at this point -- so this entire string is invalid
				free(objResult);
				return nil;
			}
			continue;
		}
        
		intCurrent = _base64DecodingTable[intCurrent];
		if (intCurrent == -1) {
			// we're at a whitespace -- simply skip over
			continue;
		} else if (intCurrent == -2) {
			// we're at an invalid character
			free(objResult);
			return nil;
		}
        
		switch (i % 4) {
			case 0:
				objResult[j] = intCurrent << 2;
				break;
                
			case 1:
				objResult[j++] |= intCurrent >> 4;
				objResult[j] = (intCurrent & 0x0f) << 4;
				break;
                
			case 2:
				objResult[j++] |= intCurrent >>2;
				objResult[j] = (intCurrent & 0x03) << 6;
				break;
                
			case 3:
				objResult[j++] |= intCurrent;
				break;
		}
		i++;
	}
    
	// mop things up if we ended on a boundary
	k = j;
	if (intCurrent == '=') {
		switch (i % 4) {
			case 1:
				// Invalid state
				free(objResult);
				return nil;
                
			case 2:
				k++;
				// flow through
			case 3:
				objResult[k] = 0;
		}
	}
    
	// Cleanup and setup the return NSData
	NSData * objData = [[[NSData alloc] initWithBytes:objResult length:j] autorelease];
	free(objResult);
	return objData;
}


@end
