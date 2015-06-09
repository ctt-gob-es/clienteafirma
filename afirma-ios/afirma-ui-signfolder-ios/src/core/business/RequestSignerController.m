//
//  RequestSignerController.m
//  PortaFirmasUniv
//
//  Created by Antonio Fiñana Sánchez on 14/11/12.
//  Copyright (c) 2012 Atos. All rights reserved.
//

#import "RequestSignerController.h"
#import "WSDataController.h"
#import "AppDelegate.h"
#import "PreSignXMLController.h"
#import "PostSignXMLController.h"
#import "CertificateUtils.h"
#import "NSData+Base64.h"
#import "Detail.h"
#import "PFRequest.h"
#import "Document.h"
#import "Param.h"
#import "Base64Utils.h"

@implementation RequestSignerController

- (id)init
{
    self = [super init];

    if (self) {
        // Sets data in RequestSignerController objet
        // BORRAR :true
        _wsController = [[WSDataController alloc] init:true];
        _wsController.delegate = self;
        waitingPreSign = NO;
        waitingPostSign = NO;
    }

    return self;
}

- (void)loadPreSignRequestsWithCurrentCertificate:(NSArray *)requests
{
    T21LogDebug(@"RequestSignerController::loadPreSignRequestsWithCurrentCertificate...");
    waitingPreSign = TRUE;

    NSData *certificateData = [[CertificateUtils sharedWrapper] publicKeyBits];
    NSString *certificateB64 = [Base64Utils base64EncodeData:certificateData];
    NSString *data = [PreSignXMLController buildRequestWithCert:certificateB64 witRequestList:requests];

    T21LogDebug(@"RequestSignerController::loadPreSignRequestsWithCurrentCertificate data=%@", data);

    _wsController.delegate = self;
    [_wsController loadPostRequestWithData:data code:0];
    [_wsController startConnection];
}

- (void)loadPreSignDetailWithCurrentCertificate:(Detail *)detail
{
    T21LogDebug(@"RequestSignerController::loadPreSignDetailWithCurrentCertificate...");

    PFRequest *request = [[PFRequest alloc] init];
    request.reqid = detail.detailid;
    request.documents = [detail.documents mutableCopy];
    [self loadPreSignRequestsWithCurrentCertificate:@[request]];
}

- (void)loadPostSignRequest:(NSArray *)requests
{
    T21LogDebug(@"RequestSignerController::loadPostSignRequest");

    // load Pre Sign Request
    [self signRequestList:requests];

    NSData *certificateData = [CertificateUtils sharedWrapper].publicKeyBits;
    // dataFromBase64String
    // NSString *certificateB64 = [certificateData base64EncodedString];
    NSString *certificateB64 = [Base64Utils base64EncodeData:certificateData];
    NSString *data = [PostSignXMLController buildRequestWithCert:certificateB64 witRequestList:requests];

    T21LogDebug(@"loadPreSignRequest::loadPostSignRequest data=%@", data);

    waitingPostSign = YES;

    [_wsController loadPostRequestWithData:data code:1];
    [_wsController startConnection];
}

- (void)cancelConnection
{
    [_wsController cancelConnection];
    waitingPostSign = waitingPreSign = NO;
}

- (void)didReceiveParserWithError:(NSString *)errorString
{
    if (waitingPreSign) {
        waitingPreSign = NO;
        T21LogDebug(@"RequestSignerController::didReceiveParserWithError PreSign message: %@", errorString);
    }

    if (waitingPostSign) {
        waitingPostSign = NO;
        T21LogDebug(@"RequestSignerController::didReceiveParserWithError PostSign message: %@", errorString);
    }

    [[self delegate] didReceiveError:errorString];
}

- (void)doParse:(NSData *)data
{
    NSString *responseString = [[NSString alloc] initWithData:data encoding:NSNonLossyASCIIStringEncoding];

    T21LogDebug(@"respuesta del servidor: %@", responseString);

    if (waitingPreSign) {
        waitingPreSign = NO;
        // create and init NSXMLParser object
        NSXMLParser *nsXmlParser = [[NSXMLParser alloc] initWithData:data];

        // create and init our delegate
        PreSignXMLController *parser = [[PreSignXMLController alloc] initXMLParser];

        // set delegate
        [nsXmlParser setDelegate:parser];

        // parsing...
        BOOL success = [nsXmlParser parse];

        // test the result
        if (success) {
            T21LogDebug(@"doParse:: Parsing PreSign XML with no errors ");

            // parsing...

            BOOL finishWithError = [parser finishWithError];

            if (finishWithError) {
                T21LogError(@"Error  parsing  document!");
                [[self delegate] didReceiveError:[NSString stringWithFormat:@"Se ha producido un error en el servidor:%@(%@)", [parser err], [parser errorCode]]];

                return;
            }

            // get array of users here
            preSignRequests = [parser dataSource];

            [self loadPostSignRequest:preSignRequests];
        } else {
            T21LogError(@"doParse::Error  parsing PreSign document!");
            [[self delegate] didReceiveError:@"Se ha producido un error de conexión con el servidor"];
        }
    } else if (waitingPostSign) {
        waitingPostSign = NO;
        // create and init NSXMLParser object
        NSXMLParser *nsXmlParser = [[NSXMLParser alloc] initWithData:data];

        // create and init our delegate
        PostSignXMLController *parser = [[PostSignXMLController alloc] initXMLParser];

        // set delegate
        [nsXmlParser setDelegate:parser];

        // parsing...
        BOOL success = [nsXmlParser parse];

        // test the result
        if (success) {
            T21LogDebug(@"doParse:: Parsing XML with no errors ");
            // get array of users here
            _dataSource = [parser dataSource];
            [[self delegate] didReceiveSignerRequestResult:_dataSource];
        } else {
            T21LogError(@"doParse::Error  parsing PreSign document!");
            [[self delegate] didReceiveError:@"Se ha producido un error de conexión con el servidor"];
        }
    }
}

// Sign list of requests
- (void)signRequestList:(NSArray *)requests
{
    for (int i = 0; i < [requests count]; i++) {
        PFRequest *_selRequest = [requests objectAtIndex:i];
        NSArray *documents = _selRequest.documents;
        for (int j = 0; j < [documents count]; j++) {
            [self signDocument:[documents objectAtIndex:j]];
        }
    }
}

// Sign selected document
- (void)signDocument:(Document *)reqDoc
{
    NSString *mdalgo = [reqDoc mdalgo];

    if (mdalgo == nil) {
        mdalgo = @"sha1";
    } else {
        mdalgo = [mdalgo lowercaseString];
    }

    T21LogDebug(@"ssconfig count: %lu", (unsigned long)[reqDoc.ssconfig count]);
    NSString *preSignResult = nil;
    for (int i = 0; i < [reqDoc.ssconfig count]; i++) {
        Param *param = [reqDoc.ssconfig objectAtIndex:i];
        T21LogDebug(@"Session param: %@", param.key);

        if ([param.key hasPrefix:@"pre."]) {
            preSignResult = param.value;
            break;
        }
    }

    // NSData *data = [NSData dataFromBase64String:preSignResult];
    NSData *data = [Base64Utils base64DecodeString:preSignResult];
    NSData *result = nil;
    T21LogDebug(@"signDocument::mdalgo=%@", mdalgo);

    if ([mdalgo isEqualToString:@"sha-1"] || [mdalgo isEqualToString:@"sha1"]) {
        result = [[CertificateUtils sharedWrapper] getSignatureBytesSHA1:data];
    } else if ([mdalgo isEqualToString:@"sha-256"] || [mdalgo isEqualToString:@"sha256"]) {
        result = [[CertificateUtils sharedWrapper] getSignatureBytesSHA256:data];
    } else if ([mdalgo isEqualToString:@"sha-384"] || [mdalgo isEqualToString:@"sha384"]) {
        result = [[CertificateUtils sharedWrapper] getSignatureBytesSHA384:data];
    } else if ([mdalgo isEqualToString:@"sha-512"] || [mdalgo isEqualToString:@"sha512"]) {
        result = [[CertificateUtils sharedWrapper] getSignatureBytesSHA512:data];
    } else {
        T21LogDebug(@"RequestController::signDocument mdalgo error =%@", mdalgo);
    }

    // reqDoc.result=[result base64EncodedString];
    reqDoc.result = [Base64Utils base64EncodeData:result];
}

- (void)showSignature:(NSString *)dataStr withCertificate:(CertificateUtils *)certificate withMdalgo:(NSString *)mdalgo
{
    //  NSData *contentData = [NSData dataFromBase64String:_dataSource.data];
    NSData *data = [dataStr
                    dataUsingEncoding:NSUTF8StringEncoding];

    NSData *result = nil;

    if (mdalgo == nil) {
        mdalgo = @"sha1";
    } else {
        mdalgo = [mdalgo lowercaseString];
    }

    T21LogDebug(@"show Signature RSA::>>>>>>>>>>>>>>mdalgo=%@", mdalgo);

    if ([mdalgo isEqualToString:@"sha-1"]) {
        result = [[CertificateUtils sharedWrapper] getSignatureBytesSHA1:data];
    } else if ([mdalgo isEqualToString:@"sha-256"]) {
        result = [[CertificateUtils sharedWrapper] getSignatureBytesSHA256:data];
    } else if ([mdalgo isEqualToString:@"sha-384"]) {
        result = [[CertificateUtils sharedWrapper] getSignatureBytesSHA384:data];
    } else if ([mdalgo isEqualToString:@"sha-512"]) {
        result = [[CertificateUtils sharedWrapper] getSignatureBytesSHA512:data];
    } else {
        T21LogDebug(@"RequestController::signDocument mdalgo error =%@", mdalgo);
    }

    // NSString *resBase64=[result base64EncodedString];
    NSString *resBase64 = [Base64Utils base64EncodeData:result];
    T21LogDebug(@"showSignature::RSA Digital sign:%@ length=%lu", resBase64, (unsigned long)[resBase64 length]);
}

@end
