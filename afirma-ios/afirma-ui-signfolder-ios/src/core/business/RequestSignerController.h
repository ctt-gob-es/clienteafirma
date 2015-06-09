//
//  RequestSignerController.h
//  PortaFirmasUniv
//
//  Created by Antonio Fiñana Sánchez on 14/11/12.
//  Copyright (c) 2012 Atos. All rights reserved.
//

#import "WSDataController.h"
#import "CertificateUtils.h"
#import "Detail.h"
#import "Document.h"

@protocol RequestSignerEvent <NSObject>

- (void)didReceiveSignerRequestResult:(NSArray *)requestsSigned;

@optional
- (void)didReceiveError:(NSString *)errorString;

@end

@interface RequestSignerController : NSObject<WSDelegate>
{
    NSArray *preSignRequests;
    NSArray *_dataSource;

    BOOL waitingPreSign;
    BOOL waitingPostSign;

    CertificateUtils *_certificate;
    WSDataController *_wsController;

}
@property (nonatomic, strong) id <RequestSignerEvent> delegate;
@property (nonatomic, strong) NSArray *dataSource;

- (void)loadPreSignRequestsWithCurrentCertificate:(NSArray *)requests;
- (void)loadPreSignDetailWithCurrentCertificate:(Detail *)detail;
- (void)loadPostSignRequest:(NSArray *)requests;

- (void)cancelConnection;
- (void)didReceiveParserWithError:(NSString *)errorString;
- (void)doParse:(NSData *)data;

// Sign list of requests
- (void)signRequestList:(NSArray *)requests;
// Sign selected document
- (void)signDocument:(Document *)reqDoc;

// Testing
- (void)showSignature:(NSString *)dataStr withCertificate:(CertificateUtils *)certificate withMdalgo:(NSString *)mdalgo;

@end
