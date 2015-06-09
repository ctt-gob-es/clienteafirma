//
//  PreSignXMLController.h
//  WSFirmaClient
//
//  Created by Antonio Fiñana on 05/11/12.
//
//

#import <Foundation/Foundation.h>
#import "XMLController.h"

@class PFRequest;
@class Document;
@class Param;

@interface PreSignXMLController : XMLController <NSXMLParserDelegate>
{
    // user object
    PFRequest *request;
    // array of requests objects
    NSMutableArray *_dataSource;

    // waiting for document
    BOOL waitingForDocument;

    // user object
    Document *document;
    // array of user objects
    NSMutableArray *documentList;
    // Param of document
    Param *ssparam;
}

@property (strong, nonatomic) NSMutableArray *dataSource;

- (PreSignXMLController *)initXMLParser;

// Builds Web Service Request message
+ (NSString *)buildRequestWithCert:(NSString *)cert witRequestList:(NSArray *)requestArr;

@end
