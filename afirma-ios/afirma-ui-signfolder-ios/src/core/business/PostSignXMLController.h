//
//  PostSignXMLController.h
//  WSFirmaClient
//
//  Created by Antonio Fiñana on 05/11/12.
//
//

#import <Foundation/Foundation.h>
#import "PFRequest.h"
#import "Document.h"
#import "XMLController.h"

@interface PostSignXMLController : XMLController <NSXMLParserDelegate> {
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
}

@property (nonatomic, strong) NSArray *dataSource;

- (PostSignXMLController *)initXMLParser;

// Builds Web Service Request message
+ (NSString *)buildRequestWithCert:(NSString *)cert witRequestList:(NSArray *)requestArr;

@end
