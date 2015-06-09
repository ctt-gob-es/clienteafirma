//
//  RequestListXMLController.h
//  TopSongs
//
//  Created by Antonio Fiñana on 31/10/12.
//
//

#import <Foundation/Foundation.h>
#import "XMLController.h"

@class PFRequest;
@class Document;

static const int kRequestListXMLControllerPageSize = 50;

@interface RequestListXMLController : XMLController <NSXMLParserDelegate> {
    // user object
    PFRequest *request;
    // array of user objects
    NSMutableArray *_dataSource;

    // waiting for document
    BOOL waitingForDocument;

    // user object
    Document *document;
    // array of user objects
    NSMutableArray *documentList;
}

@property (nonatomic, retain) NSMutableArray *dataSource;

- (RequestListXMLController *)initXMLParser;
+ (NSString *)buildDefaultRequestWithState:(NSString *)state pageNumber:(int)pageNumber filters:(NSDictionary *)filters;

@end
