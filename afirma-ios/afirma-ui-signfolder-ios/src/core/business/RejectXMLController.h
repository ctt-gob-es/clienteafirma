//
//  RejectXMLController.h
//  WSFirmaClient
//
//  Created by Antonio Fiñana on 05/11/12.
//
//

#import <Foundation/Foundation.h>
#import "XMLController.h"
#import "PFRequestResult.h"

@interface RejectXMLController : XMLController <NSXMLParserDelegate>
{
    // Reject object
    PFRequestResult *reject;
    // Reject list
    // an ad hoc string to hold element value
    NSMutableArray *_dataSource;
}

@property (nonatomic, retain) NSMutableArray *dataSource;
- (RejectXMLController *)initXMLParser;
// Builds Web Service Request message
+ (NSString *)buildRequestWithIds:(NSArray *)rjctIds;
// +(NSString *) buildRequestWithDetailId:(NSString *) reqId;

@end
