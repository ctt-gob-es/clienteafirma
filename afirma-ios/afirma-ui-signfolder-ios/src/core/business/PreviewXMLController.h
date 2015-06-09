//
//  PreviewXMLController.h
//  TopSongs
//
//  Created by Antonio Fiñana on 31/10/12.
//
//

#import <Foundation/Foundation.h>

#import "Document.h"
#import "Preview.h"
#import "XMLController.h"

@interface PreviewXMLController : XMLController <NSXMLParserDelegate> {
    
    // user object
    Preview *_dataSource;
  
}

- (PreviewXMLController *) initXMLParser;
// Builds Web Service Request message
+(NSString *) buildRequestWithId:(NSString *)id;
@property (strong,nonatomic) Preview* dataSource;
@end
