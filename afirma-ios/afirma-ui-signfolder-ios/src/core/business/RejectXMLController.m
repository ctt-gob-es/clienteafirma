//
//  RejectXMLController.m
//  WSFirmaClient
//
//  Created by Antonio Fiñana on 05/11/12.
//
//

#import "RejectXMLController.h"
#import "PFRequestResult.h"
#import "Detail.h"
#import "PFRequest.h"
#import "CertificateUtils.h"
#import "NSData+Base64.h"

@implementation RejectXMLController

@synthesize dataSource = _dataSource;

// Builds Web Service Request message
+ (NSString *)buildRequestWithIds:(NSArray *)rjctIds
{
    NSMutableString *mesg = [[NSMutableString alloc] initWithString:@"<?xml version=\"1.0\" encoding=\"UTF-8\"?><reqrjcts> \n"];

    // CERTIFICADO
    CertificateUtils *cert = [CertificateUtils sharedWrapper];
    NSString *certificado = [NSData base64EncodeData:[cert publicKeyBits]];
    // Formats lists message
    NSMutableString *certlabel = [[NSMutableString alloc] initWithString:@"<cert>\n"];

    [certlabel appendFormat:@"%@\n", certificado];
    [certlabel appendString:@"</cert>\n"];
    [mesg appendString:certlabel];

    // Formats lists message
    NSMutableString *reqrjcts = [[NSMutableString alloc] initWithString:@""];
    [reqrjcts appendFormat:@"\t<rjcts>\n"];
    for (int i = 0; i < [rjctIds count]; i++) {
        [reqrjcts appendFormat:@"\t<rjct id=\"%@\" />\n", [rjctIds[i] reqid]];
    }
    [reqrjcts appendFormat:@"\t</rjcts>\n"];

    [mesg appendString:reqrjcts];
    [mesg appendString:@"</reqrjcts>"];

    return mesg;
}

- (RejectXMLController *)initXMLParser
{
    self = [super init];
    // init array of user objects
    _dataSource = nil;

    return self;
}

// Parse the start of an element
- (void)     parser:(NSXMLParser *)parser
    didStartElement:(NSString *)elementName
       namespaceURI:(NSString *)namespaceURI
      qualifiedName:(NSString *)qualifiedName
         attributes:(NSDictionary *)attributeDict
{
    [super parser:parser didStartElement:elementName namespaceURI:namespaceURI qualifiedName:qualifiedName attributes:attributeDict];

    if ([elementName isEqualToString:@"rjct"]) {
        T21LogDebug(@"user element found – create a new instance of rjct class...");

        reject = [[PFRequestResult alloc] init];
        // We do not have any attributes in the user elements, but if
        // you do, you can extract them here:
        reject.rejectid = [attributeDict objectForKey:@"id"];
        reject.status = [attributeDict objectForKey:@"status"];
    }

    if ([elementName isEqualToString:@"rjcts"]) {
        T21LogDebug(@"user element found – create a new instance of rjcts list class...");
        _dataSource = [[NSMutableArray alloc] init];
    }
}

// Parse an element value
- (void)parser:(NSXMLParser *)parser foundCharacters:(NSString *)string
{
    NSString *strNew = [string stringByReplacingOccurrencesOfString:@"\n" withString:@""];

    strNew = [strNew stringByReplacingOccurrencesOfString:@"\t" withString:@""];

    if ([strNew isEqualToString:@"\n"]) {
        return;
    }

    if (currentElementValue) {
        [currentElementValue appendString:strNew];
    } else {
        currentElementValue = [strNew mutableCopy];
    }
}

// XMLParser.m
- (void)parser:(NSXMLParser *)parser didEndElement:(NSString *)elementName namespaceURI:(NSString *)namespaceURI qualifiedName:(NSString *)qName
{
    T21LogDebug(@"parser didEndElement=%@", elementName);
    [super parser:parser didEndElement:elementName namespaceURI:namespaceURI qualifiedName:qName];

    if ([elementName isEqualToString:@"rjcts"]) {

        // We reached the end of the XML document
        return;
    }

    if ([elementName isEqualToString:@"rjct"]) {
        // We reached the end of the XML document
        [_dataSource addObject:reject];
        reject = nil;

        return;
    }

    currentElementValue = nil;
}

// end of XMLParser.m file
@end
