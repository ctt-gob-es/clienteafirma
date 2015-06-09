//
//  PreviewXMLController.m
//  TopSongs
//
//  Created by Antonio Fiñana on 31/10/12.
//
//

#import "PreviewXMLController.h"
#import "PFRequest.h"
#import "Document.h"
#import "CertificateUtils.h"
#import "NSData+Base64.h"

@implementation PreviewXMLController

@synthesize dataSource = _dataSource;

- (PreviewXMLController *)initXMLParser
{
    self = [super init];
    // init array of user objects

    return self;
}

// Builds Web Service Request message
+ (NSString *)buildRequestWithId:(NSString *)id
{
    NSMutableString *mesg = [[NSMutableString alloc] initWithString:@"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"];

    [mesg appendFormat:@"<rqtprw docid=\"%@\">\n", id];
    // CERTIFICADO
    CertificateUtils *cert = [CertificateUtils sharedWrapper];
    NSString *certificado = [NSData base64EncodeData:[cert publicKeyBits]];
    // Formats lists message
    NSMutableString *certlabel = [[NSMutableString alloc] initWithString:@"<cert>\n"];
    [certlabel appendFormat:@"%@\n", certificado];
    [certlabel appendString:@"</cert>\n"];
    [mesg appendString:certlabel];
    [mesg appendFormat:@"</rqtprw>\n"];

    return mesg;
}

// Parse the start of an element
- (void)     parser:(NSXMLParser *)parser
    didStartElement:(NSString *)elementName
       namespaceURI:(NSString *)namespaceURI
      qualifiedName:(NSString *)qualifiedName
         attributes:(NSDictionary *)attributeDict
{
    [super parser:parser didStartElement:elementName namespaceURI:namespaceURI qualifiedName:qualifiedName attributes:attributeDict];

    if ([elementName isEqualToString:@"prw"]) {
        T21LogDebug(@"user element found – create a new instance of prw class...");

        _dataSource = [[Preview alloc] init];
        // We do not have any attributes in the user elements, but if
        // you do, you can extract them here:
        _dataSource.docid = [attributeDict objectForKey:@"docid"];
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

    if (!currentElementValue) {
        // init the ad hoc string with the value initWithData:xmlData encoding:NSUTF8StringEncoding
        currentElementValue = [[NSMutableString alloc] initWithString:strNew];
    } else {
        // append value to the ad hoc string
        [currentElementValue appendString:strNew];
    }
}

// XMLParser.m
- (void)   parser:(NSXMLParser *)parser
    didEndElement:(NSString *)elementName
     namespaceURI:(NSString *)namespaceURI
    qualifiedName:(NSString *)qName
{
    T21LogDebug(@"PreviewXMLController::parser didEndElement=%@", elementName);
    [super parser:parser didEndElement:elementName namespaceURI:namespaceURI qualifiedName:qName];

    if ([elementName isEqualToString:@"prw"]) {

        // We reached the end of the XML document
        return;
    } else {
        T21LogDebug(@"PreviewXMLController::element name=%@", elementName);
        [_dataSource setValue:currentElementValue forKey:elementName];
    }

    // [currentElementValue release];
    currentElementValue = nil;
}

@end
