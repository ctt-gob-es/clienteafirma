//
//  DetailXMLController.m
//  TopSongs
//
//  Created by Antonio Fiñana on 31/10/12.
//
//

#import "DetailXMLController.h"

#import "Detail.h"
#import "SignLine.h"
#import "Document.h"
#import "CertificateUtils.h"
#import "NSData+Base64.h"

@implementation DetailXMLController

@synthesize dataSource = _detail;

// Builds Web Service Request message
+ (NSString *)buildRequestWithId:(NSString * )rqdtlid
{
    NSMutableString *mesg = [[NSMutableString alloc] initWithString:@"<?xml version=\"1.0\" encoding=\"UTF-8\"?>"];

    [mesg appendFormat:@"<rqtdtl id=\"%@\">\n", rqdtlid];
    // CERTIFICADO
    CertificateUtils *cert = [CertificateUtils sharedWrapper];
    NSString *certificado = [NSData base64EncodeData:[cert publicKeyBits]];
    // Formats lists message
    NSMutableString *certlabel = [[NSMutableString alloc] initWithString:@"<cert>\n"];
    [certlabel appendFormat:@"%@\n", certificado];
    [certlabel appendString:@"</cert>\n"];
    [mesg appendString:certlabel];
    [mesg appendFormat:@"</rqtdtl>\n"];

    return mesg;
}

- (DetailXMLController *)initXMLParser
{
    self = [super init];

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

    if ([elementName isEqualToString:@"dtl"]) {
        T21LogDebug(@"user element found – create a new instance of dtl class...");

        _detail = [[Detail alloc] initWithDict:attributeDict];
        waitingForDocument = FALSE;
    }

    if ([elementName isEqualToString:@"snders"]) {
        T21LogDebug(@"user element found – create a new instance of snders list class...");
        waitingForSenders = YES;
        senders = [[NSMutableArray alloc ]init];

    }

    if ([elementName isEqualToString:@"sgnlines"]) {
        T21LogDebug(@"user element found – create a new instance of signlines list class...");

        signlines = [[NSMutableArray alloc ]init];
    }

    if ([elementName isEqualToString:@"sgnline"]) {
        T21LogDebug(@"user element found – create a new instance of sgnline class...");

        waitingForSignline = YES;
        signline = [[SignLine alloc ]init];

        signline.receivers = [[NSMutableArray alloc] init];
    }

    if ([elementName isEqualToString:@"docs"]) {
        T21LogDebug(@"user element found – create a new instance of docs list class...");

        documents = [[NSMutableArray alloc ]init];
    }

    if ([elementName isEqualToString:@"doc"]) {
        T21LogDebug(@"user element found – create a new instance of doc class...");
        // We reached the end of the XML document
        waitingForDocument = YES;
        document = [[Document alloc ]init];

        document.docid = [attributeDict objectForKey:@"docid"];
        T21LogDebug(@"user element found – document id=%@", [attributeDict objectForKey:@"docid"]);
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
        currentElementValue = [[NSMutableString alloc] initWithString:strNew ];
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
    T21LogDebug(@"parser didEndElement=%@", elementName);

    if ([elementName isEqualToString:@"dtl"]) {

        // We reached the end of the dtl element
        return;
    }

    if ([elementName isEqualToString:@"snders"] ) {
        // We reached the end of the XML document
        _detail.senders = senders;
        senders = nil;
        waitingForSenders = NO;

        return;
    }

    if ([elementName isEqualToString:@"docs"]) {
        // We reached the end of the XML document
        _detail.documents = documents;
        // [documents release];
        documents = nil;

        return;
    }

    if ([elementName isEqualToString:@"sgnlines"] ) {
        // We reached the end of the XML document
        _detail.signlines = signlines;
        signlines = nil;

        return;
    }

    if ([elementName isEqualToString:@"sgnline"] ) {
        // We reached the end of the XML document
        waitingForSignline = NO;
        [signlines addObject:signline];
        signline = nil;

        return;
    }

    if ([elementName isEqualToString:@"doc"]) {
        // We are done with user entry – add the parsed user
        // object to our user array
        waitingForDocument = NO;
        [documents addObject:document];
        // [document release];
        document = nil;

        return;
    }

    // The parser hit one of the element values.
    // This syntax is possible because User object
    // property names match the XML user element names
    if (waitingForDocument) {
        [document setValue:currentElementValue forKey:elementName];

    } else if (waitingForSignline) {
        [signline.receivers addObject:currentElementValue];

    } else if (waitingForSenders) {
        [senders addObject:currentElementValue];

    } else {
        [_detail setValue:currentElementValue forKey:elementName];
    }

    currentElementValue = nil;
}

@end
