//
//  RequestListXMLController.m
//  TopSongs
//
//  Created by Antonio Fiñana on 31/10/12.
//
//

#import "RequestListXMLController.h"
#import "PFRequest.h"
#import "Document.h"
#import "CertificateUtils.h"
#import "NSData+Base64.h"

@implementation RequestListXMLController
@synthesize dataSource = _dataSource;

+ (NSString *)buildDefaultRequestWithState:(NSString *)state pageNumber:(int)pageNumber filters:(NSDictionary *)filters
{
    NSArray *formatArr = [[NSArray alloc] initWithObjects:@"PAdES", @"XAdES", @"PDF", @"CAdES", nil];

    NSString *result = [RequestListXMLController buildRequestWithState:state format:formatArr filters:[self processedFilters:filters] pageNumber:pageNumber];

    return result;
}

// Builds Web Service Request message
+ (NSString *)buildRequestWithState:(NSString *)state format:(NSArray *)formatArr filters:(NSDictionary *)filters pageNumber:(int)pageNumber
{
    NSMutableString *mesg = [[NSMutableString alloc] initWithFormat:@"<?xml version=\"1.0\" encoding=\"UTF-8\"?><rqtlst state=\"%@\" pg=\"%d\" sz=\"%d\">\n", state, pageNumber, kRequestListXMLControllerPageSize];

    // CERTIFICADO
    CertificateUtils *cert = [CertificateUtils sharedWrapper];
    NSString *certificado = [NSData base64EncodeData:[cert publicKeyBits]];
    // Formats lists message
    NSMutableString *certlabel = [[NSMutableString alloc] initWithString:@"<cert>\n"];

    [certlabel appendFormat:@"%@\n", certificado];
    [certlabel appendString:@"</cert>\n"];
    [mesg appendString:certlabel];

    NSMutableString *fmts = [[NSMutableString alloc] initWithString:@"<fmts>\n"];
    for (int i = 0; i < [formatArr count]; i++) {
        [fmts appendFormat:@"<fmt>%@</fmt>\n", (NSString *)formatArr[i]];
    }
    [fmts appendString:@"</fmts>\n"];
    [mesg appendString:fmts];

    // Filters list message
    NSMutableString *fltrs = [[NSMutableString alloc] initWithString:@"<fltrs>"];
    for (NSString *key in filters.allKeys) {
        [fltrs appendFormat:@"\n<fltr>\n<key>%@</key>\n<value>%@</value></fltr>\n",
         key, filters[key]];
    }
    [fltrs appendString:@"</fltrs>\n"];
    [mesg appendString:fltrs];
    [mesg appendString:@"</rqtlst>\n"];

    return mesg;
}

+ (NSDictionary *)processedFilters:(NSDictionary *)filters
{
    NSMutableDictionary *processedFilters;

    if (filters && filters.count > 0) {
        processedFilters = [filters mutableCopy];
        NSArray *filtersKeys = filters.allKeys;

        if (![filtersKeys containsObject:kPFFilterKeySortCriteria]) {
            processedFilters[kPFFilterKeySortCriteria] = kPFFilterValueSortCriteriaDate;
            processedFilters[kPFFilterKeySort] = kPFFilterValueSortDesc;
        }
    } else {
        processedFilters = [@{} mutableCopy];
        processedFilters[kPFFilterKeySortCriteria] = kPFFilterValueSortCriteriaDate;
        processedFilters[kPFFilterKeySort] = kPFFilterValueSortDesc;
    }

    return processedFilters;
}

- (RequestListXMLController *)initXMLParser
{
    self = [super init];
    // init array of user objects
    _dataSource = [[NSMutableArray alloc] init];

    return self;
}

// Parse the start of an element
- (void)parser:(NSXMLParser *)parser didStartElement:(NSString *)elementName namespaceURI:(NSString *)namespaceURI qualifiedName:(NSString *)qualifiedName attributes:(NSDictionary *)attributeDict
{
    [super parser:parser didStartElement:elementName namespaceURI:namespaceURI qualifiedName:qualifiedName attributes:attributeDict];

    T21LogDebug(@"parser startElement=%@", elementName);

    if ([elementName isEqualToString:@"rqt"]) {
        T21LogDebug(@"user element found – create a new instance of rqt class...");

        request = [[PFRequest alloc] initWithDict:attributeDict];
        waitingForDocument = FALSE;
    }

    if ([elementName isEqualToString:@"docs"]) {
        T21LogDebug(@"user element found – create a new instance of docs list class...");
        documentList = [@[] mutableCopy];
    }

    if ([elementName isEqualToString:@"doc"]) {
        T21LogDebug(@"user element found – create a new instance of document class...");
        // We reached the end of the XML document
        waitingForDocument = YES;
        document = [[Document alloc]init];
        document.docid = [attributeDict objectForKey:@"docid"];
        document.sigfrmt = [attributeDict objectForKey:@"sigfrmt"];
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
    T21LogDebug(@"parser didEndElement=%@", elementName);

    [super parser:parser didEndElement:elementName namespaceURI:namespaceURI qualifiedName:qName];

    if ([elementName isEqualToString:@"list"]) {

        // We reached the end of the XML document
        return;
    }

    if ([elementName isEqualToString:@"doc"]) {
        // We reached the end of the XML document
        waitingForDocument = NO;
        [documentList addObject:document];
        document = nil;

        return;
    }

    if ([elementName isEqualToString:@"docs"]) {
        // We reached the end of the XML document
        request.documents = documentList;
        documentList = nil;

        return;
    }

    if ([elementName isEqualToString:@"rqt"]) {
        // We are done with user entry – add the parsed user
        // object to our user array
        [_dataSource addObject:request];
        // release user object
        documentList = nil;
        request = nil;

        return;
    }

    // The parser hit one of the element values.
    // This syntax is possible because User object
    // property names match the XML user element names

    if (waitingForDocument) {
        [document setValue:currentElementValue forKey:elementName];
    } else {
        [request setValue:currentElementValue forKey:elementName];
    }

    // [currentElementValue release];
    currentElementValue = nil;
}

// end of XMLParser.m file
@end
