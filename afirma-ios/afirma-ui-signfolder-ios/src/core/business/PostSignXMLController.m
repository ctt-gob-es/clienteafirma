//
//  PostSignXMLController.m
//  WSFirmaClient
//
//  Created by Antonio Fiñana on 05/11/12.
//
//

#import "PostSignXMLController.h"
#import "PFRequest.h"
#import "Document.h"
#import "Param.h"
#import "Base64Utils.h"

@implementation PostSignXMLController

@synthesize dataSource = _dataSource;

// Builds Web Service Request message
+ (NSString *)buildRequestWithCert:(NSString *)cert witRequestList:(NSArray *)requests;
{
    NSMutableString *mesg = [[NSMutableString alloc] initWithString:@"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<rqttri>\n"];
    [mesg appendFormat:@"<cert>%@</cert>\n", cert];

    // Filters list message
    NSMutableString *requestsMsg = [[NSMutableString alloc] initWithString:@"<reqs>"];
    for (int i = 0; i < [requests count]; i++) {
        PFRequest *request = [requests objectAtIndex:i];

        if ([request status]) {
            [requestsMsg appendFormat:@"\t<req id=\"%@\"  status=\"%@\" >", [request reqid], [request status]];
        } else {
            [requestsMsg appendFormat:@"\t<req id=\"%@\"  status=\"%@\" >", [request reqid], @"OK"];
        }

        NSArray *documents = request.documents;

        if (documents) {
            for (int j = 0; j < [documents count]; j++) {
                Document *document = [documents objectAtIndex:j];

                if (document.mdalgo) {
                    [requestsMsg appendFormat:@"\t<doc docid=\"%@\" cop=\"sign\" sigfrmt=\"%@\" mdalgo=\"%@\">\n", document.docid, document.sigfrmt, document.mdalgo];
                } else {
                    [requestsMsg appendFormat:@"\t<doc docid=\"%@\" cop=\"sign\" sigfrmt=\"%@\">\n", document.docid, document.sigfrmt];
                }

                if (document.params) {
                    [requestsMsg appendFormat:@"\t\t<params>%@</params>\n", document.params];
                } else {
                    [requestsMsg appendFormat:@"\t\t<params></params>\n"];
                }

                // [requestsMsg appendFormat:@"\t\t<meta>%@</meta>\n",document.meta];
                // [requestsMsg appendFormat:@"\t\t<result>%@</result>\n",document.result];
                // [requestsMsg appendFormat:@"\t\t<result><p k='ss.%@'>%@</p><p k='pk1.%@'>%@</p></result>\n",[NSString stringWithFormat:@"%d", j],document.ss,[NSString stringWithFormat:@"%d", j],document.result];
                /*[requestsMsg appendFormat:@"\t\t<result><p k='ss.%d'>%@</p><p k='pk1.%d'>%@</p></result>\n",j, document.ss, j, document.result];
                   [requestsMsg appendString:@"\t</doc>\n"];*/

                [requestsMsg appendFormat:@"\t\t<result><p k='pk1.%d'>%@</p>", j, document.result];

                for (int z = 0; z < [document.ssconfig count]; z++) {
                    Param *param = [document.ssconfig objectAtIndex:z];
                    [requestsMsg appendFormat:@"<p k='%@'>%@</p>", param.key, param.value];
                }

                [requestsMsg appendFormat:@"</result>\n"];

                [requestsMsg appendString:@"\t</doc>\n"];
            }
        }
        [requestsMsg appendString:@"\t</req>\n"];
    }
    [requestsMsg appendString:@"</reqs></rqttri>\n"];
    [mesg appendString:requestsMsg];

    return mesg;
}

- (PostSignXMLController *)initXMLParser
{
    self = [super init];

    if (self) {
        // init array of user objects
        _dataSource = [[NSMutableArray alloc] init];
    }

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

    if ([elementName isEqualToString:@"req"]) {
        T21LogDebug(@"user element found – create a new instance of req class...");

        request = [[PFRequest alloc] init];
        /*
           request.documents=[[NSMutableArray alloc] init];
           waitingForDocument=FALSE;
         */

        // if We attributes in the user elements, you can extract them here:
        request.reqid = [attributeDict objectForKey:@"id"];
        T21LogDebug(@"id attribute found: %@", request.reqid);

        request.status = [attributeDict objectForKey:@"status"];
        T21LogDebug(@"status attribute found: %@", request.status);

        /*
           if (!documentList) {
            documentList=[[NSMutableArray alloc ]init];
           }
         */
    }

    /*
       if ([elementName isEqualToString:@"doc"]) {
        T21LogDebug(@"user element found – create a new instance of doc class...");


        // We reached the end of the XML document
        waitingForDocument=YES;
        document=[[Document alloc ]init];
        T21LogDebug(@"user element found – create a new instance of document class...");
        // We reached the end of the XML document
        waitingForDocument=YES;
        document=[[Document alloc ]init];
        document.docid= [attributeDict objectForKey:@"docid"];
        document.sigfrmt= [attributeDict objectForKey:@"sigfrmt"];
        document.mdalgo= [attributeDict objectForKey:@"mdalgo"];
       }
     */
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

// Parse the end of an element
- (void)   parser:(NSXMLParser *)parser
    didEndElement:(NSString *)elementName
     namespaceURI:(NSString *)namespaceURI
    qualifiedName:(NSString *)qName
{
    T21LogDebug(@"parser didEndElement=%@", elementName);
    [super parser:parser didEndElement:elementName namespaceURI:namespaceURI qualifiedName:qName];

    if ([elementName isEqualToString:@"posts"]) {

        // We reached the end of the XML document
        return;
    }

    if ([elementName isEqualToString:@"req"]) {
        // We reached the end of the XML document
        [_dataSource addObject:request];
        // [request release];
        request = nil;

        return;
    }
    T21LogDebug(@"element for %@ es: %@", elementName, currentElementValue);

    // [currentElementValue release];
    currentElementValue = nil;
}

@end
