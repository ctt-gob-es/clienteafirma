//
//  XMLControler.m
//  PortaFirmasUniv
//
//  Created by Antonio Fiñana on 30/11/12.
//  Copyright (c) 2012 Atos. All rights reserved.
//
// XML Controller file

#import "XMLController.h"

@implementation XMLController

@synthesize errorCode, err, finishWithError = _finishwithError;

- (id)init
{
    self = [super self];

    if (self) {
        _finishwithError = NO;
        errorCode = nil;
    }

    return self;

}

#pragma mark - NSXMLParserDelegate

- (void)parser:(NSXMLParser *)parser didStartElement:(NSString *)elementName namespaceURI:(NSString *)namespaceURI qualifiedName:(NSString *)qualifiedName attributes:(NSDictionary *)attributeDict
{
    if ([elementName isEqualToString:@"err"]) {
        T21LogDebug(@"err element found.");

        _finishwithError = YES;
        // We do not have any attributes in the user elements, but if
        // you do, you can extract them here:
        errorCode = [attributeDict objectForKey:@"cd"];

    }
}

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

- (void)parser:(NSXMLParser *)parser didEndElement:(NSString *)elementName namespaceURI:(NSString *)namespaceURI qualifiedName:(NSString *)qName
{
    T21LogDebug(@"parser didEndElement=%@", elementName);

    if ([elementName isEqualToString:@"err"]) {
        // We reached the end of the XML document
        err = currentElementValue;

        return;
    }
}

@end
