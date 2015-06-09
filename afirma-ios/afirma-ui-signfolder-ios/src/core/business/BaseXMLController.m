//
//  BaseXMLController.m
//  PortaFirmasUniv
//
//  Created by Rocio Tovar on 11/3/15.
//  Copyright (c) 2015 Atos. All rights reserved.
//

#import "BaseXMLController.h"

@implementation BaseXMLController

#pragma mark - NSXMLParserDelegate

- (void)parser:(NSXMLParser *)parser didStartElement:(NSString *)elementName namespaceURI:(NSString *)namespaceURI qualifiedName:(NSString *)qualifiedName attributes:(NSDictionary *)attributeDict
{
    [super parser:parser didStartElement:elementName namespaceURI:namespaceURI qualifiedName:qualifiedName attributes:attributeDict];

    if ([elementName isEqualToString:@"err"]) {
        self.errorCode = attributeDict[@"cd"];
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

@end
