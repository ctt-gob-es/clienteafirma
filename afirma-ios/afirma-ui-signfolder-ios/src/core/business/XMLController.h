//
//  XMLControler.h
//  PortaFirmasUniv
//
//  Created by Antonio Fiñana on 30/11/12.
//  Copyright (c) 2012 Atos. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface XMLController : NSObject <NSXMLParserDelegate>
{
    BOOL _finishwithError;
    // an ad hoc string to hold element value
    NSMutableString *currentElementValue;
}
@property NSString *errorCode;
@property NSString *err;
@property BOOL finishWithError;

- (void)   parser:(NSXMLParser *)parser didStartElement:(NSString *)elementName namespaceURI:(NSString *)namespaceURI
    qualifiedName:(NSString *)qualifiedName attributes:(NSDictionary *)attributeDict;

// XMLParser.m
- (void)  parser:(NSXMLParser *)parser didEndElement:(NSString *)elementName
    namespaceURI:(NSString *)namespaceURI qualifiedName:(NSString *)qName;
@end
