//
//  AOXMLReader.m
//  SignSample02
//
//

#import "AOXMLReader.h"
#import "CADESConstants.h"

@implementation AOXMLReader

NSDictionary *attributeDictionary;

-(AOEntity *) loadXMLByString:(NSString *) xmlString {
    currentEntidad = [[AOEntity alloc] init];
    parser = [[NSXMLParser alloc] initWithData:[xmlString dataUsingEncoding:NSUTF8StringEncoding]];
	parser.delegate = self;
	[parser parse];
	return currentEntidad;
}

// Método que verifica cuando comienza la etiqueta "e"
- (void)parser:(NSXMLParser *)parser didStartElement:(NSString *)elementName
  namespaceURI:(NSString *)namespaceURI
 qualifiedName:(NSString *)qName attributes:(NSDictionary *)attributeDict {
	if ([elementName isEqualToString:@"e"]) {
		attributeDictionary = [[NSDictionary alloc] initWithDictionary:attributeDict];
	}
}

- (void)parser:(NSXMLParser *)parser didEndElement:(NSString *)elementName
  namespaceURI:(NSString *)namespaceURI qualifiedName:(NSString *)qName {
	if ([elementName isEqualToString:@"e"]) {
        
        if([attributeDictionary valueForKey:@"k"]!=NULL){
            NSString *key = [attributeDictionary valueForKey:@"k"];
            
            if([key isEqualToString:PARAMETER_NAME_DAT]){
                currentEntidad.datField = [attributeDictionary valueForKey:@"v"];
            }
                        
            if([key isEqualToString:PARAMETER_NAME_FORMAT]){
                currentEntidad.formatField = [attributeDictionary valueForKey:@"v"];
            }
            
            if([key isEqualToString:PARAMETER_NAME_ALGORITHM2]){
                currentEntidad.algorithmField = [attributeDictionary valueForKey:@"v"];
            }
            
            if([key isEqualToString:PARAMETER_NAME_PROPERTIES]){
                currentEntidad.propertiesField = [attributeDictionary valueForKey:@"v"];
            }
            
            if([key isEqualToString:PARAMETER_NAME_ID]){
                currentEntidad.idField = [attributeDictionary valueForKey:@"v"];
            }
            
            if([key isEqualToString:PARAMETER_NAME_STSERVLET]){
                currentEntidad.stServletField = [attributeDictionary valueForKey:@"v"];
            }
        }
	}
}

- (void)parser:(NSXMLParser *)parser foundCharacters:(NSString *)string {
	currentNodeContent = (NSMutableString *)[string stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
}


@end
