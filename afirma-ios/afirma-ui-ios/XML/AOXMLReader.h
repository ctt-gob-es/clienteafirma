//
//  AOXMLReader.h
//  SignSample02
//
//

#import <Foundation/Foundation.h>
#import "AOEntity.h"

@interface AOXMLReader : NSObject <NSXMLParserDelegate>{
    NSMutableString *currentNodeContent;
	NSXMLParser *parser;
	AOEntity *currentEntidad;
}
@property (nonatomic, retain) NSMutableArray *entidades;

-(id) loadXMLByString:(NSString *)xmlString;


@end
