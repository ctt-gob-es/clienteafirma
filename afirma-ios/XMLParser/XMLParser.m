//
//  XMLParser.m
//  XMLParser
//
//

#import "XMLParser.h"
#import "AOEntity.h"
#import "AOXMLReader.h" 

@implementation XMLParser




- (void)setUp
{
    [super setUp];
    
    // Set-up code here.
}

- (void)tearDown
{
    // Tear-down code here.
    
    [super tearDown];
}

- (void)testExample
{
    AOEntity *entidad = [[AOEntity alloc] init];
    
    AOXMLReader *xmlReader = [[AOXMLReader alloc] init];
    //50NSString *filepath = [[NSBundle mainBundle] pathForResource:@"ejemplo" ofType:@"xml"];
    NSString *xml =[NSString stringWithContentsOfFile:@"/Users/tomas/David/XCode/trunk/version madrid/SignSample02/XMLParser/ejemplo.xml" encoding:NSUTF8StringEncoding error:NULL];
    
    entidad = [xmlReader loadXMLByURL:xml ];
    
    NSLog(@"%@",entidad.datField);
   

}

@end
