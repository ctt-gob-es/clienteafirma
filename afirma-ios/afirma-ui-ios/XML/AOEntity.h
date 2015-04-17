//
//  AOEntity.h
//  SignSample02
//
//

#import <Foundation/Foundation.h>

@interface AOEntity : NSObject{
    NSString *datField;
    NSString *formatField;
    NSString *algorithmField;
    NSString *propertiesField;
    NSString *idField;
    NSString *stServletField;
}

@property (nonatomic, retain) NSString *datField;
@property (nonatomic, retain) NSString *formatField;
@property (nonatomic, retain) NSString *algorithmField;
@property (nonatomic, retain) NSString *propertiesField;
@property (nonatomic, retain) NSString *idField;
@property (nonatomic, retain) NSString *stServletField;


@end
