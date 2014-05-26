//
//  AOUrlDropbox.h
//  SignSample02
//
//

#import <Foundation/Foundation.h>

@interface AOUrlDropbox : NSObject {
    
    NSString *str;
    
}
@property(nonatomic,retain)NSString *str;
+(AOUrlDropbox*)getInstance;
@end  
