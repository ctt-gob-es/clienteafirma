//
//  SignLine.h
//  TopSongs
//
//  Created by Antonio Fiñana on 31/10/12.
//
//

#import <Foundation/Foundation.h>

@interface SignLine : NSObject

// Documents list
@property (strong,nonatomic) NSMutableArray *receivers;
// error message
@property (strong,nonatomic) NSString *errorMsg;
@property (nonatomic) NSInteger errorCode;
@end
