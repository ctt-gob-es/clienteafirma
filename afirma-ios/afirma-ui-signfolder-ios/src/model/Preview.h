//
//  Preview.h
//  TopSongs
//
//  Created by Antonio Fiñana on 31/10/12.
//
//

#import <Foundation/Foundation.h>

@interface Preview : NSObject
@property (strong,nonatomic) NSString *docid;
@property (strong,nonatomic) NSString *name;
@property (strong,nonatomic) NSString *mmtp;
@property (strong,nonatomic) NSString *data;


// error message
@property (strong,nonatomic) NSString *errorMsg;
@property (nonatomic) NSInteger errorCode;
@end
