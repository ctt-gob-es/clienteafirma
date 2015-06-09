//
//  Reject.h
//  WSFirmaClient
//
//  Created by Antonio Fiñana on 05/11/12.
//
//

#import <Foundation/Foundation.h>

@interface PFRequestResult : NSObject
@property (strong, nonatomic) NSString *rejectid;
@property (strong, nonatomic) NSString *status;

// error message
@property (strong, nonatomic) NSString *errorMsg;
@property (nonatomic) NSInteger errorCode;

@end
