//
//  T21LogMyAppLogger.h
//  T21ContentStore
//
//  Created by Eloi Guzmán on 04/02/14.
//  Copyright (c) 2014 Eloi Guzmán. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DDLog.h"

@interface T21LogMyAppLogger : NSObject<DDLogFormatter>


/**
 NSString identifier that will be shown in the log message.
 */
@property (nonatomic,strong) NSString * logIdentifier;

/**
 The default value of this property is 0. The logger will print only the messages whom inner context intersects with this property.
 */
@property (nonatomic) int context;

@end
