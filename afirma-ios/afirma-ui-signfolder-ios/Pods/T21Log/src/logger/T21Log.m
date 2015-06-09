//
//  T21Log.m
//  T21ContentStore
//
//  Created by Eloi Guzmán on 21/02/14.
//  Copyright (c) 2014 Eloi Guzmán. All rights reserved.
//

#import "T21Log.h"
#import "DDLog.h"

#ifdef DEBUG
const int T21Log_level = LOG_LEVEL_VERBOSE;
#else
const int T21Log_level = LOG_LEVEL_WARN;
#endif

@implementation T21Log

@end
