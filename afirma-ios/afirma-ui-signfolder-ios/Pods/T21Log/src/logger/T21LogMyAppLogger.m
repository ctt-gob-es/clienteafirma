//
//  T21LogMyAppLogger.m
//  T21ContentStore
//
//  Created by Eloi Guzmán on 04/02/14.
//  Copyright (c) 2014 Eloi Guzmán. All rights reserved.
//

#import "T21LogMyAppLogger.h"
#import <libkern/OSAtomic.h>


@interface T21LogMyAppLogger ()
{
    int atomicLoggerCount;
    NSDateFormatter *threadUnsafeDateFormatter;
}
@end

@implementation T21LogMyAppLogger

- (instancetype)init
{
    self = [super init];
    if (self) {
        _logIdentifier = @"APP";
        _context = 0;
    }
    return self;
}

- (NSString *)stringFromDate:(NSDate *)date
{
    int32_t loggerCount = OSAtomicAdd32(0, &atomicLoggerCount);
    
    if (loggerCount <= 1)
    {
        // Single-threaded mode.
        
        if (threadUnsafeDateFormatter == nil)
        {
            threadUnsafeDateFormatter = [[NSDateFormatter alloc] init];
            [threadUnsafeDateFormatter setFormatterBehavior:NSDateFormatterBehavior10_4];
            [threadUnsafeDateFormatter setDateFormat:@"yyyy/MM/dd HH:mm:ss:SSS"];
        }
        
        return [threadUnsafeDateFormatter stringFromDate:date];
    }
    else
    {
        // Multi-threaded mode.
        // NSDateFormatter is NOT thread-safe.
        
        NSString *key = @"MyCustomFormatter_NSDateFormatter";
        
        NSMutableDictionary *threadDictionary = [[NSThread currentThread] threadDictionary];
        NSDateFormatter *dateFormatter = [threadDictionary objectForKey:key];
        
        if (dateFormatter == nil)
        {
            dateFormatter = [[NSDateFormatter alloc] init];
            [dateFormatter setFormatterBehavior:NSDateFormatterBehavior10_4];
            [dateFormatter setDateFormat:@"yyyy/MM/dd HH:mm:ss:SSS"];
            
            [threadDictionary setObject:dateFormatter forKey:key];
        }
        
        return [dateFormatter stringFromDate:date];
    }
}

- (NSString *)formatLogMessage:(DDLogMessage *)logMessage
{
    if (_context == logMessage->logContext) {
        
        NSString *logLevel;
        switch (logMessage->logFlag)
        {
#ifdef DEBUG
            case LOG_FLAG_ERROR : logLevel = @"E|❌"; break;
            case LOG_FLAG_WARN  : logLevel = @"W|⚠️"; break;
#else
            case LOG_FLAG_ERROR : logLevel = @"E|  "; break;
            case LOG_FLAG_WARN  : logLevel = @"W|  "; break;
#endif
            case LOG_FLAG_INFO  : logLevel = @"I|  "; break;
            case LOG_FLAG_DEBUG : logLevel = @"D|  "; break;
            default             : logLevel = @"V|  "; break;
//            case LOG_FLAG_INFO  : logLevel = @"I] ℹ️"; break;
//            case LOG_FLAG_DEBUG : logLevel = @"D] 🚧"; break;
//            default             : logLevel = @"V] ♻️"; break;
        }
        
        NSString *dateAndTime = [self stringFromDate:(logMessage->timestamp)];
        NSString *logMsg = logMessage->logMsg;
        return [NSString stringWithFormat:@"%@ %@ %@[%@] > %@ | %s", logLevel,dateAndTime,logMessage->threadName,_logIdentifier,logMsg,logMessage->function];
    }
    
    return nil;
}

- (void)didAddToLogger:(id <DDLogger>)logger
{
    OSAtomicIncrement32(&atomicLoggerCount);
}
- (void)willRemoveFromLogger:(id <DDLogger>)logger
{
    OSAtomicDecrement32(&atomicLoggerCount);
}
@end
