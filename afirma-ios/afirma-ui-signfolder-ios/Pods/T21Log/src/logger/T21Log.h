//
//  T21Log.h
//  T21Log v1.0
//
//  Created by Eloi Guzman on 3/12/12.
//  Copyright (c) 2012 TEMPOS21. All rights reserved.
///

#import <Foundation/Foundation.h>

#import "DDLog.h"
#import "DDTTYLogger.h"
#import "T21LogMyAppLogger.h"

/**

   Examples to add new loggers:

   //The main My App logger
   DDTTYLogger * logger = [[DDTTYLogger alloc]init];
   [DDLog addLogger:logger];
   T21LogMyAppLogger * loggerApp = [[T21LogMyAppLogger alloc]init];
   [logger setLogFormatter:loggerApp];

   //An optional Third party logger example
   DDTTYLogger * logger3rdParty = [[DDTTYLogger alloc]init];
   [DDLog addLogger:logger3rdParty];
   T21LogMyAppLogger * logger3rdPartyLib = [[T21LogMyAppLogger alloc]init];
   logger3rdPartyLib.logIdentifier = @"3RD PARTY LIB";
   logger3rdPartyLib.context = 17821992;
   [logger3rdParty setLogFormatter:logger3rdPartyLib];

 */

// We want to use the following log levels:
//
// Error
// Warn
// Info
// Debug
// Verbose
//
// All we have to do is undefine the default values,
// and then simply define our own however we want.

extern const int T21Log_level;

// First undefine the default stuff we don't want to use.

#undef LOG_OBJC_MAYBE // (async, lvl, flg, ctx, frmt, ...)

#define LOG_OBJC_MAYBE(async, lvl, flg, ctx, frmt, ...) \
    LOG_MAYBE(async, lvl, flg, ctx, __PRETTY_FUNCTION__, frmt, ## __VA_ARGS__)

#undef DDLogError // (frmt, ...)
#undef DDLogWarn // (frmt, ...)
#undef DDLogInfo // (frmt, ...)
#undef DDLogDebug // (frmt, ...)
#undef DDLogVerbose // (frmt, ...)

#define T21LogError(frmt, ...)           T21LogCtxError(0, frmt, ## __VA_ARGS__);
#define T21LogWarn(frmt, ...)            T21LogCtxWarn(0, frmt, ## __VA_ARGS__);
#define T21LogWarning(frmt, ...)         T21LogCtxWarn(0, frmt, ## __VA_ARGS__);
#define T21LogInfo(frmt, ...)            T21LogCtxInfo(0, frmt, ## __VA_ARGS__);
#define T21LogDebug(frmt, ...)           T21LogCtxDebug(0, frmt, ## __VA_ARGS__);
#define T21LogVerbose(frmt, ...)         T21LogCtxVerbose(0, frmt, ## __VA_ARGS__);

#define T21LogCtxError(ctx, frmt, ...)   LOG_OBJC_MAYBE(NO,   T21Log_level, LOG_FLAG_ERROR,   ctx, frmt, ## __VA_ARGS__)
#define T21LogCtxWarn(ctx, frmt, ...)    LOG_OBJC_MAYBE(NO,    T21Log_level, LOG_FLAG_WARN,    ctx, frmt, ## __VA_ARGS__)
#define T21LogCtxWarning(ctx, frmt, ...) LOG_OBJC_MAYBE(NO,    T21Log_level, LOG_FLAG_WARN,    ctx, frmt, ## __VA_ARGS__)
#define T21LogCtxInfo(ctx, frmt, ...)    LOG_OBJC_MAYBE(YES,    T21Log_level, LOG_FLAG_INFO,    ctx, frmt, ## __VA_ARGS__)
#define T21LogCtxDebug(ctx, frmt, ...)   LOG_OBJC_MAYBE(NO,   T21Log_level, LOG_FLAG_DEBUG,   ctx, frmt, ## __VA_ARGS__)
#define T21LogCtxVerbose(ctx, frmt, ...) LOG_OBJC_MAYBE(YES, T21Log_level, LOG_FLAG_VERBOSE, ctx, frmt, ## __VA_ARGS__)

@interface T21Log : NSObject // Dummy object

@end
