//
//  WSDataController.m
//  PortaFirmasUniv
//
//  Created by Antonio Fiñana on 07/11/12.
//  Copyright (c) 2012 Atos. All rights reserved.
//

#import "WSDataController.h"
#import "NSData+Base64.h"

#define SERVER_URL ((NSDictionary *)[[NSUserDefaults standardUserDefaults] objectForKey:kPFUserDefaultsKeyCurrentServer])[kPFUserDefaultsKeyURL]

@implementation WSDataController
@synthesize delegate = _delegate;

struct {
    unsigned int didDoParse : 1;
    unsigned int didReceiveParserWithError : 2;

} delegateRespondsTo;

- (void)setDelegate:(id <WSDelegate>)aDelegate
{
    if (_delegate != aDelegate) {
        _delegate = aDelegate;
        delegateRespondsTo.didDoParse = [_delegate respondsToSelector:@selector(doParse:)];
        delegateRespondsTo.didReceiveParserWithError = [_delegate respondsToSelector:@selector(didReceiveParserWithError:)];
    }

}

- (id)init
{
    self = [super init];
    REQUEST_POST = NO;

    return self;
}

- (id)init:(BOOL)isPOSTRequest
{
    self = [super init];
    REQUEST_POST = isPOSTRequest;

    return self;
}

- (void)loadPostRequestWithData:(NSString *)data code:(NSInteger)code
{
    [self loadPostRequestWithURL:SERVER_URL code:code data:data];
}

- (void)loadPostRequestWithURL:(NSString *)wsURLString code:(NSInteger)code data:(NSString *)data
{

    NSData *msgData = [data dataUsingEncoding:NSUTF8StringEncoding];
    NSMutableURLRequest *request;

    if (!REQUEST_POST) {
        NSString *params = [NSString stringWithFormat:@"?op=%lu&dat=%@",
                            (unsigned long)code, [msgData base64EncodedString]];

        NSString *newURL = [wsURLString stringByAppendingString:params];
        T21LogDebug(@"WSDataController::loadPostRequestWithURL.GET Url=%@", newURL);

        request = [NSMutableURLRequest requestWithURL:[NSURL URLWithString:newURL]
                                          cachePolicy:NSURLRequestReloadIgnoringCacheData
                                      timeoutInterval:60];

    } else {

        NSString *post = [NSString stringWithFormat:@"op=%lu&dat=%@",
                          (unsigned long)code, [msgData base64EncodedString]];
        NSData *postData = [post dataUsingEncoding:NSASCIIStringEncoding allowLossyConversion:YES];
        NSString *postLength = [NSString stringWithFormat:@"%lu", (unsigned long)[postData length]];

        request = [NSMutableURLRequest requestWithURL:[NSURL URLWithString:wsURLString]
                                          cachePolicy:NSURLRequestReloadIgnoringCacheData
                                      timeoutInterval:60];

        [request setHTTPMethod:@"POST"];
        [request setValue:postLength forHTTPHeaderField:@"Content-Length"];
        [request setValue:@"application/x-www-form-urlencoded" forHTTPHeaderField:@"Content-Type"];
        [request setHTTPBody:postData];
    }

    // Clear out the existing connection if there is one
    if (connectionInProgress) {
        [connectionInProgress cancel];
        connectionInProgress = nil;
    }
    // Instantiate the object to hold all incoming data
    xmlData = [[NSMutableData alloc] init];

    // Create and initiate the connection
    connectionInProgress = [[NSURLConnection alloc] initWithRequest:request
                                                           delegate:self
                                                   startImmediately:NO];
}

- (void)loadRequestsWithURL:( NSString *)wsURLString
{
    NSURL *url = [NSURL URLWithString:wsURLString];
    // Create a request object with that URL
    NSURLRequest *request = [NSURLRequest requestWithURL:url
                                             cachePolicy:NSURLRequestReloadIgnoringCacheData
                                         timeoutInterval:60];

    // Clear out the existing connection if there is one
    if (connectionInProgress) {
        [connectionInProgress cancel];
        connectionInProgress = nil;
    }

    // Create and initiate the connection
    connectionInProgress = [[NSURLConnection alloc] initWithRequest:request
                                                           delegate:self
                                                   startImmediately:NO];

    // Instantiate the object to hold all incoming data
    xmlData = [[NSMutableData alloc] init];
}

// didReceiveResponse
- (void)connection:(NSURLConnection *)connection didReceiveResponse:(NSURLResponse *)response
{
    NSHTTPURLResponse *httpResponse = (NSHTTPURLResponse *)response;

    T21LogDebug(@"WSDataController::connection didReceive Response =%@", [httpResponse allHeaderFields]);
}

- (void)connection:(NSURLConnection *)connection didReceiveData:(NSData *)data
{
    T21LogDebug(@"Succeeded! Received %lu bytes of data", (unsigned long)[data length]);
    [xmlData appendData:data];
}

- (void)connectionDidFinishLoading:(NSURLConnection *)connection
{
    T21LogDebug(@"Datos totales descargados: %lu", (unsigned long)[xmlData length]);
    [self doParse:xmlData];
}

- (void)connection:(NSURLConnection *)connection didFailWithError:(NSError *)error
{
    [SVProgressHUD dismiss];
    connectionInProgress = nil;
    xmlData = nil;
    NSString *errorString = [NSString stringWithFormat:@"Load failed: %@",
                             [error localizedDescription]];

    if ([_delegate respondsToSelector:@selector(didReceiveParserWithError:)]) {
        [_delegate didReceiveParserWithError:errorString];
    }
}

- (void)doParse:(NSData *)data
{
    T21LogDebug(@"doParse data:\n%@", [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding]);
    [_delegate doParse:data];
}

- (void)cancelConnection
{
    // Clear out the existing connection if there is one
    if (connectionInProgress) {
        [connectionInProgress cancel];
        connectionInProgress = nil;
    }

}

- (void)startConnection
{
    if (connectionInProgress) {
        [connectionInProgress start];
    }
}

@end
