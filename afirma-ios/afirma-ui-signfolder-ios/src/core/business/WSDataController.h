//
//  WSDataController.h
//  PortaFirmasUniv
//
//  Created by Antonio Fiñana on 07/11/12.
//  Copyright (c) 2012 Atos. All rights reserved.
//

#import <Foundation/Foundation.h>

@protocol WSDelegate <NSObject>
- (void)doParse:(NSData *)data;
@optional
- (void)didReceiveParserWithError:(NSString *)errorString;
@end

@interface WSDataController : NSObject
{
    NSURLConnection *connectionInProgress;
    NSMutableData *xmlData;
    BOOL REQUEST_POST;

}
@property (nonatomic, weak) id <WSDelegate> delegate;

- (void)loadPostRequestWithData:(NSString *)data code:(NSInteger)code;
- (void)loadPostRequestWithURL:(NSString *)wsURLString code:(NSInteger)code data:(NSString *)data;
- (void)loadRequestsWithURL:( NSString *)wsURLString;
- (void)cancelConnection;
- (void)startConnection;
- (id)init:(BOOL)isPOSTRequest;
- (id)init;

@end