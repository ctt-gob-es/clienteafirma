//
//  RequestDoc.h
//  @FirmaWSProject
//
//  Created by Antonio Fiñana on 30/10/12.
//  Copyright (c) 2012 Atos. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface Document : NSObject

@property (strong, nonatomic) NSString *docid;
@property (strong, nonatomic) NSString *sigfrmt;
@property (strong, nonatomic) NSString *nm;
@property (strong, nonatomic) NSString *sz;
@property (strong, nonatomic) NSString *mmtp;
@property (strong, nonatomic) NSString *sgnfmt;
@property (strong, nonatomic) NSString *p;
@property (strong, nonatomic) NSMutableArray *ssconfig;

// Post Sign parameters
@property (strong, nonatomic) NSString *params;
@property (strong, nonatomic) NSString *meta;
@property (strong, nonatomic) NSString *result;
@property (strong, nonatomic) NSString *mdalgo;

// error message
@property (strong, nonatomic) NSString *errorMsg;
@property (nonatomic) NSInteger errorCode;

- (NSString *)getSignatureExtension;
- (void)prepareForRequestWithCode:(PFRequestCode)code;

@end
