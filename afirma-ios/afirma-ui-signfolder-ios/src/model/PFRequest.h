//
//  Request.h
//  @FirmaWSProject
//
//  Created by Antonio Fiñana on 30/10/12.
//  Copyright (c) 2012 Atos. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface PFRequest : NSObject
// Atributes
@property (strong, nonatomic) NSString *reqid;
@property (strong, nonatomic) NSString *priority;
@property (assign, nonatomic) BOOL workflow;
@property (assign, nonatomic) BOOL forward;
@property (assign, nonatomic) PFRequestType type;

// Field
@property (strong, nonatomic) NSString *subj;
@property (strong, nonatomic) NSString *snder;
@property (strong, nonatomic) NSString *view;
@property (strong, nonatomic) NSString *date;
@property (strong, nonatomic) NSString *status;

// Error codes
@property (strong, nonatomic) NSString *errorMsg;
@property (nonatomic) NSInteger errorCode;

// Documents list
@property (strong, nonatomic) NSMutableArray *documents;

- (instancetype)initWithDict:(NSDictionary *)dict;
- (BOOL)isNew;
- (id)initWithId:(NSString *)id;

@end
