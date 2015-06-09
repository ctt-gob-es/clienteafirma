//
//  Request.m
//  @FirmaWSProject
//
//  Created by Antonio Fiñana on 30/10/12.
//  Copyright (c) 2012 Atos. All rights reserved.
//

#import "PFRequest.h"

@implementation PFRequest

#pragma mark - Init methods

- (instancetype)initWithDict:(NSDictionary *)dict
{
    self = [super init];

    if (self) {
        [self setDocuments:[@[] mutableCopy]];

        NSArray *dictKeys = [dict allKeys];

        if ([dictKeys containsObject:@"id"]) {
            _reqid = dict[@"id"];
        }

        if ([dictKeys containsObject:@"priority"]) {
            _priority = dict[@"priority"];
        }

        if ([dictKeys containsObject:@"workflow"]) {
            _workflow = [dict[@"workflow"] isEqualToString:kPFTrue];
        }

        if ([dictKeys containsObject:@"forward"]) {
            _forward = [dict[@"forward"] isEqualToString:kPFTrue];
        }

        if ([dictKeys containsObject:@"type"]) {
            _type = [PFHelper getPFRequestTypeFromString:dict[@"type"]];
        }
    }

    return self;
}

- (id)initWithId:(NSString *)id
{
    self = [super init];

    if (self) {
        self.reqid = id;
    }

    return self;
}

- (BOOL)isNew
{
    return _view && [_view isEqualToString:@"NUEVO"];
}

@end
