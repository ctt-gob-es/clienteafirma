//
//  Detail.m
//  WSFirmaClient
//
//  Created by Antonio Fiñana on 05/11/12.
//
//

#import "Detail.h"
// Atributes
@implementation Detail

- (instancetype)initWithDict:(NSDictionary *)dict
{
    self = [super init];

    if (self) {
        NSArray *dictKeys = [dict allKeys];

        if ([dictKeys containsObject:@"id"]) {
            _detailid = dict[@"id"];
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

@end
