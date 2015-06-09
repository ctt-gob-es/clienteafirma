//
//  Detail.h
//  WSFirmaClient
//
//  Created by Antonio Fiñana on 05/11/12.
//
//

#import <Foundation/Foundation.h>

@interface Detail : NSObject

#pragma mark - Attributes
@property (strong, nonatomic) NSString *detailid;
@property (strong, nonatomic) NSString *priority;
@property (assign, nonatomic) BOOL workflow;
@property (assign, nonatomic) BOOL forward;
@property (assign, nonatomic) PFRequestType type;

#pragma mark - Fields
@property (strong, nonatomic) NSString *subj;
@property (strong, nonatomic) NSString *date;
@property (strong, nonatomic) NSString *app;
@property (strong, nonatomic) NSString *ref;

#pragma mark - Senders list
@property (strong, nonatomic) NSMutableArray *senders;

#pragma mark - Sign Lines
@property (strong, nonatomic) NSMutableArray *signlines;

#pragma mark - Documents list
@property (strong, nonatomic) NSMutableArray *documents;

#pragma mark - Error codes
@property (strong, nonatomic) NSString *errorMsg;
@property (nonatomic) NSInteger errorCode;

#pragma mark - Init methods
- (instancetype)initWithDict:(NSDictionary *)dict;

@end