//
//  BaseListTVC.h
//  PortaFirmasUniv
//
//  Created by Rocio Tovar on 6/3/15.
//  Copyright (c) 2015 Atos. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "WSDataController.h"

static const int kBaseListVCMinPage = 1;
static NSString *const kBaseListVCEditingCellIdentifier = @"CustomRequestNoUICell";
static NSString *const kBaseListVCCellIdentifier = @"CustomRequestCell";
static NSString *const kBaseListVCDataStatusSigned = @"signed";
static NSString *const kBaseListVCDataStatusRejected = @"rejected";
static NSString *const kBaseListVCDataStatusPending = @"unresolved";

@interface BaseListTVC : UITableViewController <WSDelegate>

@property (nonatomic, assign) int currentPage;
@property (nonatomic, assign) BOOL moreDataAvailable;
@property (nonatomic, strong) NSString *dataStatus;
@property (nonatomic, strong) WSDataController *wsDataController;
@property (nonatomic, strong) NSMutableArray *dataArray;
@property (nonatomic, strong) IBOutlet UIView *tableViewFooter;
@property (nonatomic, strong) NSMutableDictionary *filtersDict;

#pragma mark - Lazy load methods
- (void)resetLazyLoad;

#pragma mark - Network calls
- (void)loadData;
- (void)refreshInfo;
- (void)refreshInfoWithFilters:(NSDictionary *)filters;

#pragma mark - Navigation Methods
- (void)prepareForDetailSegue:(UIStoryboardSegue *)segue enablingSigning:(BOOL)enableSign;

@end
