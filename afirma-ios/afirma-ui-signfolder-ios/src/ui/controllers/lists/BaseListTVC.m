//
//  BaseListVC.m
//  PortaFirmasUniv
//
//  Created by Rocio Tovar on 6/3/15.
//  Copyright (c) 2015 Atos. All rights reserved.
//

#import "BaseListTVC.h"
#import "RequestListXMLController.h"
#import "RequestCell.h"
#import "RequestCellNoUI.h"
#import "DetailTableViewController.h"

@interface BaseListTVC ()

@end

@implementation BaseListTVC

#pragma mark - Init methods

- (instancetype)initWithCoder:(NSCoder *)aDecoder
{
    self = [super initWithCoder:aDecoder];

    if (self) {
        _currentPage = kBaseListVCMinPage;
        _moreDataAvailable = YES;
        _wsDataController = [[WSDataController alloc] init];
        [_wsDataController setDelegate:self];
        _dataArray = [@[] mutableCopy];
    }

    return self;
}

#pragma mark - Life cycle

- (void)viewDidLoad
{
    [super viewDidLoad];

    [self addPullToRefresh];
    [self addWatermark];
    [self setClearsSelectionOnViewWillAppear:NO];
    [_tableViewFooter setHidden:YES];
    [self loadData];
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

#pragma mark - User Interface

- (void)addPullToRefresh
{
    UIRefreshControl *refreshControl = [UIRefreshControl new];

    [refreshControl addTarget:self action:@selector(refreshInfo) forControlEvents:UIControlEventValueChanged];
    self.refreshControl = refreshControl;
}

- (void)addWatermark
{
    UIImageView *watermarkIV = [[UIImageView alloc] initWithImage:[UIImage imageNamed:@"logo_transp"]];

    [watermarkIV setFrame:self.view.bounds];
    [watermarkIV setAutoresizingMask:UIViewAutoresizingFlexibleHeight | UIViewAutoresizingFlexibleWidth ];
    [watermarkIV setContentMode:UIViewContentModeCenter];
    [self.navigationController.view addSubview:watermarkIV];
    [self.navigationController.view sendSubviewToBack:watermarkIV];
}

#pragma mark - Lazy load methods

- (void)resetLazyLoad
{
    _currentPage = kBaseListVCMinPage;
}

#pragma mark - Network calls

- (void)loadDataWithProgressIndicator:(BOOL)showProgressIndicator
{
    if (showProgressIndicator) {
        [SVProgressHUD showWithMaskType:SVProgressHUDMaskTypeBlack];
    }

    NSString *data = [RequestListXMLController buildDefaultRequestWithState:_dataStatus pageNumber:_currentPage filters:_filtersDict];
    T21LogDebug(@"BaseListTVC::loadData::data---\n%@", data);
    [_wsDataController loadPostRequestWithData:data code:PFRequestCodeList];
    [_wsDataController startConnection];
}

- (void)loadData
{
    [self loadDataWithProgressIndicator:YES];
}

- (void)refreshInfo
{
    [self refreshInfoWithFilters:nil];
}

- (void)refreshInfoWithFilters:(NSDictionary *)filters
{
    _filtersDict = [filters mutableCopy];
    [self resetLazyLoad];
    [self loadData];
}

#pragma mark - WSDelegate

- (void)doParse:(NSData *)data
{
    [self.refreshControl endRefreshing];

    NSXMLParser *nsXmlParser = [[NSXMLParser alloc] initWithData:data];
    RequestListXMLController *parser = [[RequestListXMLController alloc] initXMLParser];
    [nsXmlParser setDelegate:parser];
    BOOL success = [nsXmlParser parse];
    [SVProgressHUD dismiss];

    if (success) {
        BOOL finishOK = ![parser finishWithError];

        if (!finishOK) {
            T21LogError(@"Error  parsing  document!");
            [self didReceiveParserWithError:[NSString stringWithFormat:@"Mensaje del servidor:%@(%@)", [parser err], [parser errorCode]]];

            return;
        }

        if (self.currentPage == kBaseListVCMinPage) {
            self.dataArray = [parser dataSource];
        } else {
            [self.dataArray addObjectsFromArray:[parser dataSource]];
        }

        [self setMoreDataAvailable:[parser dataSource].count > 0 && [parser dataSource].count % kRequestListXMLControllerPageSize == 0];
        [self.tableViewFooter setHidden:!self.moreDataAvailable];

        [self.tableView reloadData];
    } else {
        T21LogError(@"Error parsing document!");
        [self didReceiveError:@"Se ha producido un error de conexión con el servidor"];
    }
}

- (void)didReceiveParserWithError:(NSString *)errorString
{
    [SVProgressHUD dismiss];
    [self setMoreDataAvailable:NO];
    [self.tableViewFooter setHidden:!self.moreDataAvailable];
    [self didReceiveError:errorString];
}

- (void)didReceiveError:(NSString *)errorString
{
    [[[UIAlertView alloc] initWithTitle:NSLocalizedString(@"Error", @"")
                                message:errorString
                               delegate:nil
                      cancelButtonTitle:NSLocalizedString(@"OK", @"")
                      otherButtonTitles:nil] show];
}

#pragma mark - UITableViewDataSource

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section
{
    return [_dataArray count];
}

- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath
{
    PFRequest *request = self.dataArray[indexPath.row];

    return self.isEditing ? [self editinCellForRequest:request] : [self cellForRequest:request];
}

- (UITableViewCell *)editinCellForRequest:(PFRequest *)request
{
    RequestCellNoUI *editingCell = [self.tableView dequeueReusableCellWithIdentifier:kBaseListVCEditingCellIdentifier];

    if (!editingCell) {
        T21LogError(@"UnassignedTableViewController::cellForRowAtIndexPath - Cell is nil");

        return nil;
    }

    [editingCell setPFRequest:request];

    return editingCell;
}

- (UITableViewCell *)cellForRequest:(PFRequest *)request
{
    RequestCell *cell = [self.tableView dequeueReusableCellWithIdentifier:kBaseListVCCellIdentifier];

    if (!cell) {
        T21LogError(@"BaseListTVC::cellForRowAtIndexPath - Cell is nil");

        return nil;
    }

    [cell setPFRequest:request];

    return cell;
}

- (void)tableView:(UITableView *)tableView willDisplayCell:(UITableViewCell *)cell forRowAtIndexPath:(NSIndexPath *)indexPath
{
    int normalizedRow = (int)indexPath.row + 1;

    if (_moreDataAvailable && normalizedRow % kRequestListXMLControllerPageSize == 0 && self.dataArray.count == normalizedRow) {
        [_tableViewFooter setHidden:NO];
        self.currentPage++;
        [self loadDataWithProgressIndicator:NO];
    }
}

#pragma mark - Navigation Methods

- (void)prepareForDetailSegue:(UIStoryboardSegue *)segue enablingSigning:(BOOL)enableSign
{
    [SVProgressHUD showWithMaskType:SVProgressHUDMaskTypeBlack];

    NSInteger selectedRow = [self.tableView indexPathForSelectedRow].row;
    DetailTableViewController *detailVC = [segue destinationViewController];
    PFRequest *selectedRequest = self.dataArray[selectedRow];

    [detailVC setSignEnabled:enableSign];
    [detailVC setRequestId:selectedRequest.reqid];
}

@end
