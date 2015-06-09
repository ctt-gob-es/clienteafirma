//
//  RequestTableViewController.m
//  PortaFirmas_@Firma
//
//  Created by Antonio Fiñana on 22/10/12.
//  Copyright (c) 2012 Tempos21. All rights reserved.
//

#import "UnassignedRequestTableViewController.h"
#import "RequestListXMLController.h"
#import "RejectXMLController.h"
#import "DetailTableViewController.h"
#import "PFRequest.h"
#import "PFCellContentFactory.h"
#import "AppDelegate.h"
#import "RequestSignerController.h"
#import "PFRequestResult.h"
#import "RequestCell.h"
#import "RequestCellNoUI.h"
#import "ApproveXMLController.h"

#define TAB_BAR_HIDDEN_FRAME CGRectMake(-10, -10, 0, 0)

@interface UnassignedRequestTableViewController ()
{
    CGRect _tabBarFrame;
    CGRect _tabViewFrame;
    NSSet *_selectedRequestsSetToSign;
    NSSet *_selectedRequestSetToApprove;
    PFWaitingResponseType _waitingResponseType;
    BOOL _didSetUpTabBar;
}

@property (weak, nonatomic) IBOutlet UIBarButtonItem *filterButtonItem;

@end

@implementation UnassignedRequestTableViewController

- (id)initWithCoder:(NSCoder *)aDecoder
{
    self = [super initWithCoder:aDecoder];

    if (self) {
        [SVProgressHUD dismiss];

        self.dataStatus = kBaseListVCDataStatusPending;

        // Custom initialization
        [_signBarButton setEnabled:NO];
        [_rejectBarButton setEnabled:NO];

        // Sets data in Aplication delegate objet to be shared for the application's tab
        AppDelegate *myDelegate = (AppDelegate *)[[UIApplication sharedApplication] delegate];
        appConfig = myDelegate.appConfig;

        selectedRows = [@[] mutableCopy];
    }

    return self;
}

#pragma mark - Life Cycle

- (void)viewDidLoad
{
    [super viewDidLoad];
    [self.navigationItem setRightBarButtonItems:@[_filterButtonItem, self.navigationItem.rightBarButtonItem] animated:YES];
}

- (void)viewWillAppear:(BOOL)animated
{
    [super viewWillAppear:animated];
    [self setupTabBar];
    [self.parentViewController setHidesBottomBarWhenPushed:TRUE];
    [self.navigationController setToolbarHidden:YES];
    [self.parentViewController.tabBarController.tabBar setHidden:NO];
}

- (void)viewWillDisappear:(BOOL)animated
{
    [self.parentViewController setHidesBottomBarWhenPushed:TRUE];
    [self.navigationController setToolbarHidden:YES];
    [self.parentViewController.tabBarController.tabBar setHidden:NO];
}

- (void)viewDidUnload
{
    [self setSignBarButton:nil];
    [self setRejectBarButton:nil];

    [super viewDidUnload];
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

#pragma mark - Network Calls

- (void)loadData
{
    T21LogDebug(@"UnassignedRequestTableViewController::loadRequestList");
    _waitingResponseType = PFWaitingResponseTypeList;
    [super loadData];
}

- (IBAction)rejectAction:(id)sender
{
    T21LogDebug(@"Reject Action....");
    [SVProgressHUD showWithMaskType:SVProgressHUDMaskTypeBlack];

    NSString *data = [RejectXMLController buildRequestWithIds:selectedRows];
    T21LogDebug(@"UnassignedRequestTableViewController::rejectRequest input Data=%@", data);

    _waitingResponseType = PFWaitingResponseTypeRejection;
    [self.wsDataController loadPostRequestWithData:data code:PFRequestCodeReject];
    [self.wsDataController startConnection];
}

- (IBAction)cancelAction:(id)sender
{
    T21LogDebug(@"Cancel Action....");
    [self cancelEditing];
}

- (void)startSendingSignRequests
{
    [SVProgressHUD showWithMaskType:SVProgressHUDMaskTypeBlack];

    requestSignerController = [RequestSignerController new];
    [requestSignerController setDelegate:self];
    [requestSignerController loadPreSignRequestsWithCurrentCertificate:_selectedRequestsSetToSign.allObjects];
}

- (void)startSendingApproveRequests
{
    [SVProgressHUD showWithMaskType:SVProgressHUDMaskTypeBlack];

    _waitingResponseType = PFWaitingResponseTypeApproval;
    NSString *requestData = [ApproveXMLController buildRequestWithRequestArray:_selectedRequestSetToApprove.allObjects];
    T21LogDebug(@"UnassignedRequestTableViewController::startSendingApproveRequests------\n%@\n-----------------------------------------------------------------------\n", requestData);
    [self.wsDataController loadPostRequestWithData:requestData code:PFRequestCodeApprove];
    [self.wsDataController startConnection];
}

#pragma mark - User Interface

- (void)setupTabBar
{
    if (!_didSetUpTabBar) {
        [self.navigationController setTabBarItem:[self.tabBarItem initWithTitle:@"Pendientes"
                                                                          image:[[QuartzUtils getImageWithName:@"ic_pendientes" andTintColor:[UIColor lightGrayColor]] imageWithRenderingMode:UIImageRenderingModeAlwaysOriginal]
                                                                  selectedImage:[[QuartzUtils getImageWithName:@"ic_pendientes" andTintColor:THEME_COLOR] imageWithRenderingMode:UIImageRenderingModeAlwaysOriginal]]];
        _didSetUpTabBar = YES;
    }
    [self.navigationController setTitle:@"Pendientes"];
    [self.presentingViewController setTitle:@"Pendientes"];
    [self.navigationController.navigationItem setTitle:@"Pendientes"];
    [self.navigationItem setTitle:@"Pendientes"];
}

- (void)updateEditButtons
{
    BOOL enableButtons = selectedRows.count > 0;

    [_signBarButton setEnabled:enableButtons];
    [_rejectBarButton setEnabled:enableButtons];
}

- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation
{
    return YES;
}

#pragma mark - User Interaction

- (IBAction)didTapOnBackButton:(id)sender
{
    [self.navigationController dismissViewControllerAnimated:YES completion:nil];
}

- (IBAction)editAction:(id)sender
{
    if ([self.dataArray count] > 0) {
        [self setEditing:!self.editing animated:!self.editing];
    }
}

- (IBAction)signAction:(id)sender
{
    T21LogDebug(@"Sign Action....\nSelected rows=%lu", (unsigned long)[selectedRows count]);
    [self separateSignAndApproveRequests];
    [self showSignApproveAlert];
}

- (void)separateSignAndApproveRequests
{
    NSSet *selectedRequestsSet = [NSSet setWithArray:selectedRows];

    _selectedRequestsSetToSign = [selectedRequestsSet objectsPassingTest:^BOOL (PFRequest *request, BOOL *stop) {
                                      return request.type == PFRequestTypeSign;
                                  }];
    _selectedRequestSetToApprove = [selectedRequestsSet objectsPassingTest:^BOOL (PFRequest *request, BOOL *stop) {
                                        return request.type == PFRequestTypeApprove;
                                    }];
}

- (void)showSignApproveAlert
{
    NSString *message;

    if (_selectedRequestSetToApprove && _selectedRequestSetToApprove.count > 0 && _selectedRequestsSetToSign && _selectedRequestsSetToSign.count > 0) {
        message = [NSString stringWithFormat:@"Se van a procesar %lu peticiones de firma y %lu de visto bueno.", (unsigned long)_selectedRequestsSetToSign.count, (unsigned long)_selectedRequestSetToApprove.count];
    } else if (_selectedRequestSetToApprove && _selectedRequestSetToApprove.count > 0) {
        message = [NSString stringWithFormat:@"Se van a procesar %lu peticiones de visto bueno.", (unsigned long)_selectedRequestSetToApprove.count];
    } else if (_selectedRequestsSetToSign && _selectedRequestsSetToSign.count > 0) {
        message = [NSString stringWithFormat:@"Se van a procesar %lu peticiones de firma.", (unsigned long)_selectedRequestsSetToSign.count];
    }

    if (message) {
        [[[UIAlertView alloc] initWithTitle:@"Aviso"
                                    message:message
                                   delegate:self
                          cancelButtonTitle:@"Cancelar"
                          otherButtonTitles:@"Continuar", nil] show];
    }
}

#pragma mark - UITableViewDelegate

- (void)tableView:(UITableView *)theTableView didSelectRowAtIndexPath:(NSIndexPath *)newIndexPath
{
    [self updateSelectionWithIndexPath:newIndexPath selected:YES];
}

- (void)tableView:(UITableView *)theTableView didDeselectRowAtIndexPath:(NSIndexPath *)newIndexPath
{
    [self updateSelectionWithIndexPath:newIndexPath selected:NO];
}

- (void)updateSelectionWithIndexPath:(NSIndexPath *)newIndexPath selected:(BOOL)selected
{
    if ([self isEditing]) {
        selected ? [selectedRows addObject:self.dataArray[newIndexPath.row]] : [selectedRows removeObject:self.dataArray[newIndexPath.row]];
        [self updateEditButtons];
    }
}

#pragma mark - Navigation Methods

- (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender
{
    T21LogDebug(@"BaseListTVC::prepareForSegueWithIdentifier=%@", [segue identifier]);

    if ([[segue identifier] isEqualToString:@"segueDetail"]) {
        [self prepareForDetailSegue:segue enablingSigning:YES];
    }
}

- (BOOL)shouldPerformSegueWithIdentifier:(NSString *)identifier sender:(id)sender
{
    T21LogDebug(@"shouldPerformSegueWithIdentifier:%@", ([self isEditing]) ? @"YES" : @"NO");

    return (!([self isEditing]));
}

#pragma mark - Edit Methods

- (void)setEditing:(BOOL)editing animated:(BOOL)animated
{
    [super setEditing:editing animated:animated];
    T21LogDebug(@"setEditing editing=%d", editing);

    if (editing) {
        [self.tableView reloadData];
        [_selectButtonItem setTitle:@"Hecho"];
        selectedRows = [@[] mutableCopy];
        [self setEditingBottomBar];
    } else {
        [_selectButtonItem setTitle:@"Seleccionar"];
        [self setNormalBottomBar];

        if (![selectedRows count] > 0) {
            [_signBarButton setEnabled:NO];
            [_rejectBarButton setEnabled:NO];
        }

        [[self tableView] reloadData];
    }
}

- (void)setEditingBottomBar
{
    if (_tabBarFrame.size.width != self.tabBarController.tabBar.frame.size.width) {
        _tabBarFrame = self.tabBarController.tabBar.frame;
    }

    if (!CGRectIsEmpty(self.tabBarController.tabBar.frame)) {
        CGRect fullScreen = self.view.frame;

        fullScreen.size.height += self.tabBarController.tabBar.frame.size.height;
        [self.view setFrame:fullScreen];
        [self.tabBarController.tabBar setFrame:TAB_BAR_HIDDEN_FRAME];
        [self.navigationController setToolbarHidden:NO animated:YES];
    }
}

- (void)setNormalBottomBar
{
    if (_tabBarFrame.size.height > 0 && CGRectIsEmpty(self.tabBarController.tabBar.frame)) {
        [self.navigationController setToolbarHidden:YES animated:YES];
        CGRect tabRect = self.view.frame;
        tabRect.size.height -= self.tabBarController.tabBar.frame.size.height;
        [self.view setFrame:tabRect];
        [self.tabBarController.tabBar setFrame:_tabBarFrame];
    }
}

- (void)cancelEditing
{
    [super setEditing:NO animated:NO];
    [self setEditing:NO animated:NO];
    [[self tableView] reloadData];
}

#pragma mark - WSDelegate

- (void)doParse:(NSData *)data
{
    if (_waitingResponseType == PFWaitingResponseTypeList) {
        [super doParse:data];
    } else if (_waitingResponseType == PFWaitingResponseTypeRejection) {
        [self didReceivedRejectionResponse:data];
    } else if (_waitingResponseType == PFWaitingResponseTypeApproval) {
        [self didReceivedApprovalResponse:data];
    }

    _waitingResponseType = nil;
    [self cancelEditing];
}

- (void)didReceivedRejectionResponse:(NSData *)responseData
{
    NSXMLParser *nsXmlParser = [[NSXMLParser alloc] initWithData:responseData];
    RejectXMLController *parser = [[RejectXMLController alloc] initXMLParser];

    [nsXmlParser setDelegate:parser];
    BOOL success = [nsXmlParser parse];
    [SVProgressHUD dismiss];

    if (success) {
        NSArray *rejectsReq = [parser dataSource];
        [self didReceiveRejectResult:rejectsReq];
    } else {
        [self didReceiveError:@"Se ha producido un error de conexión con el servidor (501)"];
    }
}

- (void)didReceivedApprovalResponse:(NSData *)responseData
{
    T21LogDebug(@"didReceivedApprovalResponse:\n%@", [[NSString alloc] initWithData:responseData encoding:NSUTF8StringEncoding]);
    NSXMLParser *nsXmlParser = [[NSXMLParser alloc] initWithData:responseData];
    ApproveXMLController *parser = [[ApproveXMLController alloc] init];

    [nsXmlParser setDelegate:parser];
    BOOL success = [nsXmlParser parse];
    [SVProgressHUD dismiss];

    if (success) {
        NSArray *approvalRequests = [parser dataSource];
        [self handleApprovalRequests:approvalRequests];
    } else {
        [self didReceiveError:@"Se ha producido un error de conexión con el servidor (501)"];
    }
}

- (void)handleApprovalRequests:(NSArray *)approvalRequests
{
    NSMutableArray *idsForRequestsWithError = [@[] mutableCopy];

    [approvalRequests enumerateObjectsUsingBlock:^(PFRequest *request, NSUInteger idx, BOOL *stop) {
         if ([request.status isEqualToString:@"KO"]) {
             [idsForRequestsWithError addObject:request.reqid];
         }
     }];

    if (idsForRequestsWithError.count == 0) {
        // @" Peticiones firmadas corrrectamente"
        [[[UIAlertView alloc] initWithTitle:NSLocalizedString(@"INFO", @"")
                                    message:@"Peticiones procesadas correctamente"
                                   delegate:nil
                          cancelButtonTitle:NSLocalizedString(@"OK", @"")
                          otherButtonTitles:nil] show];
    } else {
        NSString *errorMessage;

        if (idsForRequestsWithError.count == 1) {
            errorMessage = [NSString stringWithFormat:@"Error al procesar la petición con código:%@", idsForRequestsWithError[0]];
        } else {
            NSMutableString *errorIDSString = [@"" mutableCopy];
            [idsForRequestsWithError enumerateObjectsUsingBlock:^(NSString *requestID, NSUInteger idx, BOOL *stop) {
                 [errorIDSString appendFormat:@" %@", requestID];
             }];

            errorMessage = [NSString stringWithFormat:@"Error al procesar las peticiones con códigos:%@", errorIDSString];
        }

        [self didReceiveError:errorMessage];
    }

    [self cancelEditing];
    [self refreshInfo];
}

- (void)didReceiveSignerRequestResult:(NSArray *)requestsSigned
{
    T21LogDebug(@"UnsignedRequestTableViewController::didReceiveSignerRequestResult - reqs count: %lu", (unsigned long)[requestsSigned count]);
    [SVProgressHUD dismiss];

    NSIndexSet *requestsWithError = [requestsSigned indexesOfObjectsPassingTest:^BOOL (PFRequest *request, NSUInteger idx, BOOL *stop) {
                                         return [request.status isEqualToString:@"KO"];
                                     }];

    // Mostramos un mensaje modal con el resultado de la operacion
    if (requestsWithError.count == 0) {
        // Peticiones firmadas corrrectamente
        [[[UIAlertView alloc] initWithTitle:NSLocalizedString(@"INFO", @"")
                                    message:@"Peticiones firmadas correctamente"
                                   delegate:nil
                          cancelButtonTitle:NSLocalizedString(@"OK", @"")
                          otherButtonTitles:nil] show];
    } else {
        // Operacion finalizada con errores
        NSString *msg = requestsWithError.count == 1 ? @"Ocurrio un error al firmar la peticion seleccionada" : @"Ocurrio un error al firmar algunas de las peticiones seleccionadas.";
        [[[UIAlertView alloc] initWithTitle:NSLocalizedString(@"Error", @"")
                                    message:msg
                                   delegate:nil
                          cancelButtonTitle:NSLocalizedString(@"OK", @"")
                          otherButtonTitles:nil] show];
    }

    if (_selectedRequestSetToApprove && _selectedRequestSetToApprove.count > 0) {
        [self startSendingApproveRequests];
    } else {
        [self cancelEditing];
        [self refreshInfo];
    }
}

- (void)didReceiveRejectResult:(NSArray *)requestsSigned
{
    BOOL processedOK = TRUE;

    for (PFRequestResult *request in requestsSigned) {
        if ([[request status] isEqualToString:@"KO"]) {
            [[[UIAlertView alloc] initWithTitle:NSLocalizedString(@"Error", @"")
                                        message:[[NSString alloc] initWithFormat:@"Error al procesar la petición con codigo:%@", [request rejectid]]
                                       delegate:nil
                              cancelButtonTitle:NSLocalizedString(@"OK", @"")
                              otherButtonTitles:nil] show];
            processedOK = FALSE;
        }
    }

    if (processedOK) {
        // @" Peticiones firmadas corrrectamente"
        [[[UIAlertView alloc] initWithTitle:NSLocalizedString(@"INFO", @"")
                                    message:@"Peticiones rechazadas correctamente"
                                   delegate:nil
                          cancelButtonTitle:NSLocalizedString(@"OK", @"")
                          otherButtonTitles:nil] show];
    }
    [self cancelEditing];
}

#pragma mark - UIAlertViewDelegate

- (void)alertView:(UIAlertView *)alertView didDismissWithButtonIndex:(NSInteger)buttonIndex
{
    if (buttonIndex > 0) {
        if (_selectedRequestsSetToSign && _selectedRequestsSetToSign.count > 0) {
            [self startSendingSignRequests];
        } else if (_selectedRequestSetToApprove && _selectedRequestSetToApprove.count > 0) {
            [self startSendingApproveRequests];
        }
    }
}

@end
