//
//  DetailTableViewController.m
//  PortaFirmas_@Firma
//
//  Created by Antonio Fiñana Sánchez on 19/10/12.
//  Copyright (c) 2012 Luis Lopez. All rights reserved.
//

#import "DetailTableViewController.h"
#import "BaseListTVC.h"
#import "DetailXMLController.h"
#import "Detail.h"
#import "AttachmentViewController.h"
#import "ReceiversViewController.h"
#import "SendersViewController.h"
#import "WSDataController.h"
#include "AppDelegate.h"
#import "PFRequest.h"
#import "RejectXMLController.h"
#import "ApproveXMLController.h"

typedef NS_ENUM (NSInteger, PFDocumentAction)
{
    PFDocumentActionReject,
    PFDocumentActionSign,
    PFDocumentActionCancel
};

@interface DetailTableViewController ()
{
    UIActionSheet *_documentActionSheet;
    RequestSignerController *_requestSignerController;
    PFWaitingResponseType _waitingResponseType;
}

@end

@implementation DetailTableViewController

@synthesize requestId = _requestId, dataSource = _dataSource, signEnabled = _signEnabled;

- (id)initWithCoder:(NSCoder *)aDecoder
{
    self = [super initWithCoder:aDecoder];

    if (self) {
        [SVProgressHUD dismiss];
        wsController = [[WSDataController alloc] init];
        wsController.delegate = self;
        _signEnabled = FALSE;
    }

    return self;
}

- (void)loadWebService
{
    NSString *url = [appConfig objectForKey:@"requestDetailURL"];

    T21LogDebug(@"DetailTableViewController::loadWebService.url=%@", url);

    NSString *data = [DetailXMLController buildRequestWithId:_requestId];
    T21LogDebug(@"DetailTableViewController::loadWebService.message data=%@", data);

    // Load Detail request
    _waitingResponseType = PFWaitingResponseTypeDetail;
    [wsController loadPostRequestWithData:data code:4];
    [wsController startConnection];
}

- (void)viewWillAppear:(BOOL)animated
{
    T21LogDebug(@"DetailTableViewController::viewWillAppear");

    self.navigationController.toolbarHidden = YES;
}

//  [dataController loadRequestsWithURL:[DetailXMLController buildRequestWithId:_requestId]];
- (void)viewWillDisappear:(BOOL)animated
{
    // self.navigationController.toolbarHidden=YES;
    [wsController cancelConnection];
}

- (void)viewDidLoad
{
    [super viewDidLoad];
    T21LogDebug(@"DetailTableViewController::viewDidLoad");

    [self.tableView setTableFooterView:[[UIView alloc] initWithFrame:CGRectZero]];

    // Enable or disable  action button
    [_btnDocumentAction setEnabled:_signEnabled];
    [self loadWebService];
}

- (void)viewDidUnload
{
    [self setSubject:nil];
    [self setBtnDocumentAction:nil];
    [super viewDidUnload];
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

#pragma mark - UIActionSheet methods

- (IBAction)didTapDocumentActionButton:(id)sender
{
    NSString *signButtonTitle = [(PFRequest *)_dataSource type] == PFRequestTypeSign ? @"Firmar" : @"Visto Bueno";

    _documentActionSheet = [[UIActionSheet alloc] initWithTitle:nil
                                                       delegate:self
                                              cancelButtonTitle:@"Cancelar"
                                         destructiveButtonTitle:nil
                                              otherButtonTitles:@"Rechazar", signButtonTitle, nil];

    [_documentActionSheet showInView:self.view];
}

- (void)actionSheet:(UIActionSheet *)actionSheet clickedButtonAtIndex:(NSInteger)buttonIndex
{
    switch (buttonIndex) {
        case PFDocumentActionReject:
            [self rejectAction];
            break;

        case PFDocumentActionSign:
            [self signAction];
            break;

        case PFDocumentActionCancel:
            T21LogDebug(@"Cancel Action....");
            break;

        default:
            break;
    }
}

- (void)rejectAction
{
    T21LogDebug(@"Reject Action....");

    T21LogDebug(@"UnassignedRequestTableViewController::Reject request....Selected rows=%lu", (unsigned long)[_selectedRows count]);
    [SVProgressHUD showWithMaskType:SVProgressHUDMaskTypeBlack];

    NSString *data = [RejectXMLController buildRequestWithIds:_selectedRows];
    T21LogDebug(@"UnassignedRequestTableViewController::rejectRequest input Data=%@", data);

    _waitingResponseType = PFWaitingResponseTypeRejection;
    [wsController loadPostRequestWithData:data code:PFRequestCodeReject];
    [wsController startConnection];
}

- (void)signAction
{
    T21LogDebug(@"Sign Action....\nAccept request....Selected rows=%lu", (unsigned long)[_selectedRows count]);

    [SVProgressHUD showWithMaskType:SVProgressHUDMaskTypeBlack];

    if ([(Detail *)_dataSource type] == PFRequestTypeSign) {
        [self startSignRequest];
    } else {
        [self startApprovalRequest];
    }
}

#pragma mark - Network Methods

- (void)startSignRequest
{
    _waitingResponseType = PFWaitingResponseTypeSign;
    _requestSignerController = [[RequestSignerController alloc] init];
    _requestSignerController.delegate = self;
    [_requestSignerController loadPreSignDetailWithCurrentCertificate:_dataSource];
}

- (void)startApprovalRequest
{
    _waitingResponseType = PFWaitingResponseTypeApproval;
    NSString *requestData = [ApproveXMLController buildRequestWithRequestArray:@[_dataSource]];

    T21LogDebug(@"DetailTableViewController::startApprovalRequest------\n%@\n-----------------------------------------------------------------------\n", requestData);
    [wsController loadPostRequestWithData:requestData code:PFRequestCodeApprove];
    [wsController startConnection];
}

#pragma mark - Navigation

- (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender
{
    T21LogDebug(@"DetailTableViewController::prepareForSegue sender=%@", [segue identifier]);

    if ([[segue identifier] isEqual:@"segueAttachments"]) {
        T21LogDebug(@"DetailTableViewController::prepareForSegue number of attachments=%lu", (unsigned long)[_dataSource.documents count]);

        AttachmentViewController *attachmentController = [segue destinationViewController];
        attachmentController.dataSource = _dataSource.documents;
        [attachmentController setDetail:_dataSource];
        [attachmentController setRequestStatus:[PFHelper getPFRequestStatusFromClass:self.presentingViewController.class]];
    }

    if ([[segue identifier] isEqual:@"segueShowSenders"]) {
        T21LogDebug(@"DetailTableViewController::prepareForSegue number of receivers=%lu", (unsigned long)[_dataSource.senders count]);
        SendersViewController *sendersController = [segue destinationViewController];
        sendersController.dataSource = _dataSource.senders;
    }

    if ([[segue identifier] isEqual:@"segueShowReceivers"]) {
        T21LogDebug(@"DetailTableViewController::prepareForSegue number of receivers=%lu", (unsigned long)[_dataSource.senders count]);
        ReceiversViewController *receiversController = [segue destinationViewController];
        receiversController.dataSource = _dataSource.signlines;
    }
}

- (void)loadDetailInfo
{
    self.referenceLbl.text = _dataSource.ref;
    self.inputDateLbl.text = _dataSource.date;
    self.subject.text = _dataSource.subj;
    self.applicationLbl.text = _dataSource.app;

    _selectedRows = nil;
    PFRequest *detailRequest = [[PFRequest alloc] initWithId:_requestId];
    detailRequest.documents = _dataSource.documents;
    _selectedRows = [[NSArray alloc] initWithObjects:detailRequest, nil];
}

- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation
{
    UIInterfaceOrientation des = self.interfaceOrientation;

    if (UI_USER_INTERFACE_IDIOM() == UIUserInterfaceIdiomPad) { // iPad
        if (des == UIInterfaceOrientationPortrait || des == UIInterfaceOrientationPortraitUpsideDown) { // ipad-portairait
        } else {                                                // ipad -landscape
        }
    } else {                                                    // iphone
        UIInterfaceOrientation des = self.interfaceOrientation;

        if (des == UIInterfaceOrientationPortrait || des == UIInterfaceOrientationPortraitUpsideDown) { // iphone portrait
        } else {                                                // iphone -landscape
        }
    }

    return YES;
}

#pragma mark - WSDataController delegate

- (void)doParse:(NSData *)data
{
    [SVProgressHUD dismiss];
    NSXMLParser *nsXmlParser = [[NSXMLParser alloc] initWithData:data];
    id <NSXMLParserDelegate> parser = [self parserForCurrentRequest];
    [nsXmlParser setDelegate:parser];

    if ([nsXmlParser parse]) {
        if (_waitingResponseType == PFWaitingResponseTypeRejection) {
            [self didReceiveRejectResult:[(RejectXMLController *)parser dataSource]];
        } else if (_waitingResponseType == PFWaitingResponseTypeApproval) {
            [self didReceiveApprovalResult:[(ApproveXMLController *)parser dataSource]];
        } else {
            [self didFinisParsingWithParser:parser];
        }
    } else {
        [self didReceiveParserWithError:@"Se ha producido un error de conexión con el servidor"];
    }
}

- (id <NSXMLParserDelegate> )parserForCurrentRequest
{
    if (_waitingResponseType == PFWaitingResponseTypeDetail) {
        return [[DetailXMLController alloc] initXMLParser];
    } else if (_waitingResponseType == PFWaitingResponseTypeRejection) {
        return [[RejectXMLController alloc] initXMLParser];
    } else if (_waitingResponseType == PFWaitingResponseTypeApproval) {
        return [[ApproveXMLController alloc] init];
    }

    return nil;
}

- (void)didReceiveRejectResult:(NSArray *)requestsSigned
{
    BOOL processedOK = TRUE;

    for (PFRequestResult *request in requestsSigned) {
        if ([[request status] isEqualToString:@"KO"]) {
            [self didReceiveError:[[NSString alloc] initWithFormat:@"Error al procesar la petición con codigo:%@", [request rejectid]]];
            processedOK = FALSE;
        }
    }

    if (processedOK) {
        // @" Peticiones firmadas corrrectamente"
        [[[UIAlertView alloc] initWithTitle:NSLocalizedString(@"INFO", @"")
                                    message:@"Peticiones rechazadas correctamente"
                                   delegate:self
                          cancelButtonTitle:NSLocalizedString(@"OK", @"")
                          otherButtonTitles:nil] show];
    }

    [_documentActionSheet dismissWithClickedButtonIndex:-1 animated:YES];
}

- (void)didReceiveApprovalResult:(NSArray *)approvedRequests
{
    NSMutableArray *idsForRequestsWithError = [@[] mutableCopy];

    [approvedRequests enumerateObjectsUsingBlock:^(PFRequest *request, NSUInteger idx, BOOL *stop) {
         if ([request.status isEqualToString:@"KO"]) {
             [idsForRequestsWithError addObject:request.reqid];
         }
     }];

    if (idsForRequestsWithError.count == 0) {
        // @" Peticiones firmadas corrrectamente"
        [[[UIAlertView alloc] initWithTitle:NSLocalizedString(@"INFO", @"")
                                    message:@"Peticiones procesadas correctamente"
                                   delegate:self
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

    [_documentActionSheet dismissWithClickedButtonIndex:-1 animated:YES];
}

- (void)didFinisParsingWithParser:(DetailXMLController *)parser
{
    BOOL finishOK = ![parser finishWithError];

    if (!finishOK) {
        T21LogError(@"Error  parsing  document!");
        [self didReceiveParserWithError:[NSString stringWithFormat:@"Mensaje del servidor:%@(%@)", [parser err], [parser errorCode]]];
    } else {
        T21LogDebug(@"DetailTableViewController:: Parsing Detail XML message with no errors ");
        _dataSource = [parser dataSource];
        [self loadDetailInfo];
    }
}

- (void)didReceiveError:(NSString *)errorString
{
    [SVProgressHUD dismiss];

    T21LogDebug(@"UnassignedRequestTableViewController::didReceiveParserWithError: %@", errorString);
    [[[UIAlertView alloc] initWithTitle:NSLocalizedString(@"Error", @"")
                                message:errorString
                               delegate:nil
                      cancelButtonTitle:NSLocalizedString(@"OK", @"")
                      otherButtonTitles:nil] show];
}

#pragma mark - RequestSignerEvent

- (void)didReceiveSignerRequestResult:(NSArray *)requestsSigned
{
    T21LogDebug(@"ModalSignerController::didReceiveSignerRequestResult");
    [SVProgressHUD dismiss];

    BOOL processedOK = TRUE;
    NSString *msg = @"";

    for (PFRequest *request in requestsSigned) {
        if ([[request status] isEqualToString:@"KO"]) {
            if (![msg isEqualToString:@""]) {
                msg = @"Ocurrio un error al firmar algunas de las peticiones seleccionadas.";
                break;
            } else {
                msg = @"Ocurrio un error al firmar la peticion seleccionada";
            }
            processedOK = FALSE;
        }
    }

    if (processedOK) {
        // @" Peticiones firmadas corrrectamente"
        [[[UIAlertView alloc] initWithTitle:NSLocalizedString(@"INFO", @"")
                                    message:@"Peticiones firmadas correctamente"
                                   delegate:self
                          cancelButtonTitle:NSLocalizedString(@"OK", @"")
                          otherButtonTitles:nil] show];
    } else {
        [self didReceiveError:msg];
    }

    [_documentActionSheet dismissWithClickedButtonIndex:-1 animated:YES];
}

#pragma mark - UIAlertViewDelegate

- (void)alertView:(UIAlertView *)alertView didDismissWithButtonIndex:(NSInteger)buttonIndex
{
    [(BaseListTVC *)self.navigationController.previousViewController refreshInfo];
    [self.navigationController popViewControllerAnimated:YES];
}

@end
