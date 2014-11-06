//
//  AOHelpMenuViewController.m
//  SignSample02
//
//

#import "AOHelpMenuViewController.h"
#import <QuartzCore/QuartzCore.h>

@interface AOHelpMenuViewController ()

@end

@implementation AOHelpMenuViewController

@synthesize tblViewHelp;
NSMutableArray *tableData = NULL;

- (id)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil
{
    self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil];
    if (self) {
        // Custom initialization
    }
    return self;
}

- (void)viewDidLoad
{
    [super viewDidLoad];
	// Do any additional setup after loading the view.
    
    [[self navigationController] setNavigationBarHidden:YES animated:YES];
    
    [self populateTable];
    //definimos los bordes de la tabla.
    self.tblViewHelp.layer.borderWidth = 0.5;
    self.tblViewHelp.layer.borderColor = [[UIColor grayColor] CGColor];
    self.tblViewHelp.layer.cornerRadius = 6.0f;
    self.tblViewHelp.scrollEnabled=NO;
    
    self.screenName = @"IOS AOHelpMenuViewController - Help menu";

}
- (IBAction)goBackHome:(id)sender {
    [self.navigationController popToRootViewControllerAnimated:YES];
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}


//Carga en la lista de almacenes los almacenes encontrados en Itunes.
-(void)populateTable {
    
    tableData = [[NSMutableArray alloc] init];
    
    [tableData addObject: NSLocalizedString(@"help_acercade",nil)];
    [tableData addObject: NSLocalizedString(@"help_instalar_certificados",nil)];
    [tableData addObject: NSLocalizedString(@"help_preguntas",nil)];

}

/******************************************************************/
/******** METODOS IMPLEMENTADOS DE LA TABLA DE CERTIFICADOS *******/
/******************************************************************/

#pragma mark -
#pragma mark Table view data source
// Detalla el numbre de secciones en la tabla.
- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView {
    return 1;
}

// Detalla el número de filas en la tabla.
- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section {
    return [tableData count];
}

// Detalla la apariencia de las celdas.
- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath {
    
    static NSString *CellIdentifier = @"Cell";
    
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:CellIdentifier];
    if (cell == nil) {
        cell = [[[UITableViewCell alloc] initWithStyle:UITableViewCellStyleDefault reuseIdentifier:CellIdentifier] autorelease];
    }
    
    //Cell View
    UIView *cellView = [[UIView alloc]initWithFrame:CGRectMake(0, 0, 320, 65)];
       
    //Label
    UILabel *lblFor = [[UILabel alloc]initWithFrame:CGRectMake(10, 15, 200, 21)];
    lblFor.text = [tableData objectAtIndex:indexPath.row];
    lblFor.backgroundColor = [UIColor clearColor];
    lblFor.font = [UIFont fontWithName:@"ArialMT" size:14];
    lblFor.tag =1;
    
    //Adding Views to Cell View
    [cellView addSubview:lblFor];
    
    for(UIView *view in cell.contentView.subviews){
        if ([view isKindOfClass:[UIView class]]) {
            [view removeFromSuperview];
        }
    }
    
    [cell.contentView addSubview:cellView];
    cell.accessoryType = UITableViewCellAccessoryDisclosureIndicator;
    
    return cell;
}

//Nos devuelve la fila seleccionada.
-(void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath
{
    int fila = (int)indexPath.row;
    @try {
        if(fila==0)
            [self performSegueWithIdentifier:@"toAcerca" sender:self];
        else if(fila==1)
            [self performSegueWithIdentifier:@"toInstalar" sender:self];
        else if(fila==2)
            [self performSegueWithIdentifier:@"toPregFrecuentes" sender:self];
    }
    @catch (NSException *e) {
        // Se ignora
    }
}


@end
