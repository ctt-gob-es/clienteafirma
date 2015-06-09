/**
 * 
 */
DNInBUtils = (function(){
	
	//TODO terminar remplazo de jquery $j, $
	
	var jQueryAux;
	var dialogContentSelectMethod = "iframeSelectMethod.html";
	var dialogContentCertificates = 'iframeCertificates.html';
	
	var dialogContainer = '<div class="DNInBCSS dialogDNInBContainer"></div>';
	
	var jQueryLib = "jquery-ui-1.11.4.custom/external/jquery/jquery.js";
	var jQueryUILib = "jquery-ui-1.11.4.custom/jquery-ui.min.js";
	var dialogCSS = "jquery-ui-1.11.4.custom/jquery-ui.css";
	var complementaryCSS = "DNInBCSS.css";
	
	var maxWidth = "600";
	var maxHeight = "400";
	
	function getDialog(url){
		// Se pone un width de 98% para evitar que salga la barra de scroll horizontal.
		return '<div id="dialogDNInB" title="Di&aacute;logo de Seguridad" style="overflow:hidden;padding:0 !important;width:100% !important;">'+
		 	   '<iframe name="dialogDNInBIframe" id="dialogDNInBIframe" width="98%" height="100%" marginWidth="0" marginHeight="0" frameBorder="0" '+
		 	   'src="'+url+'"></iframe></div>';
	}
	/** Carga una librer&iacute;a al vuelo y ejecuta una funci&oacute;n al finalizar. 
	 * Con ella podremos usar jQuery sin afectar a la web. */
	function loadScript(url, callback, errorCallback){
	    // Anade el script a la cabecera.
	    var head = document.getElementsByTagName('head')[0];
	    var script = document.createElement('script');
	    script.type = 'text/javascript';
	    script.src = url;
	    
	    // Handle Script loading
	    var done = false;

		// Attach handlers for all browsers
		script.onload = script.onreadystatechange = function() {
		    if ( !done && (!this.readyState ||
		            this.readyState === "loaded" || this.readyState === "complete") ) {
		        done = true;

		        callback();
		
		        // Handle memory leak in IE
		        script.onload = script.onreadystatechange = null;
		        if ( head && script.parentNode ) {
		            head.removeChild( script );
		        }
		    }
		};
		
		script.onerror = errorCallback;
		
		// Use insertBefore instead of appendChild  to circumvent an IE6 bug.
		// This arises when a base node is used (#2709 and #4378).
		head.insertBefore( script, head.firstChild );	    
	}
	
	/** Agregamos jQuery-ui tambi&eacute;n para operar con las modales. */
	function jQueryLoadCallback() {
		if( typeof $.ui != 'undefined' && $.ui.version){
			$j = $;
		}else{
			loadScript(jQueryUILib, jQueryUILoadCallback, jQueryUILoadErrorCallback);
		}					
	}
	/** Ocultamos jQuery para evitar conflictos con el funcionamiento de la web. */
	function jQueryUILoadCallback() {
		$j = jQuery.noConflict(true);
		if(jQueryAux){
			$ = jQueryAux;
		}
		$j('head').append('<link rel="stylesheet" href="'+dialogCSS+'" type="text/css" />');
		$j('head').append('<link rel="stylesheet" href="'+complementaryCSS+'" type="text/css" />');
		
	}
	/** funci&oacute;n que controla el error al incluir la librer&iacute;a. */
	function jQueryLoadErrorCallback() {
		console.warn("Hubo un problema al cargar la libreria jQuery.");
		alert("Hubo un problema al empezar el proceso, intentelo de nuevo.");
	}
	/** funci&oacute;n que controla el error al incluir la librer&iacute;a. */
	function jQueryUILoadErrorCallback() {
		console.warn("Hubo un problema al cargar la libreria jQuery-ui.");
		alert("Hubo un problema al empezar el proceso, intentelo de nuevo.");
	}
	/** Inicializaci&oacute;n del elemento. */
	function init(){	
		if(typeof $ != 'undefined'){
			jQueryAux = $;
		}
		if(typeof $ != 'undefined' && versionCompare($().jquery,"1.6")>=0){			
			jQueryLoadCallback();
		}else{
			loadScript(jQueryLib, jQueryLoadCallback, jQueryLoadErrorCallback);
		}
	}
	
	/** Comprobamos el DNI y lanzamos la Modal. */
	function launcher(){
		var dni = getDNI();
		if(verificateDNI(dni)){
			launchDialogSelectMethod();
			
		}
	}
	
	/** Comprobamos si ese DNI existe en DNInB.
	 * 
	 * @param dni DNI del usuario.
	 * @returns true si existe, false si no existe.
	 */
	function verificateDNI(dni){
		//TODO
		return true;
	}
	
	/** Obtenci&oacute;n del DNI del usuario.
	 * 
	 * @returns DNI del usuario.
	 */
	function getDNI(){
		return "31733888Y";
	}
	
	/** Creaci&oacute;n y lanzamiento de la modal. */
	function launchDialog(){
		
		if(!$j(".dialogDNInBContainer").length){
			$j("body").append(dialogContainer);
		}
		if(!$j("#dialogDNInB").length){
			$j("body").append(getDialog(dialogContentCertificates));
		}else{
			$j("#dialogDNInBIframe")[0].src = dialogContentCertificates;
		}
		
		
		
		var d = $j("#dialogDNInB").dialog({
		    autoOpen: false,
		    width: "90%",
		    height:"90%",
		    maxWidth: maxWidth+"px",
            modal: true,
            fluid: true, //new option
            resizable: false, 
            draggable: false,
            open: function(event, ui){ 
                	fluidDialog();
                },
            create: function(event, ui){ 
                	fluidDialog();
                },
            buttons: {
               "Firmar": function() {
            	   if(firmar()){
            		   $j(this).dialog("close");   
            	   }            	   
               },
               "Cancelar": function() {
            	   $j(this).dialog("close");
               }
            }
		});
		
		d.dialog("close");
		
		d.parent('.ui-dialog').appendTo('.dialogDNInBContainer');
		d.dialog('open');
	}
	function launchDialogSelectMethod(){
		if(!$j(".dialogDNInBContainer").length){
			$j("body").append(dialogContainer);
		}
		
		if(!$j("#dialogDNInB").length){
			$j("body").append(getDialog(dialogContentSelectMethod));
		}else{
			$j("#dialogDNInBIframe")[0].src = dialogContentSelectMethod;
		}
		
		
		var d = $j("#dialogDNInB").dialog({
		    autoOpen: false,
            modal: true,
            resizable: false, 
            draggable: false,
            width : "auto",
            height : "auto",
            open: function(event, ui){ 
            	var dialog = $j(this).find(".ui-dialog-content");
            	$j(this).css("max-height", "auto");
            	$j(this).css("height", "auto");      
            	dialog.css("height", "auto");	            
            	dialog.dialog("option","position", dialog.dialog("option","position"));
            },
            buttons : ""
		});
		
		d.dialog("close");
		
		d.parent('.ui-dialog').appendTo('.dialogDNInBContainer');
		d.dialog('open');
	}
	
	function versionCompare(left, right) {
	    if (typeof left + typeof right != 'stringstring')
	        return false;
	    
	    var a = left.split('.')
	    ,   b = right.split('.')
	    ,   i = 0, len = Math.max(a.length, b.length);
	        
	    for (; i < len; i++) {
	        if ((a[i] && !b[i] && parseInt(a[i]) > 0) || (parseInt(a[i]) > parseInt(b[i]))) {
	            return 1;
	        } else if ((b[i] && !a[i] && parseInt(b[i]) > 0) || (parseInt(a[i]) < parseInt(b[i]))) {
	            return -1;
	        }
	    }
	    
	    return 0;
	}
	function fluidDialog() {

	    var $visible = $j(".ui-dialog:visible");
	    // each open dialog
	    $visible.each(function () {
	        var $this = $j(this);
	        var dialog = $this.find(".ui-dialog-content");
	        
	        // if fluid option == true
	        if (dialog.dialog("option","maxWidth") && dialog.dialog("option","width")) {
	            // fix maxWidth bug
	            $this.css("max-width", dialog.dialog("option","maxWidth"));
	        }
	        
	        adjustHeight($this, dialog);	        
            
	        if (dialog.dialog("option","fluid")) {
	            // namespace window resize
	            $j(window).on("resize.responsive", function () {
	            	adjustHeight($this, dialog);           
	            });
	        }
	    });
	}
	function adjustHeight(visibleDialog, dialogContent){
    	var hHeight = 0.9*$j(window).height();
        
        hHeight = hHeight > maxHeight ? maxHeight : hHeight;
        
        visibleDialog.css("max-height", hHeight);
        visibleDialog.css("height", hHeight);            
        
        var parentHeight = dialogContent.parent().height();
        
        var contentHeight = parentHeight;
        dialogContent.siblings().each(function(){	                
            var height = $j(this).outerHeight();
            contentHeight = contentHeight - height;
        });	            
        dialogContent.css("height", contentHeight);	            
        dialogContent.dialog("option","position", dialogContent.dialog("option","position"));
    }
	
	function mostrarAyuda(){
		parent.window.open("http://incidencias-ctt.administracionelectronica.gob.es/wiki/doku.php?id=forja-ctt_wiki:clienteafirma:start");
	}	
	
	return{
		init : init,
		launcher : launcher,
		launchDialog : launchDialog,
		mostrarAyuda : mostrarAyuda 
	}
})();