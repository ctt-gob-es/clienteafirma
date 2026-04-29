package es.gob.afirma.standalone.plugins.manager;

import es.gob.afirma.core.ErrorCode;

public class PluginManagerError {

	public static class Internal {

		public static final ErrorCode PLUGIN_ERROR						= new ErrorCode("230200", "Error desconocido con el plugin"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode UNINSTALL_PLUGIN_ERROR   			= new ErrorCode("230201", "Ocurrio un error al desinstalar el plugin"); //$NON-NLS-1$ //$NON-NLS-2$
        public static final ErrorCode ERROR_IN_PLUGIN   				= new ErrorCode("230202", "El propio plugin devolvio un error durante su instalacion"); //$NON-NLS-1$ //$NON-NLS-2$
        public static final ErrorCode CANT_REMOVE_LOADED_PLUGIN   		= new ErrorCode("230203", "Ha ocurrido un error al eliminar el plugin anterior"); //$NON-NLS-1$ //$NON-NLS-2$
        public static final ErrorCode PLUGIN_DIRECTORY_ERROR   			= new ErrorCode("230204", "El plugin seleccionado ya se encuentra instalado"); //$NON-NLS-1$ //$NON-NLS-2$
        public static final ErrorCode PLUGIN_LOAD_ERROR   				= new ErrorCode("230205", "Ha ocurrido un error al intentar cargar el plugin"); //$NON-NLS-1$ //$NON-NLS-2$
        public static final ErrorCode INSTALLED_PLUGIN_NOT_FOUND		= new ErrorCode("230206", "No se encontraron los ficheros de un plugin previamente instalado"); //$NON-NLS-1$ //$NON-NLS-2$
	}

	public static class Functional {

		public static final ErrorCode MALFORMED_PLUGIN_SERVICE    		= new ErrorCode("530001", "Se han encontrado plugins mal definidos en el fichero importado"); //$NON-NLS-1$ //$NON-NLS-2$
        public static final ErrorCode NO_PLUGIN_FOUND             		= new ErrorCode("530002", "No se encontro ningun plugin en los archivos"); //$NON-NLS-1$ //$NON-NLS-2$
        public static final ErrorCode MULTIPLE_PLUGINS_FOUND      		= new ErrorCode("530003", "No se permite la carga simultanea de varios plugins"); //$NON-NLS-1$ //$NON-NLS-2$
        public static final ErrorCode BUTTON_NO_ACTION            		= new ErrorCode("530004", "El plugin no ha definido accion para un boton"); //$NON-NLS-1$ //$NON-NLS-2$
        public static final ErrorCode BUTTON_NO_WINDOW            		= new ErrorCode("530005", "El plugin no ha definido la ventana en la que debe aparecer un boton"); //$NON-NLS-1$ //$NON-NLS-2$
        public static final ErrorCode INVALID_ACTION_CLASS        		= new ErrorCode("530006", "El plugin definio una clase de accion erronea"); //$NON-NLS-1$ //$NON-NLS-2$
        public static final ErrorCode ALREADY_INSTALLED_PLUGIN   		= new ErrorCode("530008", "El plugin ya se encuentra instalado"); //$NON-NLS-1$ //$NON-NLS-2$
	}
}
