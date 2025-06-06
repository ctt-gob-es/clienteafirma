# Instrucciones de instalación de plugins y archivos de idioma para NSIS

Este paquete contiene un script por lotes (`.bat`) para automatizar la instalación de los plugins `nsProcess` y `registry`,
así como los archivos de idioma personalizados (`.nlf` y `.nsh`) necesarios para compilar los instaladores NSIS de Autofirma.

---

## Requisitos

- **Sistema operativo**: Windows 7 o superior
- **NSIS instalado en**: `C:\Program Files (x86)\NSIS`
- **Estructura de carpetas esperada**:

```
NSIS/
├── instalar-nsis-completo.bat
├── plugins/
│   ├── nsProcess.dll
│   ├── registry.dll
│   ├── nsProcess.nsh
│   └── Registry.nsh
├── Contrib/
│   └── Language Files/
│       ├── *.nlf
│       └── *.nsh
```

---

## Descarga de plugins

Antes de ejecutar el script, compruebe que los plugins se encuentra en la caprtea `plugins`. En caso contrario debes descargar los plugins desde las siguientes páginas:

- [`nsProcess`](https://nsis.sourceforge.io/NsProcess_plugin)
- [`registry`](https://nsis.sourceforge.io/Registry_plug-in)

Una vez descargados, copia los archivos correspondientes a la carpeta `plugins/` del proyecto:

- NSProcess
  - `nsProcess.dll`: El cual se encuentra dentro del archivo ZIP en la ruta `Plugin/`
  - `nsProcess.nsh`: El cual se encuentra dentro del archivo ZIP en la ruta `Include/`
- Registry
  - `registry.dll`: El cual se encuentra dentro del archivo ZIP en la ruta `Desktop/Plugin/`
  - `Registry.nsh`: El cual se encuentra dentro del archivo ZIP en la ruta `Desktop/Include/`

> **Nota**: Si decides ubicar estos archivos en otro directorio distinto, edita el script `instalar-nsis-completo.bat`  
> y reemplaza la línea que define `PLUGINS_SRC` con la ruta absoluta donde se encuentren.  
> Ejemplo:
>
> ```
> set PLUGINS_SRC=C:\Users\usuario\Descargas\nsis-plugins
> ```

---

## ¿Qué hace el script?

El script realiza las siguientes acciones:

1. Solicita permisos de administrador si no los tiene.
2. Copia los siguientes archivos desde la carpeta `./plugins/`:

   - `nsProcess.dll` → `Plugins\x86-unicode\`
   - `registry.dll` → `Plugins\x86-unicode\`
   - `nsProcess.nsh` → `Include\`
   - `Registry.nsh` → `Include\`

3. Copia archivos `.nlf` y `.nsh` desde `./Contrib/Language Files/`:
   - `.nlf` → `Contrib\Language files\`
   - `.nsh` → `Contrib\Language files\` y también `Include\`

---

## Instrucciones de uso

1. Haz doble clic sobre `instalar-nsis-completo.bat`.
2. Si no tiene permisos de administrador, solicitará confirmación.
3. Espera a que termine el proceso con el mensaje de instalación completada.

---

## Verificación

Puedes comprobar que los archivos fueron copiados correctamente revisando estas carpetas:

- `C:\Program Files (x86)\NSIS\Plugins\x86-unicode\`
- `C:\Program Files (x86)\NSIS\Include\`
- `C:\Program Files (x86)\NSIS\Contrib\Language files\`

---
