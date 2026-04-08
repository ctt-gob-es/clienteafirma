<#
  Script PowerShell que:
  - Anade una ruta al valor Path del registro (HKLM\SYSTEM\CurrentControlSet\Control\Session Manager\Environment) si se pasa como parametro
  - Evita duplicados (comprobando coincidencias delimitadas por `;`)
  - Usa REG_EXPAND_SZ para almacenar el valor
  - Devuelve 0 en exito, 1 en error
#>

param(
    [Parameter(Mandatory=$false, Position=0)]
    [string]$Target
)


Add-Type -AssemblyName System.Windows.Forms

#[System.Windows.Forms.MessageBox]::Show(
#    "Inicio del script",
#    "Autofirma",
#    [System.Windows.Forms.MessageBoxButtons]::OK,
#    [System.Windows.Forms.MessageBoxIcon]::Information
#)


# Si no se indica la ruta, salimos con codigo 0
if ([string]::IsNullOrWhiteSpace($Target)) {
    exit 0
}

#[System.Windows.Forms.MessageBox]::Show(
#    "1",
#    "Autofirma",
#    [System.Windows.Forms.MessageBoxButtons]::OK,
#    [System.Windows.Forms.MessageBoxIcon]::Information
#)

# Eliminamos la barra final si la tiene
if ($Target.EndsWith('\')) {
    $Target = $Target.TrimEnd('\')
}

#[System.Windows.Forms.MessageBox]::Show(
#    "2. Target: $Target",
#    "Autofirma",
#    [System.Windows.Forms.MessageBoxButtons]::OK,
#    [System.Windows.Forms.MessageBoxIcon]::Information
#)


$envKeyPath = 'SYSTEM\CurrentControlSet\Control\Session Manager\Environment'

# Leemos el PATH del registro
try {
    $readKey = [Microsoft.Win32.Registry]::LocalMachine.OpenSubKey($envKeyPath)
    if (-not $readKey) {
        exit 1
    }
    $syspath = $readKey.GetValue('Path', $null)
    $readKey.Close()
} catch {
    exit 1
}

#[System.Windows.Forms.MessageBox]::Show(
#    "3",
#    "Autofirma",
#    [System.Windows.Forms.MessageBoxButtons]::OK,
#    [System.Windows.Forms.MessageBoxIcon]::Information
#)


# Si no se ha podido leer el PATH, devolvemos error
if ([string]::IsNullOrEmpty($syspath)) {
    exit 1
}

#[System.Windows.Forms.MessageBox]::Show(
#    "4. PATH actual: $syspath",
#    "Autofirma",
#    [System.Windows.Forms.MessageBoxButtons]::OK,
#    [System.Windows.Forms.MessageBoxIcon]::Information
#)


# Comprobamos si ya existe en el PATH (anadimos ; al principio y al final para evitar coincidencias parciales)
$aug = ";$syspath;"
if ($aug.IndexOf(";$Target;", [System.StringComparison]::OrdinalIgnoreCase) -ge 0) {
    exit 0
}

#[System.Windows.Forms.MessageBox]::Show(
#    "5. No existe en el PATH, se anade",
#    "Autofirma",
#    [System.Windows.Forms.MessageBoxButtons]::OK,
#    [System.Windows.Forms.MessageBoxIcon]::Information
#)

# Construir nuevo PATH: asegurar punto y coma final
if (-not $syspath.EndsWith(';')) {
    $newpath = $syspath + ';'
} else {
    $newpath = $syspath
}
$newpath = $newpath + $Target


#[System.Windows.Forms.MessageBox]::Show(
#    "6. Nuevo PATH: $newpath",
#    "Autofirma",
#    [System.Windows.Forms.MessageBoxButtons]::OK,
#    [System.Windows.Forms.MessageBoxIcon]::Information
#)


# Guardamos el nuevo PATH en el registro como REG_EXPAND_SZ
try {
    $writeKey = [Microsoft.Win32.Registry]::LocalMachine.OpenSubKey($envKeyPath, $true)
    if (-not $writeKey) {
        exit 1
    }
    $writeKey.SetValue('Path', $newpath, [Microsoft.Win32.RegistryValueKind]::ExpandString)
    $writeKey.Close()
} catch {
    exit 1
}


#[System.Windows.Forms.MessageBox]::Show(
#    "7. PATH actualizado en el registro",
#    "Autofirma",
#    [System.Windows.Forms.MessageBoxButtons]::OK,
#    [System.Windows.Forms.MessageBoxIcon]::Information
#)

# Enviar broadcast WM_SETTINGCHANGE para notificar a otras aplicaciones del cambio de entorno
# Declaramos SendMessageTimeout via P/Invoke
$pinvoke = @"
using System;
using System.Runtime.InteropServices;

public static class NativeMethods {
    public const int HWND_BROADCAST = 0xffff;
    public const int WM_SETTINGCHANGE = 0x001A;
    public const int SMTO_ABORTIFHUNG = 0x0002;

    [DllImport("user32.dll", CharSet = CharSet.Auto, SetLastError = true)]
    public static extern IntPtr SendMessageTimeout(
        IntPtr hWnd,
        uint Msg,
        UIntPtr wParam,
        string lParam,
        uint fuFlags,
        uint uTimeout,
        out UIntPtr lpdwResult
    );
}
"@

Add-Type -TypeDefinition $pinvoke -ErrorAction Stop

try {
    $result = [UIntPtr]::Zero
    [void][NativeMethods]::SendMessageTimeout([intptr]::op_Explicit([NativeMethods]::HWND_BROADCAST), [NativeMethods]::WM_SETTINGCHANGE, [uintptr]::Zero, "Environment", [NativeMethods]::SMTO_ABORTIFHUNG, 5000, [ref] $result)
} catch {
    # No fallamos la operacion si el broadcast no funciona; simplemente continuamos pero devolvemos 0 (ya hemos escrito el registro)
}


#[System.Windows.Forms.MessageBox]::Show(
#    "8. Broadcast enviado",
#    "Autofirma",
#    [System.Windows.Forms.MessageBoxButtons]::OK,
#    [System.Windows.Forms.MessageBoxIcon]::Information
#)

exit 0