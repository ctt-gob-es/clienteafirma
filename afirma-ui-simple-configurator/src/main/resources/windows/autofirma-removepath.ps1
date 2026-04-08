<#
  Script PowerShell que:
  - Si no se pasa parametro, sale con codigo 0
  - Elimina una ruta del valor Path en HKLM\SYSTEM\CurrentControlSet\Control\Session Manager\Environment
  - Mantiene el resto de rutas en el mismo orden, sin duplicados anadidos ni `;;`
  - Escribe como REG_EXPAND_SZ
  - Emite broadcast WM_SETTINGCHANGE para notificar a otros procesos
  - Devuelve 0 en éxito, 1 en error
#>

param(
    [Parameter(Mandatory=$false, Position=0)]
    [string]$Target
)

# Si no se indica la ruta, salimos con codigo 0
if ([string]::IsNullOrWhiteSpace($Target)) {
    exit 0
}

# Eliminamos la barra final si la tiene
if ($Target.EndsWith('\')) {
    $Target = $Target.TrimEnd('\')
}

$envKeyPath = 'SYSTEM\CurrentControlSet\Control\Session Manager\Environment'

# Leemos el PATH del registro
try {
    $readKey = [Microsoft.Win32.Registry]::LocalMachine.OpenSubKey($envKeyPath)
    if (-not $readKey) { exit 1 }
    $syspath = $readKey.GetValue('Path', $null)
    $readKey.Close()
} catch {
    exit 1
}

if (-not $syspath) {
    exit 1
}

# Asegurar punto y coma final para facilitar el parseo
if (-not $syspath.EndsWith(';')) {
    $syspath = $syspath + ';'
}

$rest = $syspath
$newpathParts = @()

while ($rest -ne '') {
    # Extraer el siguiente elemento hasta el primer ';'
    $sepIndex = $rest.IndexOf(';')
    if ($sepIndex -lt 0) { break }
    $item = $rest.Substring(0, $sepIndex)
    # Quitar el item y el ';' de rest
    $rest = $rest.Substring([math]::Min($sepIndex + 1, $rest.Length))

    if ($item -eq '') { continue }

    # Eliminar barra final si la tiene
    if ($item.EndsWith('\')) { $item = $item.TrimEnd('\') }

    # Comparacion case-insensitive; si no es la ruta objetivo, la añadimos
    if (-not ([string]::Equals($item, $Target, [System.StringComparison]::OrdinalIgnoreCase))) {
        $newpathParts += $item
    }
}

# Reconstruir PATH sin duplicados introducidos y preservando orden
$newpath = [string]::Join(';', $newpathParts)

# Escribir como REG_EXPAND_SZ
try {
    $writeKey = [Microsoft.Win32.Registry]::LocalMachine.OpenSubKey($envKeyPath, $true)
    if (-not $writeKey) { exit 1 }
    $writeKey.SetValue('Path', $newpath, [Microsoft.Win32.RegistryValueKind]::ExpandString)
    $writeKey.Close()
} catch {
    exit 1
}

# Enviar broadcast WM_SETTINGCHANGE para notificar a otras aplicaciones del cambio de entorno
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
    # No fallamos si el broadcast no funciona
}

exit 0
