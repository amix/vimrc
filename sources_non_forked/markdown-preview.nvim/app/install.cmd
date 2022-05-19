@PowerShell -ExecutionPolicy Bypass -Command Invoke-Expression $('$args=@(^&{$args} %*);'+[String]::Join(';',(Get-Content '%~f0') -notmatch '^^@PowerShell.*EOF$')) & goto :EOF

[Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12

$repo = "iamcco/markdown-preview.nvim"
$file = "markdown-preview-win.zip"

$releases = "https://api.github.com/repos/$repo/releases"

Write-Host Determining latest release
if ($args[0]) { $tag = $args[0] } else { $tag = (Invoke-WebRequest $releases | ConvertFrom-Json)[0].tag_name }

$download = "https://github.com/$repo/releases/download/$tag/$file"
$name = $file.Split(".")[0]
$zip = "$name-$tag.zip"
$dir = "bin"

new-item -Name $dir -ItemType directory -Force

Write-Host Dowloading latest release
Invoke-WebRequest $download -Out $zip

Remove-Item $dir\* -Recurse -Force -ErrorAction SilentlyContinue

Write-Host Extracting release files
Expand-Archive $zip -DestinationPath $dir -Force

Remove-Item $zip -Force
Write-Host markdown-preview install completed.
