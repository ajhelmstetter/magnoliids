#!/usr/bin/env pwsh


$apikey = $Env:CKAN_API_KEY
if (!$apikey) {
  'Please set the CKAN_API_KEY environment variable.'
  ''
  'You can find your API Key by browsing to:'
  'https://data.bioplatforms.com/user/hervesauquet'
  ''
  'The API key has the format:'
  'xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx'
  ''
  'To set the environment variable in Linux/MacOS/Unix, use:'
  'export CKAN_API_KEY=xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx'
  ''
  'On Microsoft Windows, within Powershell, use:'
  '$env:CKAN_API_KEY=xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx'
  exit 1
}


#
# This PowerShell script was automatically generated.
#

function DownloadURL($url)
{
    $filename = $url.Substring($url.lastIndexOf('/') + 1)
    if (Test-Path $filename) {
        "File already exists, skipping download: " + $filename
        return
    }
    $client = new-object System.Net.WebClient
    if ($apikey) {
        $client.Headers.Add('Authorization: ' + $apikey)
    }
    "Downloading: " + $filename
    $client.DownloadFile($url, $filename)
}

function VerifyMD5([String]$filename, [String]$expected_md5)
{
    $md5hash = new-object -TypeName System.Security.Cryptography.MD5CryptoServiceProvider
    try {
        $actual_md5 = [System.BitConverter]::ToString($md5hash.ComputeHash([System.IO.File]::ReadAllBytes($filename))).Replace('-', '').toLower();
    } catch [System.IO.FileNotFoundException] {
        $filename + ": FAILED open or read"
        return
    }
    if ($actual_md5 -eq $expected_md5) {
        $filename + ": OK"
    } else {
        $filename + ": FAILED"
    }
}


'Commencing bulk download of data from CKAN:'
''

$urls = Get-Content 'tmp/bpa-plants_Eupomatiaceae_20210316T0452_urls.txt'
ForEach ($line in $urls) {
    DownloadURL $line
}

'File downloads complete.'
''
'Verifying file checksums:'
''
$md5s = Get-Content 'tmp/bpa-plants_Eupomatiaceae_20210316T0452_md5sum.txt'
ForEach ($line in $md5s) {
    $md5, $filename = $line.Split(" ",[StringSplitOptions]'RemoveEmptyEntries')
    VerifyMD5 $filename $md5
}
